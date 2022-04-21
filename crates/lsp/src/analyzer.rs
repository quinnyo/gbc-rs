// STD Dependencies -----------------------------------------------------------
use std::thread;
use std::sync::Arc;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use std::sync::atomic::AtomicUsize;


// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use tower_lsp::Client;
use tower_lsp::lsp_types::{DocumentSymbol, Range};
use tower_lsp::lsp_types::{SymbolInformation, SymbolKind, Url, Location, CompletionItem, CompletionItemKind};
use gbd::{EmulatorAddress, EmulatorCommand, Model};


// Internal Dependencies ------------------------------------------------------
use compiler::lexer::LexerToken;
use compiler::linker::{AnalysisSymbol, AnalysisMacroExpansion, AnalysisHint};

use crate::{
    state::State,
    emulator::Emulator,
    parser::Parser,
    types::{InlayHint, InlayKind}
};


// Constants ------------------------------------------------------------------
const THREAD_DEBOUNCE: u64 = 150;


// Analyzer Implementation ----------------------------------------------------
pub struct Analyzer {
    state: State,
    link_gen: Arc<AtomicUsize>
}

impl Analyzer {
    pub fn new(client: Arc<Client>) -> Self {
        let state = State::from_client(client);
        Self {
            state,
            link_gen: Arc::new(AtomicUsize::new(0))
        }
    }

    pub async fn initialize(&self) {
        self.state.update_server_status("Ready").await;
        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
    }

    pub async fn set_workspace_path(&self, path: PathBuf) {
        self.state.set_workspace_path(path);
    }

    // TODO handle deletion and renaming of files
    pub fn open_document(&self, path: &str, text: &str) {
        self.state.documents().insert(path.to_string(), text.to_string());
        self.link_async(PathBuf::from(path));
    }

    pub fn change_document(&self, path: &str, text: &str) {
        self.state.tokens().remove(&PathBuf::from(path));
        self.state.documents().insert(path.to_string(), text.to_string());
        self.link_async(PathBuf::from(path));
    }

    pub fn save_document(&self, path: &str) {
        self.state.tokens().remove(&PathBuf::from(path));
        self.state.documents().remove(path);
        self.link_async(PathBuf::from(path));
    }

    pub fn close_document(&self, path: &str) {
        self.state.tokens().remove(&PathBuf::from(path));
        self.state.documents().remove(path);
        self.link_async(PathBuf::from(path));
    }
}

impl Analyzer {
    pub async fn build_rom(&self) {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            if let Some((entries, _, _, rom_path)) = Parser::build(workspace_path, self.state.clone()).await {
                log::info!("Build ROM \"{}\" ({} entries)", rom_path.display(), entries.len());
            }
        }
    }
}

impl Analyzer {
    pub async fn emulator_start(&self, model: Option<Model>) {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            if let Some((entries, labels, rom, _)) = Parser::build(workspace_path, self.state.clone()).await {
                // Reset and reload existing emulator process if still running
                let was_reset = if let Some(process) = self.state.emulator().as_mut() {
                    process.reset(entries.clone(), labels.clone(), rom.clone(), model)

                } else {
                    false
                };
                if !was_reset {
                    let state = self.state.clone();
                    Handle::current().spawn(async move {
                        state.set_emulator(Some(Emulator::launch(state.clone(), entries, labels, rom, model)));
                    });
                }
            }
        }
    }

    pub async fn emulator_stop(&self) {
        if let Some(process) = self.state.emulator().as_mut() {
            process.stop();
        }
        self.state.set_emulator(None);
    }

    pub async fn toggle_breakpoint(&self, location: Location) {
        let mut addr = None;
        for (address, loc) in self.state.address_locations().iter() {
            if location.uri == loc.uri && location.range.start.line == loc.range.start.line {
                addr = Some(*address);
                break;
            }
        }
        if let Some(addr) = addr {
            self.state.send_command(EmulatorCommand::ToggleBreakpoint(EmulatorAddress::from_raw(addr)));
        }
    }

}

impl Analyzer {
    pub async fn workspace_symbols(&self) -> Vec<SymbolInformation> {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            self.get_symbols(workspace_path).await.into_iter().filter(|symbol| {
                symbol.kind != SymbolKind::METHOD

            }).map(AnalysisSymbol::into_symbol_information).collect()

        } else {
           Vec::new()
        }
    }

    pub async fn document_symbols(&self, current_file: PathBuf) -> Vec<DocumentSymbol> {
        let workspace_path = current_file.clone();
        let uri = Url::from_file_path(current_file).unwrap();
        self.get_symbols(workspace_path).await.into_iter().filter(|symbol| {
            // Return if we either want all symbols or the symbol originates from the current file
            symbol.location.uri == uri && symbol.kind != SymbolKind::METHOD

        }).map(AnalysisSymbol::into_document_symbol).collect()
    }

    pub async fn completions(&self, current_file: PathBuf, _line: usize, _col: usize) -> Vec<CompletionItem> {
        let uri = Url::from_file_path(current_file.clone()).ok();
        return self.get_symbols(current_file).await.into_iter().filter(|symbol| {
            // Complete if the symbol is either global or originates from the current file
            symbol.is_global || Some(&symbol.location.uri) == uri.as_ref()

        }).flat_map(|symbol| match symbol.kind {
            SymbolKind::CONSTANT => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some(format!("= {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::CONSTRUCTOR => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::CONSTRUCTOR),
                detail: Some(format!("MACRO{}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::FUNCTION => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(format!("@ {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::VARIABLE => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(format!("@ {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::FIELD => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!("@ {}", symbol.value)),
                .. CompletionItem::default()
            }),
            _ => None

        }).collect();
    }

    pub async fn hover(&self, current_file: PathBuf, line: usize, col: usize) -> Option<(String, Location)> {
        if let Some(symbol) = self.symbol(current_file, line, col).await {
            let value = self.query_symbol_memory_values(&[&symbol])[0];
            let info = symbol.info();
            let location = symbol.location;
            let is_global = symbol.is_global;
            let m = match symbol.kind {
                SymbolKind::NAMESPACE => {
                    Some(format!("SECTION \"{}\",{}", symbol.name, symbol.value))
                },
                SymbolKind::CONSTANT => {
                    Some(format!("CONST {}", symbol.name))
                },
                // TODO show expanded tokens, need to record expanded entries (already needed above)
                // and then be able to to_string() them all again into source code
                SymbolKind::CONSTRUCTOR => {
                    Some(format!("MACRO {}{}", symbol.name, symbol.value))
                },
                SymbolKind::FUNCTION => {
                    Some(format!("{}: ; ({})", symbol.name, info))
                },
                SymbolKind::VARIABLE => {
                    Some(format!("{}: {}; ({}){}", symbol.name, if symbol.width == 1 {
                        "DB"

                    } else if symbol.width == 2 {
                        "DW"

                    } else {
                        "DS"

                    }, info, if let Some(value) = value {
                        if symbol.width == 1 {
                            format!("\n\nEMULATED: ${:0>2X} / {}", value, value)

                        } else {
                            format!("\n\nEMULATED: ${:0>4X} / {}", value, value)
                        }

                    } else {
                        "".to_string()
                    }))
                },
                SymbolKind::FIELD => {
                    Some(format!("{}:", symbol.name))
                },
                _ => None
            };
            m.map(|s| (if is_global {
                format!("GLOBAL {}", s)

            } else {
                s
            }, location))

        } else {
            None
        }
    }

    pub async fn symbol(&self, current_file: PathBuf, line: usize, col: usize) -> Option<AnalysisSymbol> {
        let uri = Url::from_file_path(&current_file).unwrap();
        if let Some((token, _)) = Parser::get_token(&self.state, current_file.clone(), line, col) {
            let symbol_name = token.inner().value.to_string();
            let symbols = self.get_symbols(current_file).await;
            symbols.into_iter().find(|symbol| {
                // For child labels only search in the current file
                symbol.name == symbol_name &&
                    (symbol.kind != SymbolKind::METHOD || symbol.location.uri == uri)
            })

        } else {
            None
        }
    }

    pub async fn inlay_hints(&self, current_file: PathBuf) -> Vec<InlayHint> {
        // Get the line of the first error in the current file (if any)
        let first_error_line = if let Some((p, line, _)) = self.state.error().clone() {
            if PathBuf::from(p.path()) == current_file {
                line

            } else {
                10_000_000
            }

        } else {
            10_000_000
        };

        let uri = Url::from_file_path(current_file.clone()).ok();
        let (symbols, expansions, hints) = self.get_symbols_all(current_file).await;
        let symbols: Vec<AnalysisSymbol> = symbols.into_iter().filter(|symbol| {
            // Hint if the symbol is from the current file
            Some(&symbol.location.uri) == uri.as_ref()

        }).filter(|symbol| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            symbol.location.range.start.line < first_error_line as u32

        }).collect();

        let symbol_refs: Vec<&AnalysisSymbol> = symbols.iter().collect();
        let emulator_values = self.query_symbol_memory_values(&symbol_refs[..]);

        let mut inlay_hints: Vec<InlayHint> = symbols.into_iter().enumerate().flat_map(|(i, symbol)| match symbol.kind {
            SymbolKind::NAMESPACE => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: symbol.value.to_string(),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::CONSTANT => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::CONSTRUCTOR => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("MACRO{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::FUNCTION => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::VARIABLE => {
                let emulator_value = if let Some(value) = emulator_values[i] {
                    if symbol.width == 1 {
                        format!("  ${:0>2X} | {: <5} @ ", value, value)

                    } else {
                        format!("${:0>4X} | {: <5} @ ", value, value)
                    }

                } else {
                    "".to_string()
                };
                Some(InlayHint {
                    kind: InlayKind::TypeHint,
                    label: format!("{}{} ({})", emulator_value, symbol.value, symbol.info()),
                    range: Range {
                        start: symbol.location.range.start,
                        end: symbol.location.range.start
                    }
                })
            },
            SymbolKind::FIELD => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            _ => None

        }).collect();

        // Macro expansions
        inlay_hints.append(&mut expansions.into_iter().filter(|exp| {
            // Hint if the expression is from the current file
            Some(&exp.location.uri) == uri.as_ref()

        }).filter(|exp| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            exp.location.range.start.line < first_error_line as u32

        }).map(|exp| {
            InlayHint {
                kind: InlayKind::OptimizerHint,
                label: if exp.children == 0 {
                    format!("{} byte(s) (expanded)", exp.size)

                } else if exp.children == 1 {
                    format!("{} byte(s) ({} sub-expansion)", exp.size, exp.children)

                } else {
                    format!("{} byte(s) ({} sub-expansions)", exp.size, exp.children)
                },
                range: Range {
                    start: exp.location.range.start,
                    end: exp.location.range.start
                }
            }
        }).collect());

        // Optimizer results
        inlay_hints.append(&mut hints.into_iter().filter(|hint| {
            // Hint if the symbol is from the current file
            Some(&hint.location.uri) == uri.as_ref()

        }).filter(|hint| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            hint.location.range.start.line < first_error_line as u32

        }).map(|hint| {
            InlayHint {
                kind: InlayKind::OptimizerHint,
                label: format!("{} (optimized)", hint.detail),
                range: Range {
                    start: hint.location.range.start,
                    end: hint.location.range.start
                }
            }
        }).collect());
        inlay_hints
    }
}

impl Analyzer {
    async fn get_symbols_all(&self, workspace_path: PathBuf) -> (Vec<AnalysisSymbol>, Vec<AnalysisMacroExpansion>, Vec<AnalysisHint>) {
        if !self.state.has_symbols() {
            Parser::link(workspace_path, self.state.clone()).await;
            self.state.publish_diagnostics().await;
        }
        self.state.symbols_all().clone().unwrap_or_else(|| (Vec::new(), Vec::new(), Vec::new()))
    }

    async fn get_symbols(&self, workspace_path: PathBuf) -> Vec<AnalysisSymbol> {
        if !self.state.has_symbols() {
            Parser::link(workspace_path, self.state.clone()).await;
            self.state.publish_diagnostics().await;
        }
        self.state.symbols_cloned().unwrap_or_default()
    }

    fn query_symbol_memory_values(&self, symbols: &[&AnalysisSymbol]) -> Vec<Option<u16>> {

        // Generate initial result set
        let mut result_set = Vec::with_capacity(512);
        for _ in 0..symbols.len() {
            result_set.push(None)
        }

        // Do nothing if no emulator is present
        if self.state.status().is_none() {
            return result_set;
        }

        #[derive(Debug)]
        enum PendingResult {
            Done,
            Invalid,
            Byte(u16),
            Word(u16, u16)
        }

        // Query emulator
        let mut pending = Vec::with_capacity(symbols.len());
        let mut reads = Vec::new();
        for symbol in symbols {
            if let Some(address) = symbol.address {
                if address <= 0xFFFF && (symbol.width == 1 || symbol.width == 2) {
                    let address = address as u16;
                    if symbol.width == 1 {
                        pending.push(PendingResult::Byte(address));
                        reads.push(EmulatorAddress::new(address, 0));

                    } else {
                        pending.push(PendingResult::Word(
                            address,
                            address.saturating_add(1)
                        ));
                        reads.push(EmulatorAddress::new(address, 0));
                        reads.push(EmulatorAddress::new(address.saturating_add(1), 0));
                    }

                } else {
                    pending.push(PendingResult::Invalid);
                }
            } else {
                pending.push(PendingResult::Invalid);
            }
            result_set.push(None);
        }
        self.state.send_command(EmulatorCommand::ReadMemory(reads));

        // Wait for results from emulator
        let started = Instant::now();
        while started.elapsed() < Duration::from_millis(300) {
            let mut results = self.state.results();
            let mut waiting = 0;
            for (i, pending) in pending.iter_mut().enumerate() {
                match pending {
                    PendingResult::Byte(address) => {
                        if let Some(value) = results.remove(address) {
                            result_set[i] = Some(value as u16);
                            *pending = PendingResult::Done;
                        }
                        waiting += 1;
                    },
                    PendingResult::Word(address_low, address_high) => {
                        if results.contains_key(address_low) && results.contains_key(address_high) {
                            result_set[i] = Some(results[address_low] as u16 | (results[address_high] as u16) << 8);
                            *pending = PendingResult::Done;
                        }
                        waiting += 1;
                    },
                    _ => {}
                }
            }
            if waiting == 0 {
                break;
            }
            thread::sleep(Duration::from_millis(10));
        }
        result_set
    }

    fn link_async(&self, workspace_path: PathBuf) where Self: 'static {
        // Bump symbol generation
        let link_gen = self.link_gen.clone();
        link_gen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        let thread_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
        let handle = Handle::current();
        let state = self.state.clone();
        handle.spawn(async move {
            // Wait for debounce purposes
            tokio::time::sleep(tokio::time::Duration::from_millis(THREAD_DEBOUNCE)).await;

            // Check if still the latest link request
            let latest_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
            if thread_gen == latest_gen {
                Parser::link(
                    workspace_path,
                    state.clone()
                ).await;
                state.publish_diagnostics().await;
            }
        });
    }
}

