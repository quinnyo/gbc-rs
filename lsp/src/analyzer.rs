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


// Internal Dependencies ------------------------------------------------------
use project::{ProjectConfig, ProjectReader};
use compiler::lexer::LexerToken;
use compiler::expression::ExpressionResult;

use crate::{
    state::State,
    emulator::{EmulatorConnection, EmulatorCommand, EmulatorProcess},
    parser::Parser,
    types::{
        InlayHint, InlayKind,
        GBCSymbol, MacroExpansion, Optimizations
    }
};


// Constants ------------------------------------------------------------------
const THREAD_DEBOUNCE: u64 = 150;


// Analyzer Implementation ----------------------------------------------------
pub struct Analyzer {
    state: State,
    link_gen: Arc<AtomicUsize>,
    emulator: Arc<EmulatorConnection>
}

impl Analyzer {
    pub fn new(client: Arc<Client>) -> Self {
        let state = State::from_client(client);
        let emulator = Arc::new(EmulatorConnection::new(state.clone()));
        Self {
            state,
            link_gen: Arc::new(AtomicUsize::new(0)),
            emulator
        }
    }

    pub async fn initialize(&self) {
        self.state.update_server_status("Ready").await;
        tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;

        // Load config and get path to output ROM file to start emulator connection
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            let config = ProjectConfig::try_load(&ProjectReader::from_absolute(workspace_path)).ok();
            if let Some(config) = config {
                self.emulator.listen(config.rom.output).await;

            } else {
                self.emulator.shutdown().await;
            }
        }
    }

    pub async fn shutdown(&self) {
        self.emulator.shutdown().await;
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
            if let Some((entries, rom_path)) = Parser::build(workspace_path, self.state.clone()).await {
                log::info!("Build ROM \"{}\" ({} entries)", rom_path.display(), entries.len());
            }
        }
    }
}

impl Analyzer {
    pub async fn emulator_start(&self) {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            if let Some((entries, rom_path)) = Parser::build(workspace_path, self.state.clone()).await {
                // Reset and reload existing emulator process if still running
                let was_reset = if let Some(process) = self.state.emulator().as_mut() {
                    process.reset(entries.clone())

                } else {
                    false
                };
                if !was_reset {
                    let state = self.state.clone();
                    Handle::current().spawn(async move {
                        if let Some(process) = EmulatorProcess::launch(state.clone(), rom_path, entries) {
                            state.set_emulator(Some(process));
                        }
                    });
                }
            }
        }
    }

    pub async fn emulator_write_memory(&self, address: String, value: String) {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {

            // Split symbols into variables and constants for lookup purposes
            let symbols = self.get_symbols(workspace_path).await.0;
            let (variables, constants): (Vec<GBCSymbol>, Vec<GBCSymbol>) = symbols.into_iter().filter(|s| {
                s.kind == SymbolKind::Constant ||
                s.kind == SymbolKind::Variable ||
                s.kind == SymbolKind::Field ||
                s.kind == SymbolKind::Function

            }).partition(|s| {
                s.kind != SymbolKind::Constant
            });


            // TODO support expressions like playerX + 2
            // TODO support all names i.e. use address if available and otherwise result

            // Parse or lookup address
            let address = address.trim();
            let address_info = if address.starts_with('$') {
                u32::from_str_radix(&address[1..], 16).ok().map(|v| (v, 1))

            } else if address.starts_with("0x") {
                u32::from_str_radix(&address[2..], 16).ok().map(|v| (v, 1))

            } else if address.starts_with("0b") {
                u32::from_str_radix(&address[2..], 2).ok().map(|v| (v, 1))

            } else if address.starts_with(|c| c >= '0' && c <= '9') {
                u32::from_str_radix(&address, 10).ok().map(|v| (v, 1))

            } else if let Some(variable) = variables.iter().find(|s| s.name == address) {
                variable.address.map(|v| (v as u32, variable.width))

            } else {
                None
            };

            if let Some((address, width)) = address_info {
                let value = value.trim();
                let value = if value.starts_with('$') {
                    i32::from_str_radix(&value[1..], 16).ok()

                } else if value.starts_with("0x") {
                    i32::from_str_radix(&value[2..], 16).ok()

                } else if value.starts_with("0b") {
                    i32::from_str_radix(&value[2..], 2).ok()

                } else if value.starts_with("-") {
                    i32::from_str_radix(&value, 10).ok()

                } else if value.starts_with(|c| c >= '0' && c <= '9') {
                    i32::from_str_radix(&value, 10).ok()

                } else if let Some(constant) = constants.iter().find(|s| s.name == value) {
                    if let Some(ExpressionResult::Integer(v)) = constant.result {
                        Some(v)

                    } else {
                        None
                    }

                } else {
                    None
                };
                match (value, width) {
                    (Some(value), 1) => {
                        log::info!("Write ${:0>4X} = ${:0>2X}", address, value as u8);
                        self.emulator_command(EmulatorCommand::WriteAddressValue(address, value as u8)).await;
                    },
                    (Some(value), 2) => {
                        log::info!("Write ${:0>4X} = ${:0>2X}", address, value as u8);
                        log::info!("Write ${:0>4X} = ${:0>2X}", address.saturating_add(1), ((value as u16) >> 8) as u8);
                        self.emulator_command(EmulatorCommand::WriteAddressValue(address, value as u8)).await;
                        self.emulator_command(EmulatorCommand::WriteAddressValue(address.saturating_add(1), ((value as u16) >> 8) as u8)).await;
                    },
                    _ => log::warn!("Invalid Write attempt to ${:0>4X}", address)
                }

            } else {
                log::warn!("Invalid Write attempt to \"{}\"", address)
            }
        }
    }

    pub async fn emulator_stop(&self) {
        if let Some(process) = self.state.emulator().as_mut() {
            process.stop();
        }
        self.state.set_emulator(None);
    }

    pub async fn emulator_command(&self, command: EmulatorCommand) {
        self.state.commands().push_back(command);
    }
}

impl Analyzer {
    pub async fn workspace_symbols(&self) -> Vec<SymbolInformation> {
        let workspace_path = self.state.workspace_path().clone();
        if let Some(workspace_path) = workspace_path {
            self.get_symbols(workspace_path).await.0.into_iter().filter(|symbol| {
                symbol.kind != SymbolKind::Method

            }).map(GBCSymbol::into_symbol_information).collect()

        } else {
           Vec::new()
        }
    }

    pub async fn document_symbols(&self, current_file: PathBuf) -> Vec<DocumentSymbol> {
        let workspace_path = current_file.clone();
        let uri = Url::from_file_path(current_file).unwrap();
        self.get_symbols(workspace_path).await.0.into_iter().filter(|symbol| {
            // Return if we either want all symbols or the symbol originates from the current file
            symbol.location.uri == uri && symbol.kind != SymbolKind::Method

        }).map(GBCSymbol::into_document_symbol).collect()
    }

    pub async fn completions(&self, current_file: PathBuf, _line: usize, _col: usize) -> Vec<CompletionItem> {
        let uri = Url::from_file_path(current_file.clone()).ok();
        return self.get_symbols(current_file).await.0.into_iter().filter(|symbol| {
            // Complete if the symbol is either global or originates from the current file
            symbol.is_global || Some(&symbol.location.uri) == uri.as_ref()

        }).flat_map(|symbol| match symbol.kind {
            SymbolKind::Constant => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Constant),
                detail: Some(format!("= {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Constructor => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Constructor),
                detail: Some(format!("MACRO{}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Function => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Function),
                detail: Some(format!("@ {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Variable => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Variable),
                detail: Some(format!("@ {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Field => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Field),
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
                SymbolKind::Namespace => {
                    Some(format!("SECTION \"{}\",{}", symbol.name, symbol.value))
                },
                SymbolKind::Constant => {
                    Some(format!("CONST {}", symbol.name))
                },
                // TODO show expanded tokens, need to record expanded entries (already needed above)
                // and then be able to to_string() them all again into source code
                SymbolKind::Constructor => {
                    Some(format!("MACRO {}{}", symbol.name, symbol.value))
                },
                SymbolKind::Function => {
                    Some(format!("{}: ; ({})", symbol.name, info))
                },
                SymbolKind::Variable => {
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
                SymbolKind::Field => {
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

    pub async fn symbol(&self, current_file: PathBuf, line: usize, col: usize) -> Option<GBCSymbol> {
        let current_file = PathBuf::from(current_file);
        let uri = Url::from_file_path(&current_file).unwrap();
        if let Some((token, _)) = Parser::get_token(&self.state, current_file.clone(), line, col) {
            let symbol_name = token.inner().value.to_string();
            let symbols = self.get_symbols(current_file).await.0;
            symbols.into_iter().find(|symbol| {
                // For child labels only search in the current file
                symbol.name == symbol_name &&
                    (symbol.kind != SymbolKind::Method || symbol.location.uri == uri)
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
        let (symbols, expansions, optimizations) = self.get_symbols(current_file).await;
        let symbols: Vec<GBCSymbol> = symbols.into_iter().filter(|symbol| {
            // Hint if the symbol is from the current file
            Some(&symbol.location.uri) == uri.as_ref()

        }).filter(|symbol| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            symbol.location.range.start.line < first_error_line as u32

        }).collect();

        let symbol_refs: Vec<&GBCSymbol> = symbols.iter().collect();
        let emulator_values = self.query_symbol_memory_values(&symbol_refs[..]);

        let mut hints: Vec<InlayHint> = symbols.into_iter().enumerate().flat_map(|(i, symbol)| match symbol.kind {
            SymbolKind::Namespace => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{}", symbol.value),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Constant => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Constructor => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("MACRO{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Function => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({})", symbol.value, symbol.info()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Variable => {
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
            SymbolKind::Field => Some(InlayHint {
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
        hints.append(&mut expansions.into_iter().filter(|exp| {
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
        hints.append(&mut optimizations.into_iter().filter(|(location, _)| {
            // Hint if the symbol is from the current file
            Some(&location.uri) == uri.as_ref()

        }).filter(|(location, _)| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            location.range.start.line < first_error_line as u32

        }).map(|(location, label)| {
            InlayHint {
                kind: InlayKind::OptimizerHint,
                label: format!("{} (optimized)", label),
                range: Range {
                    start: location.range.start,
                    end: location.range.start
                }
            }
        }).collect());
        hints
    }
}

impl Analyzer {
    async fn get_symbols(&self, workspace_path: PathBuf) -> (Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations) {
        let has_linker = self.state.symbols().is_some();
        if !has_linker {
            Parser::link(workspace_path, self.state.clone()).await;
            self.state.publish_diagnostics().await;
        }
        // TODO get rid of copy overhead
        // TODO use a callback function?
        self.state.symbols().clone().unwrap_or_else(|| (Vec::new(), Vec::new(), Vec::new()))
    }

    fn query_symbol_memory_values(&self, symbols: &[&GBCSymbol]) -> Vec<Option<u16>> {

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
        for symbol in symbols {
            if let Some(address) = symbol.address {
                if address <= 0xFFFF && (symbol.width == 1 || symbol.width == 2) {
                    let address = address as u16;
                    if symbol.width == 1 {
                        pending.push(PendingResult::Byte(address));
                        self.state.commands().push_back(EmulatorCommand::ReadAddressValue(address));

                    } else {
                        pending.push(PendingResult::Word(
                            address,
                            address.saturating_add(1)
                        ));
                        self.state.commands().push_back(EmulatorCommand::ReadAddressValue(address));
                        self.state.commands().push_back(EmulatorCommand::ReadAddressValue(address.saturating_add(1)));
                    }

                } else {
                    pending.push(PendingResult::Invalid);
                }
            } else {
                pending.push(PendingResult::Invalid);
            }
            result_set.push(None);
        }

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

