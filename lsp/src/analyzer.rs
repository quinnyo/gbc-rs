// STD Dependencies -----------------------------------------------------------
use std::thread;
use std::path::PathBuf;
use std::cell::RefCell;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::sync::atomic::AtomicUsize;


// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use tower_lsp::Client;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, DocumentSymbol, Range, NumberOrString};
use tower_lsp::lsp_types::{PublishDiagnosticsParams, SymbolInformation, SymbolKind, Url, Location, CompletionItem, CompletionItemKind};
use tower_lsp::lsp_types::notification::PublishDiagnostics;


// Internal Dependencies ------------------------------------------------------
use project::{ProjectConfig, ProjectReader};
use compiler::lexer::{
    stage::include::IncludeToken,
    LexerToken,
    LexerFile
};

use crate::{
    emulator::{EmulatorConnection, EmulatorCommand, EmulatorStatus},
    parser::Parser,
    types::{
        ServerStatusParams, ServerStatusNotification,
        InlayHint, InlayKind,
        GBCSymbol, MacroExpansion, Optimizations
    }
};


// Constants ------------------------------------------------------------------
const THREAD_DEBOUNCE: u64 = 250;


// Analyzer Implementation ----------------------------------------------------
pub struct Analyzer {
    client: Arc<Client>,
    documents: Arc<Mutex<RefCell<HashMap<String, String>>>>,
    tokens: Mutex<RefCell<HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>>>,
    workspace_path: Arc<Mutex<Option<PathBuf>>>,
    symbols: Arc<Mutex<Option<(Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations)>>>,
    diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
    addresses: Arc<Mutex<HashMap<usize, Location>>>,
    link_error: Arc<Mutex<Option<(Url, usize, usize)>>>,
    link_gen: Arc<AtomicUsize>,
    emulator_connection: Arc<EmulatorConnection>
}

impl Analyzer {
    pub fn new(client: Arc<Client>) -> Self {
        let addresses: Arc<Mutex<HashMap<usize, Location>>> = Arc::new(Mutex::new(HashMap::new()));
        let diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>> = Arc::new(Mutex::new(HashMap::new()));
        let emulator_connection = Arc::new(EmulatorConnection::new(
            client.clone(),
            diagnostics.clone(),
            addresses.clone()
        ));
        Self {
            client,
            documents: Arc::new(Mutex::new(RefCell::new(HashMap::new()))),
            tokens: Mutex::new(RefCell::new(HashMap::new())),
            workspace_path: Arc::new(Mutex::new(None)),
            symbols: Arc::new(Mutex::new(None)),
            diagnostics,
            addresses,
            link_error: Arc::new(Mutex::new(None)),
            link_gen: Arc::new(AtomicUsize::new(0)),
            emulator_connection
        }
    }

    pub async fn initialize(&self) {
        self.client.send_custom_notification::<ServerStatusNotification>(ServerStatusParams {
            quiescent: true,
            message: Some("Ready".to_string())

        }).await;

        // Load config and get path to output ROM file to start emulator connection
        let workspace_path = if let Ok(ws) = self.workspace_path.lock() { ws.clone() } else { None };
        if let Some(workspace_path) = workspace_path {
            let config = ProjectConfig::try_load(&ProjectReader::from_absolute(workspace_path)).ok();
            if let Some(config) = config {
                self.emulator_connection.listen(config.rom.output).await;

            } else {
                self.emulator_connection.shutdown().await;
            }
        }
    }

    pub async fn shutdown(&self) {
        self.emulator_connection.shutdown().await;
    }

    // TODO handle deletion and renaming of files
    pub async fn set_workspace_path(&self, path: PathBuf) {
        if let Ok(mut workspace_path) = self.workspace_path.lock() {
            *workspace_path = Some(path);
        }
    }

    pub fn open_document(&self, path: &str, text: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.insert(path.to_string(), text.to_string());
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn change_document(&self, path: &str, text: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.insert(path.to_string(), text.to_string());
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn save_document(&self, path: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.remove(path);
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn close_document(&self, path: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.remove(path);
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub async fn workspace_symbols(&self) -> Vec<SymbolInformation> {
        let workspace_path = if let Ok(ws) = self.workspace_path.lock() {
            ws.clone()

        } else {
            None
        };
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
        if let Some((token, _)) = Parser::get_token(&self.tokens, &self.documents, current_file.clone(), line, col) {
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

    pub async fn emulator_command(&self, command: EmulatorCommand) {
        if let Ok(mut c) = self.emulator_connection.commands.lock() {
            c.push_back(command);
        }
    }

    pub async fn toggle_breakpoint(&self, location: Location) {
        if let Some(address) = self.address_from_location(location) {
            self.emulator_command(EmulatorCommand::DebuggerToggleBreakpoint(address)).await;
        }
    }

    fn address_from_location(&self, location: Location) -> Option<u16> {
        if let Ok(addresses) = self.addresses.lock() {
            for (address, loc) in addresses.iter() {
                if location.uri == loc.uri && location.range.start.line == loc.range.start.line {
                    return Some(*address as u16);
                }
            }
        }
        None
    }

    pub async fn inlay_hints(&self, current_file: PathBuf) -> Vec<InlayHint> {
        // Get the line of the first error in the current file (if any)
        let first_error_line = if let Ok(error) = self.link_error.lock() {
            if let Some((p, line, _)) = &*error {
                if PathBuf::from(p.path()) == current_file {
                    *line

                } else {
                    10_000_000
                }

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
        let has_linker = if let Ok(symbols) = self.symbols.lock() {
            symbols.is_some()

        } else {
            false
        };
        if !has_linker {
            Parser::link(
                self.client.clone(),
                NumberOrString::Number(0),
                workspace_path,
                self.symbols.clone(),
                self.link_error.clone(),
                self.diagnostics.clone(),
                self.addresses.clone(),
                self.documents.clone()

            ).await;
            Self::publish_diagnostics(
                self.client.clone(),
                self.diagnostics.clone(),
                self.addresses.clone(),
                self.emulator_connection.status.clone()

            ).await;
        }
        if let Ok(symbols) = self.symbols.lock() {
            symbols.clone().unwrap_or_else(|| (Vec::new(), Vec::new(), Vec::new()))

        } else {
            (Vec::new(), Vec::new(), Vec::new())
        }
    }

    fn query_symbol_memory_values(&self, symbols: &[&GBCSymbol]) -> Vec<Option<u16>> {

        // Generate initial result set
        let mut results = Vec::new();
        for _ in 0..symbols.len() {
            results.push(None)
        }

        // Do nothing if no emulator is present
        if let Ok(status) = self.emulator_connection.status.lock() {
            if status.is_none() {
                return results;
            }
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
        if let Ok(mut q) = self.emulator_connection.commands.lock() {
            for symbol in symbols {
                if let Some(address) = symbol.address {
                    if address <= 0xFFFF && (symbol.width == 1 || symbol.width == 2) {
                        let address = address as u16;
                        if symbol.width == 1 {
                            pending.push(PendingResult::Byte(address));
                            q.push_back(EmulatorCommand::QueryAddressValue(address));

                        } else {
                            pending.push(PendingResult::Word(
                                address,
                                address.saturating_add(1)
                            ));
                            q.push_back(EmulatorCommand::QueryAddressValue(address));
                            q.push_back(EmulatorCommand::QueryAddressValue(address.saturating_add(1)));
                        }

                    } else {
                        pending.push(PendingResult::Invalid);
                    }
                } else {
                    pending.push(PendingResult::Invalid);
                }
                results.push(None);
            }
        }

        // Wait for results from emulator
        let started = Instant::now();
        while started.elapsed() < Duration::from_millis(300) {
            if let Ok(mut r) = self.emulator_connection.results.lock() {
                let mut waiting = 0;
                for (i, pending) in pending.iter_mut().enumerate() {
                    match pending {
                        PendingResult::Byte(address) => {
                            if let Some(value) = r.remove(address) {
                                results[i] = Some(value as u16);
                                *pending = PendingResult::Done;
                            }
                            waiting += 1;
                        },
                        PendingResult::Word(address_low, address_high) => {
                            if r.contains_key(address_low) && r.contains_key(address_high) {
                                results[i] = Some(r[address_low] as u16 | (r[address_high] as u16) << 8);
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
            }
            thread::sleep(Duration::from_millis(10));
        }
        results
    }

    fn link_async(&self, workspace_path: PathBuf) where Self: 'static {
        let client = self.client.clone();
        let symbols = self.symbols.clone();
        let diagnostics = self.diagnostics.clone();
        let addresses = self.addresses.clone();
        let link_gen = self.link_gen.clone();
        let link_error = self.link_error.clone();
        let documents = self.documents.clone();
        let status = self.emulator_connection.status.clone();

        // Bump symbol generation
        link_gen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let thread_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);

        let handle = Handle::current();
        handle.spawn(async move {
            // Wait for debounce purposes
            tokio::time::sleep(tokio::time::Duration::from_millis(THREAD_DEBOUNCE)).await;

            // Check if still the latest link request
            let latest_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
            if thread_gen == latest_gen {
                Parser::link(
                    client.clone(),
                    NumberOrString::Number(thread_gen as i32),
                    workspace_path,
                    symbols,
                    link_error,
                    diagnostics.clone(),
                    addresses.clone(),
                    documents
                ).await;
                Self::publish_diagnostics(client, diagnostics, addresses, status.clone()).await;
            }
        });
    }

    pub async fn publish_diagnostics(
        client: Arc<Client>,
        diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
        addresses: Arc<Mutex<HashMap<usize, Location>>>,
        status: Arc<Mutex<Option<EmulatorStatus>>>
    ) {
        let diagnostics = if let Ok(mut diagnostics) = diagnostics.lock() {

            // Remove any Information diagnostics
            for (_, diagnostics) in diagnostics.iter_mut() {
                *diagnostics = diagnostics.iter().cloned().filter(|d| {
                    !d.message.starts_with("Debugger")

                }).collect();
            }

            if let Ok(status) = status.lock() {
                // Add debugger location
                if let Some(status) = status.as_ref() {
                    if let Ok(addresses) = addresses.lock() {
                        for b in &status.breakpoints {
                            if let Some(location) = addresses.get(&(b.address as usize)) {
                                let info = Diagnostic {
                                    message: format!("Debugger Breakpoint @ ${:0>4X}", b.address),
                                    range: location.range,
                                    severity: Some(DiagnosticSeverity::Warning),
                                    .. Diagnostic::default()
                                };
                                diagnostics.entry(location.uri.clone()).or_insert_with(Vec::new).push(info);
                            }
                        }
                        if status.debugger == 1 {
                            if let Some(location) = addresses.get(&(status.pc as usize)) {
                                let info = Diagnostic {
                                    message: format!(
                                        "Debugger Halt\nAF={:0>4X}\nBC={:0>4X}\nDE={:0>4X}\nHL={:0>4X}\nSP={:0>4X}\nPC={:0>4X}",
                                        status.registers[0],
                                        status.registers[1],
                                        status.registers[2],
                                        status.registers[3],
                                        status.registers[4],
                                        status.pc,
                                    ),
                                    range: location.range,
                                    severity: Some(DiagnosticSeverity::Hint),
                                    .. Diagnostic::default()
                                };
                                diagnostics.entry(location.uri.clone()).or_insert_with(Vec::new).push(info);
                            }
                        }
                    }
                }
            }
            diagnostics.clone()

        } else {
            HashMap::new()
        };

        // Send diagnostics for all files
        for (uri, diagnostics) in &diagnostics {
            client.send_custom_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None
            }).await;
        }
    }
}

