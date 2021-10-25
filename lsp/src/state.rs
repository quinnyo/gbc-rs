// STD Dependencies -----------------------------------------------------------
//use std::thread;
use std::sync::Arc;
use std::sync::Mutex;
use std::path::PathBuf;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;


// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use serde::{Serialize, Deserialize};
use tower_lsp::Client;
use tower_lsp::lsp_types::Diagnostic;
use tower_lsp::lsp_types::DiagnosticSeverity;
use tower_lsp::lsp_types::{PublishDiagnosticsParams, SymbolInformation, SymbolKind, Url, Location};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::{Range, Position, NumberOrString, WorkDoneProgressCreateParams};


// Internal Dependencies ------------------------------------------------------
use file_io::Logger;
use project::{ProjectConfig, ProjectReader};
use compiler::{
    compiler::Compiler,
    lexer::{
        stage::include::{IncludeStage, IncludeToken},
        LexerToken,
        LexerFile
    },
    linker::{
        Completion, Linker, EntryData
    }
};
use crate::{InlayHint, InlayKind};


// Constants ------------------------------------------------------------------
const THREAD_DEBOUNCE: u64 = 250;


// Types ----------------------------------------------------------------------
pub enum ServerStatusNotification {}
impl Notification for ServerStatusNotification {
    type Params = ServerStatusParams;
    const METHOD: &'static str = "experimental/serverStatus";
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct ServerStatusParams {
    pub quiescent: bool,
    pub message: Option<String>,
}

pub enum InlayHintsNotification {}
impl Notification for InlayHintsNotification {
    type Params = InlayHintsParams;
    const METHOD: &'static str = "experimental/inlayHints";
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct InlayHintsParams {}

#[derive(Debug, Clone)]
pub struct GBCSymbol {
    pub kind: SymbolKind,
    pub is_global: bool,
    pub location: Location,
    pub address: usize,
    pub name: String,
    pub value: String,
    pub children: Vec<GBCSymbol>,
    pub references: Vec<Location>
}


// State Abstraction ----------------------------------------------------------
pub struct State {
    client: Arc<Client>,
    documents: Arc<Mutex<RefCell<HashMap<String, String>>>>,
    tokens: Mutex<RefCell<HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>>>,
    workspace_path: Arc<Mutex<Option<PathBuf>>>,
    symbols: Arc<Mutex<Option<Vec<GBCSymbol>>>>,
    diagnostic_urls: Arc<Mutex<Vec<Url>>>,
    link_error: Arc<Mutex<Option<(Url, usize, usize)>>>,
    link_gen: Arc<AtomicUsize>
}

impl State {
    pub fn new(client: Arc<Client>) -> Self {
        Self {
            client,
            documents: Arc::new(Mutex::new(RefCell::new(HashMap::new()))),
            tokens: Mutex::new(RefCell::new(HashMap::new())),
            workspace_path: Arc::new(Mutex::new(None)),
            symbols: Arc::new(Mutex::new(None)),
            diagnostic_urls: Arc::new(Mutex::new(Vec::new())),
            link_error: Arc::new(Mutex::new(None)),
            link_gen: Arc::new(AtomicUsize::new(0))
        }
    }

    pub async fn initialize(&self) {
        self.client.send_custom_notification::<ServerStatusNotification>(ServerStatusParams {
            quiescent: true,
            message: Some("Ready".to_string())

        }).await;
    }

    // TODO handle deletion and renaming of files
    pub fn set_workspace_path(&self, path: PathBuf) {
        if let Ok(mut workspace_path) = self.workspace_path.lock() {
            log::info!(&format!("WorkspacePath {}", path.display()));
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
            log::info!(&format!("Cleared cached tokens for {}...", path));
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
            log::info!(&format!("Cleared cached tokens for {}...", path));
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
            log::info!(&format!("Cleared cached tokens for {}...", path));
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub async fn symbols(&self, current_file: Option<PathBuf>) -> Vec<SymbolInformation> {
        let mut workspace_path = current_file.clone();
        if workspace_path.is_none() {
            if let Ok(ws) = self.workspace_path.lock() {
                workspace_path = ws.clone();
            }
        }
        if let Some(workspace_path) = workspace_path {
            let uri = current_file.map(Url::from_file_path).transpose().ok().flatten();
            self.get_symbols(workspace_path).await.into_iter().filter(|symbol| {
                // Return if we either want all symbols or the symbol originates from the current file
                uri.is_none() || Some(&symbol.location.uri) == uri.as_ref()

            }).map(|symbol| {
                SymbolInformation {
                    name: symbol.name,
                    kind: symbol.kind,
                    tags: None,
                    deprecated: None,
                    location: symbol.location,
                    container_name: None
                }
            }).collect()

        } else {
           Vec::new()
        }
    }
}

impl State {
    pub async fn completions(&self, current_file: PathBuf, _line: usize, _col: usize) -> Vec<Completion> {
        let uri = Url::from_file_path(current_file.clone()).ok();
        return self.get_symbols(current_file).await.into_iter().filter(|symbol| {
            // Complete if the symbol is either global or originates from the current file
            symbol.is_global || Some(&symbol.location.uri) == uri.as_ref()

        }).flat_map(|symbol| match symbol.kind {
            SymbolKind::Constant => Some(Completion::Constant {
                name: symbol.name,
                info: Some(format!("Value: {}", symbol.value))
            }),
            // TODO Functions info: Some(format!("Address: ${:0>4x}", address))
            SymbolKind::Function => None,
            SymbolKind::Variable => None,
            _ => None

        }).collect();
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
        return self.get_symbols(current_file).await.into_iter().filter(|symbol| {
            // Hint if the symbol is from the current file
            Some(&symbol.location.uri) == uri.as_ref()

        }).filter(|symbol| {
            // Don't generate hints for lines after the current error as they'll be
            // cached and potentially incorrect
            symbol.location.range.start.line < first_error_line as u32

        }).flat_map(|symbol| match symbol.kind {
            SymbolKind::Constant => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: symbol.location.range
            }),
            // TODO format!("${:0>4x} (Address) ({} refs)", address, refs)
            SymbolKind::Function => None,
            SymbolKind::Variable => None,
            _ => None

        }).collect()
    }

    pub async fn hover(&self, current_file: PathBuf, line: usize, col: usize) -> Option<(String, Location)> {
        if let Some(symbol) = self.symbol(current_file, line, col).await {
            Some((
                // TODO show callers
                // TODO details for constants / functions / variables
                // TODO show ref count
                format!(
                    "# `{}`\n\nDefined in {}:{}:{}",
                    symbol.name,
                    symbol.location.uri.path(),
                    symbol.location.range.start.line + 1,
                    symbol.location.range.start.character + 1
                ),
                symbol.location
            ))

        } else {
            None
        }
    }

    pub async fn symbol(&self, current_file: PathBuf, line: usize, col: usize) -> Option<GBCSymbol> {
        let current_file = PathBuf::from(current_file);
        if let Some((token, _)) = self.parse_token(current_file.clone(), line, col) {
            let symbol_name = token.inner().value.to_string();
            let symbols = self.get_symbols(current_file).await;
            symbols.into_iter().find(|symbol| {
                symbol.name == symbol_name
            })

        } else {
            None
        }
    }
}

impl State {
    fn parse_token(&self, current_file: PathBuf, line: usize, col: usize) -> Option<(IncludeToken, LexerFile)> {
        log::info!(&format!("Getting token from {:?}:{}:{}...", current_file, line, col));

        let mut logger = Logger::new();
        logger.set_silent();

        let (_, reader) = Self::load_project(&mut logger, current_file.clone(), &self.documents);
        let relative_file = current_file.strip_prefix(reader.base_dir()).unwrap().to_path_buf();

        let data = if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            if let Some(data) = tokens.get(&current_file) {
                log::info!(&format!("Returned {} cached token(s)...", data.0.len()));
                Some(data.clone())

            } else {
                let mut files = Vec::new();
                if let Ok(parsed) = IncludeStage::tokenize_single(&reader, &relative_file, &mut files) {
                    log::info!(&format!("Parsed {} token(s)...", parsed.len()));
                    let data = (parsed, files.remove(0));
                    tokens.insert(current_file, data.clone());
                    Some(data)

                } else {
                    None
                }
            }

        } else {
            None
        };
        if let Some((tokens, file)) = data {
            let index = file.get_index(line, col);
            for t in tokens {
                if index >= t.inner().start_index && index < t.inner().end_index {
                    return Some((t, file))
                }
            }
        }
        None
    }

    fn load_project(logger: &mut Logger, workspace_path: PathBuf, documents: &Arc<Mutex<RefCell<HashMap<String, String>>>>) -> (ProjectConfig, ProjectReader) {
        // TODO cache config
        let config = ProjectConfig::load(logger, &ProjectReader::from_absolute(workspace_path));
        let reader = ProjectReader::from_relative(config.rom.input.clone());

        // Overlay LS files on file system reader
        if let Ok(documents) = documents.lock() {
            let documents = documents.borrow();
            for (path, text) in &*documents {
                reader.overlay_file(PathBuf::from(path), text.clone());
            }
        }
        (config, reader)
    }

    async fn get_symbols(&self, workspace_path: PathBuf) -> Vec<GBCSymbol> {
        let has_linker = if let Ok(symbols) = self.symbols.lock() {
            symbols.is_some()

        } else {
            false
        };
        if has_linker {
            log::info!("Returning cached symbols");

        } else {
            //let handle = Handle::current();
            //handle.block_on(async move {
                log::info!("Force Linking...");
                Self::link(
                    self.client.clone(),
                    NumberOrString::Number(0),
                    workspace_path,
                    self.symbols.clone(),
                    self.link_error.clone(),
                    self.diagnostic_urls.clone(),
                    self.documents.clone()
                ).await;
            //})
        }
        if let Ok(symbols) = self.symbols.lock() {
            symbols.clone().unwrap_or_else(Vec::new)

        } else {
            Vec::new()
        }
    }

    fn parse_symbols(linker: Linker) -> Vec<GBCSymbol> {
        let mut symbols = Vec::new();
        let context = linker.context(None);

        // Constant Symbols
        for (index, constant) in context.constants {
            let token = &constant.inner;
            let name = token.value.to_string();
            if let Some((result, _)) = context.constant_values.get(index) {
                let file = &context.files[constant.inner.file_index];
                let (line, col) = file.get_line_and_col(constant.inner.start_index);
                let (eline, ecol) = file.get_line_and_col(constant.inner.end_index);
                let references = context.constant_usage.get(index).map(|refs| {
                    refs.iter().map(|(file_index, start_index)| {
                        let file = &context.files[*file_index];
                        let (line, col) = file.get_line_and_col(*start_index);
                        Location {
                            uri: Url::from_file_path(&file.path).unwrap(),
                            range: Range {
                                start: Position {
                                    line: line as u32,
                                    character: col as u32
                                },
                                end: Position {
                                    line: line as u32,
                                    character: (col + name.len()) as u32
                                }
                            }
                        }

                    }).collect()

                }).unwrap_or_else(Vec::new);
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constant,
                    is_global: index.1.is_none(),
                    location: Location {
                        uri: Url::from_file_path(&file.path).unwrap(),
                        range: Range {
                            start: Position {
                                line: line as u32,
                                character: col as u32
                            },
                            end: Position {
                                line: eline as u32,
                                character: ecol as u32
                            }
                        }
                    },
                    address: 0,
                    name,
                    value: result.to_string(),
                    children: Vec::new(),
                    references
                });
            }
        }

        // TODO Differentiate Variables and Functions
        let sections = linker.section_entries();
        for entries in sections {
            let mut entries = entries.iter();
            while let Some(entry) = entries.next() {
                if let EntryData::Label { id, is_local, name } = &entry.data {
                    // TODO any labels
                }
            }
        }
        symbols
    }

    fn link_async(&self, workspace_path: PathBuf) where Self: 'static {
        let client = self.client.clone();
        let symbols = self.symbols.clone();
        let diagnostic_urls = self.diagnostic_urls.clone();
        let link_gen = self.link_gen.clone();
        let link_error = self.link_error.clone();
        let documents = self.documents.clone();

        // Bump symbol generation
        link_gen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let thread_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);

        let handle = Handle::current();
        handle.spawn(async move {
            // Wait for debounc purposes
            tokio::time::sleep(tokio::time::Duration::from_millis(THREAD_DEBOUNCE)).await;

            // Check if still the latest link request
            let latest_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
            if thread_gen == latest_gen {
                log::info!("Linking in background...");
                Self::link(
                    client,
                    NumberOrString::Number(thread_gen as i32),
                    workspace_path,
                    symbols,
                    link_error,
                    diagnostic_urls,
                    documents
                ).await;

            } else {
                log::info!("Link canceled");
            }
        });
    }

    // TODO handle lint warnings as diagnostics
    async fn link(
        client: Arc<Client>,
        link_id: NumberOrString,
        workspace_path: PathBuf,
        outer_symbols: Arc<Mutex<Option<Vec<GBCSymbol>>>>,
        outer_error: Arc<Mutex<Option<(Url, usize, usize)>>>,
        outer_diagnostic_urls: Arc<Mutex<Vec<Url>>>,
        documents: Arc<Mutex<RefCell<HashMap<String, String>>>>
    ) {
        // Tell client about the linking
        client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: link_id.clone()

        }).await;
        client.show_progress_begin(link_id.clone(), "Linking...").await;

        // Start Linking Process
        let start = std::time::Instant::now();
        let mut logger = Logger::new();
        logger.set_silent();

        let (config, mut reader) = Self::load_project(&mut logger, workspace_path, &documents);

        // Create compiler
        let main_file = PathBuf::from(config.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();
        compiler.set_linter_enabled();

        // Run linker
        let mut errors: Vec<(Url, Diagnostic)> = Vec::new();
        let warnings: Vec<(Url, Diagnostic)> = Vec::new();
        match compiler.create_linker(&mut logger, &mut reader, main_file) {
            Ok(linker) => {
                log::info!(&format!("Linked in {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Linking complete").await;

                // Generate and store new symbols
                if let Ok(mut symbols) = outer_symbols.lock() {
                    *symbols = Some(Self::parse_symbols(linker));
                }
            },
            Err(err) => {
                log::error!(&format!("Linking failed after {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Linking failed").await;
                if let Some((path, line, col, message)) = err.into_diagnostic() {
                    let uri = Url::from_file_path(path).unwrap();
                    errors.push((uri, Diagnostic {
                        range: Range {
                            start: Position {
                                line: line as u32,
                                character: col as u32
                            },
                            end: Position {
                                line: line as u32,
                                character: col as u32
                            }
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        message,
                        code: None,
                        code_description: None,
                        source: None,
                        related_information: None,
                        tags: None,
                        data: None
                    }));

                } else {
                    // TODO show at current location
                }
            }
        }

        // Update Error State
        if let Ok(mut error) = outer_error.lock() {
            *error = errors.first().map(|(url, diagnostic)| {
                (url.clone(), diagnostic.range.start.line as usize, diagnostic.range.start.character as usize)
            });
        }

        // Send Diagnostics to client
        let mut diagnostics = HashMap::new();

        // Make sure to send empty diagnostics for any previous files so they get cleared
        if let Ok(mut urls) = outer_diagnostic_urls.lock() {
            for uri in urls.drain(0..) {
                diagnostics.insert(uri, Vec::new());
            }
        }

        for (uri, err) in errors {
            diagnostics.entry(uri).or_insert_with(Vec::new).push(err);
        }
        for (uri, warn) in warnings {
            diagnostics.entry(uri).or_insert_with(Vec::new).push(warn);
        }
        for (uri, diagnostics) in &diagnostics {
            client.send_custom_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None
            }).await;
        }

        if let Ok(mut urls) = outer_diagnostic_urls.lock() {
            *urls = diagnostics.into_keys().collect();
        }

        // Trigger new inlay hint fetching
        client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
    }
}

