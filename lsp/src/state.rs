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
use tower_lsp::lsp_types::DocumentSymbol;
use tower_lsp::lsp_types::{PublishDiagnosticsParams, SymbolInformation, SymbolKind, Url, Location, CompletionItem, CompletionItemKind};
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
        Linker, SectionEntry, EntryData
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
    pub in_macro: bool,
    pub location: Location,
    pub name: String,
    pub value: String,
    pub width: usize,
    pub children: Vec<GBCSymbol>,
    pub references: Vec<Location>
}

impl GBCSymbol {
    fn into_symbol_information(self) -> SymbolInformation {
        SymbolInformation {
            name: self.name,
            kind: self.kind,
            tags: None,
            deprecated: None,
            location: self.location,
            container_name: None
        }
    }

    fn into_document_symbol(self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.name,
            kind: self.kind,
            tags: None,
            deprecated: None,
            range: self.location.range,
            selection_range: self.location.range,
            detail: Some(self.value),
            children: Some(self.children.into_iter().map(GBCSymbol::into_document_symbol).collect())
        }
    }
}

// State Abstraction ----------------------------------------------------------
// TODO rename Analyzer
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
            self.get_symbols(workspace_path).await.into_iter().map(GBCSymbol::into_symbol_information).collect()

        } else {
           Vec::new()
        }
    }

    pub async fn document_symbols(&self, current_file: PathBuf) -> Vec<DocumentSymbol> {
        let workspace_path = current_file.clone();
        let uri = Url::from_file_path(current_file).unwrap();
        self.get_symbols(workspace_path).await.into_iter().filter(|symbol| {
            // Return if we either want all symbols or the symbol originates from the current file
            symbol.location.uri == uri

        }).map(GBCSymbol::into_document_symbol).collect()
    }
}

impl State {
    pub async fn completions(&self, current_file: PathBuf, _line: usize, _col: usize) -> Vec<CompletionItem> {
        let uri = Url::from_file_path(current_file.clone()).ok();
        return self.get_symbols(current_file).await.into_iter().filter(|symbol| {
            // Complete if the symbol is either global or originates from the current file
            symbol.is_global || Some(&symbol.location.uri) == uri.as_ref()

        }).flat_map(|symbol| match symbol.kind {
            SymbolKind::Constant => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Constant),
                detail: Some(format!("Value: {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Function => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Function),
                detail: Some(format!("Address: {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Variable => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Variable),
                detail: Some(format!("Address: {}", symbol.value)),
                .. CompletionItem::default()
            }),
            SymbolKind::Field => Some(CompletionItem {
                label: symbol.name,
                kind: Some(CompletionItemKind::Field),
                detail: Some(format!("Address: {}", symbol.value)),
                .. CompletionItem::default()
            }),
            _ => None

        // TODO sort by relevance?
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
            SymbolKind::Namespace => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{}", symbol.value),
                range: symbol.location.range
            }),
            SymbolKind::Constant => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: symbol.location.range
            }),
            SymbolKind::Function => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: symbol.location.range
            }),
            SymbolKind::Variable => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: symbol.location.range
            }),
            SymbolKind::Field => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: symbol.location.range
            }),
            _ => None

        }).collect()
    }

    pub async fn hover(&self, current_file: PathBuf, line: usize, col: usize) -> Option<(String, Location)> {
        if let Some(symbol) = self.symbol(current_file, line, col).await {
            let location = symbol.location;
            let is_global = symbol.is_global;
            let m = match symbol.kind {
                SymbolKind::Namespace => {
                    Some(format!("SECTION \"{}\",{}", symbol.name, symbol.value))
                },
                SymbolKind::Constant => {
                    Some(format!("CONST {}", symbol.name))
                },
                SymbolKind::Function => {
                    Some(format!("{}:", symbol.name))
                },
                SymbolKind::Variable => {
                    Some(format!("{}: {}", symbol.name, if symbol.width == 1 {
                        "DB"

                    } else if symbol.width == 2 {
                        "DW"

                    } else {
                        "DS"
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
        let mut logger = Logger::new();
        logger.set_silent();

        let (_, reader) = Self::load_project(&mut logger, current_file.clone(), &self.documents);
        let relative_file = current_file.strip_prefix(reader.base_dir()).unwrap().to_path_buf();

        let data = if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            if let Some(data) = tokens.get(&current_file) {
                Some(data.clone())

            } else {
                let mut files = Vec::new();
                if let Ok(parsed) = IncludeStage::tokenize_single(&reader, &relative_file, &mut files) {
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
        if !has_linker {
            Self::link(
                self.client.clone(),
                NumberOrString::Number(0),
                workspace_path,
                self.symbols.clone(),
                self.link_error.clone(),
                self.diagnostic_urls.clone(),
                self.documents.clone()
            ).await;
        }
        if let Ok(symbols) = self.symbols.lock() {
            symbols.clone().unwrap_or_else(Vec::new)

        } else {
            Vec::new()
        }
    }

    fn resolve_reference(linker: &Linker, file_index: usize, start_index: usize, macro_call_id: Option<usize>, l: usize) -> Location {
        let context = linker.context(None);

        // Follow macro calls to their original source
        let macro_call = macro_call_id.and_then(|id| context.macro_calls.get(id));
        if let Some(macro_call) = macro_call {
            let token = macro_call.name();
            let file = &context.files[token.file_index];
            let (line, col) = file.get_line_and_col(token.start_index);
            Location {
                uri: Url::from_file_path(&file.path).unwrap(),
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: col as u32
                    },
                    end: Position {
                        line: line as u32,
                        character: (col + l) as u32
                    }
                }
            }

        } else {
            let file = &context.files[file_index];
            let (line, col) = file.get_line_and_col(start_index);
            Location {
                uri: Url::from_file_path(&file.path).unwrap(),
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: col as u32
                    },
                    end: Position {
                        line: line as u32,
                        character: (col + l) as u32
                    }
                }
            }
        }
    }

    fn parse_symbols(linker: Linker) -> Vec<GBCSymbol> {
        let mut symbols: Vec<GBCSymbol> = Vec::new();
        let context = linker.context(None);

        // Constants
        for (index, constant) in context.constants {
            let token = &constant.inner;
            let name = token.value.to_string();
            if let Some((result, _)) = context.constant_values.get(index) {
                let file = &context.files[constant.inner.file_index];
                let (line, col) = file.get_line_and_col(constant.inner.start_index);
                let (eline, ecol) = file.get_line_and_col(constant.inner.end_index);
                let references = context.constant_usage.get(index).map(|refs| {
                    refs.iter().map(|(file_index, start_index, macro_call_id)| {
                        Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                    }).collect()

                }).unwrap_or_else(Vec::new);
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constant,
                    is_global: index.1.is_none(),
                    in_macro: token.macro_call_id.is_some(),
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
                    name,
                    width: 0,
                    value: result.to_string(),
                    children: Vec::new(),
                    references
                });
            }
        }

        // Sections
        // TODO list stuff inside as children
        // TODO document_symbols must also return sections in case their children are part of a
        // given file even if the section itself is not
        for section in &linker.sections {
            let file = &context.files[section.inner.file_index];
            let (line, col) = file.get_line_and_col(section.inner.start_index);
            let (eline, ecol) = file.get_line_and_col(section.inner.end_index);
            symbols.push(GBCSymbol {
                kind: SymbolKind::Namespace,
                is_global: false,
                in_macro: section.inner.macro_call_id.is_some(),
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
                name: section.name.to_string(),
                width: 0,
                value: format!("{}[${:0>4x}-${:0>4x}][{}]", section.segment, section.start_address, section.end_address, section.bank),
                children: Vec::new(),
                references: Vec::new()
            });
        }

        // Variables / Labels
        let mut last_parent_label_index: Option<usize> = None;
        let sections = linker.section_entries();
        for entries in sections {
            let mut entries = entries.iter().peekable();
            while let Some(entry) = entries.next() {
                let file_index = entry.inner.file_index;
                let entry_name = &entry.inner.value;
                if let EntryData::Label { is_local, name, .. } = &entry.data {

                    // Local label under a parent
                    let (kind, size) = if *is_local {
                        if let Some(index) = last_parent_label_index {

                            // Add to last parent label
                            if let Some(symbol) = symbols.get_mut(index) {
                                let file = &context.files[file_index];
                                let (line, col) = file.get_line_and_col(entry.inner.start_index);
                                let (eline, ecol) = file.get_line_and_col(entry.inner.end_index);
                                symbol.children.push(GBCSymbol {
                                    kind: SymbolKind::Method,
                                    is_global: false,
                                    in_macro: entry.inner.macro_call_id.is_some(),
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
                                    name: name.to_string(),
                                    width: 0,
                                    value: "".to_string(),
                                    children: Vec::new(),
                                    references: Vec::new()
                                });
                            }
                        }
                        (SymbolKind::Method, None)

                    // Field Data
                    } else if let Some(SectionEntry {
                        size,
                        data: EntryData::Data {
                            bytes: Some(_),
                            ..
                        },
                        ..

                    }) = entries.peek() {
                        // TODO collect multiple fields together
                        (SymbolKind::Field, Some(*size))

                    // Variable
                    } else if let Some(SectionEntry {
                        size,
                        data: EntryData::Data {
                            bytes: None,
                            ..
                        },
                        ..

                    }) = entries.peek() {
                        (SymbolKind::Variable, Some(*size))

                    // Normal label
                    } else {
                        last_parent_label_index = Some(symbols.len());
                        (SymbolKind::Function, None)
                    };

                    // Find matching label reference
                    for ((symbol, label_id, is_global), token) in context.labels {
                        if token.file_index == file_index && symbol == entry_name {
                            let name = symbol.to_string();
                            let file = &context.files[token.file_index];
                            let (line, col) = file.get_line_and_col(token.start_index);
                            let (eline, ecol) = file.get_line_and_col(token.end_index);
                            let address = context.label_addresses.get(label_id).unwrap_or(&0);
                            let references = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().map(|(file_index, start_index, macro_call_id)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            // TODO for callable labels list register arguments
                            symbols.push(GBCSymbol {
                                kind,
                                is_global: *is_global,
                                in_macro: token.macro_call_id.is_some(),
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
                                name,
                                width: size.unwrap_or(0),
                                value: if let Some(size) = size {
                                    if size <= 1 {
                                        format!("${:0>4x} ({} byte)", address, size)

                                    } else {
                                        format!("${:0>4x} ({} bytes)", address, size)
                                    }

                                } else {
                                    format!("${:0>4x}", address)
                                },
                                children: Vec::new(),
                                references
                            });
                            break;
                        }
                    }
                }
            }
        }

        // Sort by Name
        symbols.sort_by(|a, b| {
            a.name.cmp(&b.name)
        });

        // Ignore everything outside of gbc files
        symbols.into_iter().filter(|s| {
            s.location.uri.path().ends_with(".gbc")

        }).collect()
    }

    fn parse_warnings(symbols: &[GBCSymbol]) -> Vec<(Url, Diagnostic)> {
        let mut warnings = Vec::new();
        for symbol in symbols {

            // Ignore Sections, symbols inside macros, symbols prefixed with _ and symbols inside a
            // library folder
            if symbol.kind == SymbolKind::Namespace
                || symbol.in_macro
                || symbol.name.starts_with('_')
                || symbol.location.uri.path().contains("lib") {
                continue;
            }

            // Unused Symbols
            if symbol.references.is_empty() {
                warnings.push((symbol.location.uri.clone(), Diagnostic {
                    message: "Is never used".to_string(),
                    range: symbol.location.range.clone(),
                    severity: Some(DiagnosticSeverity::Warning),
                    .. Diagnostic::default()
                }));

            // Symbols unused outside their file
            } else if symbol.is_global {
                // TODO check if referenced from within a macro OR fix macro reference detection
                let outer_refs = symbol.references.iter().filter(|r| r.uri != symbol.location.uri).count();
                if outer_refs == 0 {
                    warnings.push((symbol.location.uri.clone(), Diagnostic {
                        message: "Is only used inside this file".to_string(),
                        range: symbol.location.range.clone(),
                        severity: Some(DiagnosticSeverity::Warning),
                        .. Diagnostic::default()
                    }));
                }
            }
        }
        warnings
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
            // Wait for debounce purposes TODO better way to do this i.e. canceling the previous
            // task?
            tokio::time::sleep(tokio::time::Duration::from_millis(THREAD_DEBOUNCE)).await;

            // Check if still the latest link request
            let latest_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
            if thread_gen == latest_gen {
                Self::link(
                    client,
                    NumberOrString::Number(thread_gen as i32),
                    workspace_path,
                    symbols,
                    link_error,
                    diagnostic_urls,
                    documents
                ).await;
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

        }).await.ok();
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
        let mut warnings: Vec<(Url, Diagnostic)> = Vec::new();
        match compiler.create_linker(&mut logger, &mut reader, main_file) {
            Ok(linker) => {
                log::info!(&format!("Linked in {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Linking complete").await;

                // Generate and store new symbols
                if let Ok(mut symbols) = outer_symbols.lock() {
                    let s = Self::parse_symbols(linker);
                    warnings.append(&mut Self::parse_warnings(&s));
                    log::info!(&format!("{} symbol(s)", s.len()));
                    *symbols = Some(s);
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

