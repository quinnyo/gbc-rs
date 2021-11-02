// STD Dependencies -----------------------------------------------------------
//use std::thread;
use std::sync::Arc;
use std::sync::Mutex;
use std::path::PathBuf;
use std::cell::RefCell;
use std::sync::atomic::AtomicUsize;
use std::collections::{HashMap, HashSet};


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
        MacroCall,
        stage::include::{IncludeStage, IncludeToken},
        LexerToken,
        LexerFile
    },
    expression::ExpressionResult,
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

type Optimizations = Vec<(Location, String)>;


#[derive(Debug, Clone)]
pub struct MacroExpansion {
    name: String,
    location: Location,
    children: usize,
    size: usize
}

#[derive(Debug, Clone)]
pub struct GBCSymbol {
    pub kind: SymbolKind,
    pub is_global: bool,
    pub in_macro: bool,
    pub location: Location,
    pub name: String,
    pub value: String,
    pub width: usize,
    pub result: Option<ExpressionResult>,
    pub children: Vec<GBCSymbol>,
    pub references: Vec<Location>
}

impl GBCSymbol {
    fn typ(&self) -> &str {
        match self.kind {
            SymbolKind::Constant => "constant",
            SymbolKind::Constructor => "macro",
            SymbolKind::Function => "label",
            SymbolKind::Variable => "variable",
            SymbolKind::Field => "label",
            _ => "entry"
        }
    }

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
            detail: Some(match self.kind {
                SymbolKind::Constant => format!("= {}", self.value),
                SymbolKind::Constructor => format!("MACRO{}", self.value),
                SymbolKind::Function => format!("@ {}", self.value),
                SymbolKind::Variable => format!("@ {}", self.value),
                SymbolKind::Field => format!("@ {}", self.value),
                _ => self.value
            }),
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
    symbols: Arc<Mutex<Option<(Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations)>>>,
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
}

impl State {
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
        let (symbols, expansions, optimizations) = self.get_symbols(current_file).await;
        let mut hints: Vec<InlayHint> = symbols.into_iter().filter(|symbol| {
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
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Constant => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Constructor => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("MACRO{} ({} calls)", symbol.value, symbol.references.len()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Function => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Variable => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
                range: Range {
                    start: symbol.location.range.start,
                    end: symbol.location.range.start
                }
            }),
            SymbolKind::Field => Some(InlayHint {
                kind: InlayKind::TypeHint,
                label: format!("{} ({} refs)", symbol.value, symbol.references.len()),
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
                    format!("{} byte (expanded)", exp.size)

                } else if exp.children == 1 {
                    format!("{} byte ({} sub-expansion)", exp.size, exp.children)

                } else {
                    format!("{} byte ({} sub-expansions)", exp.size, exp.children)
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
                // TODO show expanded tokens, need to record expanded entries (already needed above)
                // and then be able to to_string() them all again into source code
                SymbolKind::Constructor => {
                    Some(format!("MACRO {}{}", symbol.name, symbol.value))
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
        let uri = Url::from_file_path(&current_file).unwrap();
        if let Some((token, _)) = self.parse_token(current_file.clone(), line, col) {
            let symbol_name = token.inner().value.to_string();
            let symbols = self.get_symbols(current_file).await.0;
            symbols.into_iter().find(|symbol| {
                // For child labels only search in the curent file
                symbol.name == symbol_name &&
                    (symbol.kind != SymbolKind::Method || symbol.location.uri == uri)
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

    async fn get_symbols(&self, workspace_path: PathBuf) -> (Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations) {
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
            symbols.clone().unwrap_or_else(|| (Vec::new(), Vec::new(), Vec::new()))

        } else {
            (Vec::new(), Vec::new(), Vec::new())
        }
    }

    fn resolve_reference(linker: &Linker, file_index: usize, start_index: usize, macro_call_id: Option<usize>, l: usize) -> Location {
        let context = linker.context();

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

    fn location_from_file_index(files: &[LexerFile], file_index: usize, start_index: usize, end_index: usize) -> Location {
        let file = &files[file_index];
        let (line, col) = file.get_line_and_col(start_index);
        let (eline, ecol) = file.get_line_and_col(end_index);
        Location {
            uri: Url::from_file_path(&file.path).unwrap(),
            range: Range {
                start: Position {
                    line: line as u32,
                    character: col as u32
                },
                end: Position {
                    line: if eline > line {
                        (eline + 1) as u32

                    } else {
                        eline as u32
                    },
                    character: ecol as u32
                }
            }
        }
    }

    fn parse_symbols(linker: &Linker) -> (Vec<GBCSymbol>, Vec<MacroExpansion>) {
        let mut symbols: Vec<GBCSymbol> = Vec::new();
        let context = linker.context();

        // Constants
        for (index, constant) in context.constants {
            let token = &constant.inner;
            let name = token.value.to_string();
            if let Some(result) = context.constant_values.get(index) {
                let references = context.constant_usage.get(index).map(|refs| {
                    refs.iter().map(|(file_index, start_index, macro_call_id)| {
                        Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                    }).collect()

                }).unwrap_or_else(Vec::new);
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constant,
                    is_global: index.1.is_none(),
                    in_macro: token.macro_call_id.is_some(),
                    location: Self::location_from_file_index(context.files, constant.inner.file_index, constant.inner.start_index, constant.inner.end_index),
                    name,
                    width: 0,
                    result: Some(result.clone()),
                    value: result.to_string(),
                    children: Vec::new(),
                    references
                });
            }
        }

        // Sections
        for section in &linker.sections {
            symbols.push(GBCSymbol {
                kind: SymbolKind::Namespace,
                is_global: false,
                in_macro: section.inner.macro_call_id.is_some(),
                location: Self::location_from_file_index(context.files, section.inner.file_index, section.inner.start_index, section.inner.end_index),
                name: section.name.to_string(),
                width: 0,
                result: None,
                value: format!("{}[${:0>4X}-${:0>4X}][{}]", section.segment, section.start_address, section.end_address, section.bank),
                children: Vec::new(),
                references: Vec::new()
            });
        }

        // Macros
        for def in context.macro_defs {
            // ignore callable labels and only include normal macros
            if !def.is_label {
                // TODO also list builtin macros
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constructor,
                    is_global: def.is_exported,
                    in_macro: false,
                    location: Self::location_from_file_index(context.files, def.name.file_index, def.name.start_index, def.name.end_index),
                    name:  def.name.value.to_string(),
                    width: 0,
                    result: None,
                    value: format!("({})", def.parameters.iter().map(|(_, name)| {
                        format!("@{}", name.value)

                    }).collect::<Vec<String>>().join(", ")),
                    children: Vec::new(),
                    references: context.macro_calls.iter().filter(|call| {
                        call.name().value == def.name.value

                    }).map(|call| {
                        let name = call.name();
                        Self::resolve_reference(&linker, name.file_index, name.start_index, name.macro_call_id, name.value.as_str().len())

                    }).collect()
                });
            }
        }

        let mut macro_call_sizes: HashMap<usize, usize> = HashMap::with_capacity(32);
        let sections = linker.section_entries();
        for entry in sections {

            // Variables / Labels
            let mut entries = entry.iter().peekable();
            while let Some(entry) = entries.next() {

                let file_index = entry.inner.file_index;
                let entry_name = &entry.inner.value;
                let start_index = entry.inner.start_index;
                let mut end_index = entry.inner.end_index;

                if let EntryData::Label { .. } = &entry.data {

                    let mut children = Vec::new();
                    let mut kind = SymbolKind::Function;
                    let mut data_size = 0;
                    while let Some(SectionEntry { size, data, inner, .. }) = entries.peek() {

                        // Encountered next parent label
                        if let EntryData::Label { is_local: false, .. } = data {
                            break;
                        }

                        // Child labels
                        if let EntryData::Label { is_local: true, name, .. } = data {
                            let mut label = GBCSymbol {
                                kind: SymbolKind::Method,
                                is_global: false,
                                in_macro: inner.macro_call_id.is_some(),
                                location: Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index),
                                name: format!(".{}", name),
                                width: 0,
                                result: None,
                                value: "".to_string(),
                                children: Vec::new(),
                                references: Vec::new()
                            };
                            children.push(label.clone());
                            label.name = name.clone();

                            // Copy for goto definition
                            symbols.push(label);
                            entries.next();

                        // Collect Data Storage
                        } else if let EntryData::Data { bytes: Some(b), ..  } = data {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += b.len();
                            }
                            kind = SymbolKind::Field;
                            data_size += b.len();
                            entries.next();

                        // Variables
                        } else if let EntryData::Data { bytes: None, .. } = data {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += *size;
                            }
                            data_size = *size;
                            kind = SymbolKind::Variable;
                            entries.next();
                            break;

                        // Any other entry like an instruction
                        } else {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += *size;
                            }
                            data_size += *size;
                            entries.next();
                        }

                        // Update range of label body
                        if inner.macro_call_id.is_none() {
                            end_index = end_index.max(inner.end_index);
                        }

                    }

                    // Find matching label in context
                    for ((symbol, label_id, is_global), token) in context.labels {
                        if token.file_index == file_index && symbol == entry_name {
                            let name = symbol.to_string();
                            let address = context.label_addresses.get(label_id).unwrap_or(&0);

                            // TODO record re-defitions of default constants as a reference
                            let references = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().map(|(file_index, start_index, macro_call_id)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            // TODO show signature for callable labels, match by finding the macro with the same inner token
                            symbols.push(GBCSymbol {
                                kind,
                                is_global: *is_global,
                                in_macro: token.macro_call_id.is_some(),
                                location: Self::location_from_file_index(context.files, file_index, start_index, end_index),
                                name,
                                width: data_size,
                                result: None,
                                value: if data_size == 0 {
                                    format!("${:0>4X}", address)

                                } else {
                                    format!("${:0>4X} ({} byte)", address, data_size)
                                },
                                children,
                                references
                            });
                            break;
                        }
                    }
                }
            }
        }

        fn macro_child_size(calls: &[MacroCall], call_sizes: &HashMap<usize, usize>, parent_id: usize, size: &mut usize, children: &mut usize) {
            // Find all calls within the given parent
            for c in calls {
                if c.parent_id() == Some(parent_id) {
                    *children += 1;
                    *size += *call_sizes.get(&c.id()).unwrap_or(&0);
                    // Now recsurse to also find this macros children
                    macro_child_size(calls, call_sizes, c.id(), size, children);
                }
            }
        }

        // Macro Calls for inlay hints
        let mut macro_expansions = Vec::new();
        for call in context.macro_calls {
            if call.is_expansion() {
                let name = call.name();
                let mut size = *macro_call_sizes.get(&call.id()).unwrap_or(&0);
                let mut children = 0;
                macro_child_size(context.macro_calls, &macro_call_sizes, call.id(), &mut size, &mut children);
                macro_expansions.push(MacroExpansion {
                    name: name.value.to_string(),
                    location: Self::location_from_file_index(context.files, name.file_index, name.start_index, name.end_index),
                    children,
                    size
                });
            }
        }

        // Remove any duplicate symbols
        let mut symbol_locations = HashSet::new();
        let mut unique_symbols = Vec::new();
        let mut dupes = 0;
        for s in symbols {
            let loc = (
                s.location.uri.path().to_string(),
                s.location.range.start.line,
                s.location.range.start.character,
                s.location.range.end.line,
                s.location.range.end.character
            );
            if !symbol_locations.contains(&loc) {
                symbol_locations.insert(loc);
                unique_symbols.push(s);

            } else {
                dupes += 1;
            }
        }
        log::info!(&format!("Removed {} duplicate symbols", dupes));

        // Sort by Name
        unique_symbols.sort_by(|a, b| {
            a.name.cmp(&b.name)
        });

        // Ignore everything outside of gbc files
        (unique_symbols.into_iter().filter(|s| {
            s.location.uri.path().ends_with(".gbc")

        }).collect(), macro_expansions)
    }

    fn parse_optimizations(linker: &Linker) -> Optimizations {
        let context = linker.context();
        context.optimizations.into_iter().filter(|(inner, _)| {
            inner.macro_call_id.is_none()

        }).map(|(inner, note)| {
            (
                Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index),
                note.clone()
            )

        }).collect()
    }

    fn generate_lints(linker: &Linker, symbols: &[GBCSymbol]) -> Vec<(Url, Diagnostic)> {
        let mut lints = Vec::new();
        let mut constants = Vec::new();
        for symbol in symbols {

            // Record constants for later use
            if symbol.kind == SymbolKind::Constant {
                constants.push(symbol);
            }

            // Ignore Sections, child labels, symbols inside macros, symbols prefixed with _ and symbols inside a
            // library folder
            if symbol.kind == SymbolKind::Namespace
                || symbol.kind == SymbolKind::Method
                || symbol.in_macro
                || symbol.name.starts_with('_')
                || symbol.location.uri.path().contains("lib") {
                continue;
            }

            // Unused Symbols
            if symbol.references.is_empty() {
                lints.push((symbol.location.uri.clone(), Diagnostic {
                    message: format!("unused {}", symbol.typ()),
                    range: symbol.location.range.clone(),
                    severity: Some(DiagnosticSeverity::Warning),
                    .. Diagnostic::default()
                }));

            // Symbols unused outside their file
            } else if symbol.is_global {
                let outer_refs = symbol.references.iter().filter(|r| r.uri != symbol.location.uri).count();
                if outer_refs == 0 {
                    lints.push((symbol.location.uri.clone(), Diagnostic {
                        message: format!("{} never used outside current file", symbol.typ()),
                        range: symbol.location.range.clone(),
                        severity: Some(DiagnosticSeverity::Warning),
                        .. Diagnostic::default()
                    }));
                }
            }
        }

        // Recommend to replace magic number integers with matching constants
        let context = linker.context();
        for (_, (inner, value)) in context.integers {
            let location = Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index);

            // Ignore integers inside macros and inside of a library folder
            if inner.macro_call_id.is_some() || location.uri.path().contains("lib") {
                continue;
            }

            let matching_constants: Vec<&&GBCSymbol> = constants.iter().filter(|symbol|
                if let Some(ExpressionResult::Integer(v)) = symbol.result {
                    let p = v.abs() as usize;
                    if v == *value && !p.is_power_of_two() && (v > 8 || v < 0) && p != 0xFF && p != 0xFFFF && p % 10 != 0 {
                        // Always consider constants in the local file, for word values also consider global symbols
                        symbol.location.uri == location.uri || (p > 255 && symbol.is_global)

                    } else {
                        false
                    }

                } else {
                    false
                }

            ).collect();
            if !matching_constants.is_empty() {
                for symbol in matching_constants {
                    lints.push((location.uri.clone(), Diagnostic {
                        message: format!("`{}` has the same value", symbol.name),
                        range: location.range.clone(),
                        severity: Some(DiagnosticSeverity::Hint),
                        .. Diagnostic::default()
                    }));
                }
            }
        }
        lints
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

    async fn link(
        client: Arc<Client>,
        link_id: NumberOrString,
        workspace_path: PathBuf,
        outer_symbols: Arc<Mutex<Option<(Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations)>>>,
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

        // Run linker
        let mut errors: Vec<(Url, Diagnostic)> = Vec::new();
        let mut lints: Vec<(Url, Diagnostic)> = Vec::new();
        match compiler.create_linker(&mut logger, &mut reader, main_file) {
            Ok(linker) => {
                log::info!(&format!("Linked in {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Linking complete").await;

                // Generate and store new symbols
                if let Ok(mut symbols) = outer_symbols.lock() {
                    let (s, m) = Self::parse_symbols(&linker);
                    lints.append(&mut Self::generate_lints(&linker, &s));
                    let optimizations = Self::parse_optimizations(&linker);
                    log::info!(&format!("{} symbol(s)", s.len()));
                    *symbols = Some((s, m, optimizations));
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
        for (uri, lint) in lints {
            diagnostics.entry(uri).or_insert_with(Vec::new).push(lint);
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
