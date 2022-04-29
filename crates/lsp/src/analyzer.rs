// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;


// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use tower_lsp::Client;
use tower_lsp::lsp_types::{DocumentSymbol, Range};
use tower_lsp::lsp_types::{SymbolInformation, SymbolKind, Url, Location, CompletionItem, CompletionItemKind};


// Internal Dependencies ------------------------------------------------------
use compiler::lexer::LexerToken;
use compiler::linker::{AnalysisSymbol, AnalysisMacroExpansion, AnalysisHint};
use project::{ProjectConfig, ProjectReader};

use crate::{
    state::State,
    parser::Parser,
    types::{InlayHint, InlayKind, Runnable, RunnableArgs}
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
    pub fn runnables(&self) -> Vec<Runnable> {
        let workspace_path = self.state.workspace_path().clone();
        let workspace_root = workspace_path.clone().map(|p| p.to_string_lossy().to_string());
        let mut runnables = vec![
            Runnable {
                label: "Build Release ROM".to_string(),
                kind: "gbc".to_string(),
                args: RunnableArgs {
                    workspace_root: workspace_root.clone(),
                    gbc_args: vec!["release".to_string()]
                }
            },
            Runnable {
                label: "Build Debug ROM".to_string(),
                kind: "gbc".to_string(),
                args: RunnableArgs {
                    workspace_root: workspace_root.clone(),
                    gbc_args: vec!["debug".to_string()]
                }
            }
        ];
        if let Some(workspace_path) = workspace_path {
            if let Ok(config) = ProjectConfig::try_load(&ProjectReader::from_absolute(workspace_path.clone())) {
                for name in config.runner.into_keys() {
                    runnables.push(Runnable {
                        label: format!("Run via \"{}\"", name),
                        kind: "gbc".to_string(),
                        args: RunnableArgs {
                            workspace_root: workspace_root.clone(),
                            gbc_args: vec!["run".to_string(), name]
                        }
                    });
                }
            }
        }
        runnables
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

    pub async fn completions(&self, current_file: PathBuf, line: usize, _col: usize) -> Vec<CompletionItem> {
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
            SymbolKind::METHOD if line > symbol.line_range.0 && line <= symbol.line_range.1 => Some(CompletionItem {
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

                    }, info, ""))
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

        let mut inlay_hints: Vec<InlayHint> = symbols.into_iter().flat_map(|symbol| match symbol.kind {
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
                Some(InlayHint {
                    kind: InlayKind::TypeHint,
                    label: format!("{} ({})", symbol.value, symbol.info()),
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
                label: format!("{} (opt)", hint.detail),
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

