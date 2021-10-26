// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};
use serde::{Serialize, Deserialize};
use serde_json::Value;


// Modules --------------------------------------------------------------------
mod state;
use self::state::State;


// Types ----------------------------------------------------------------------
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InlayKind {
    TypeHint,
    ParameterHint,
    ChainingHint,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InlayHint {
    pub range: Range,
    pub kind: InlayKind,
    pub label: String,
}

// Backend Implementation -----------------------------------------------------
struct Backend {
    state: State
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(folders) = params.workspace_folders {
            if let Some(folder) = folders.first() {
                self.state.set_workspace_path(PathBuf::from(folder.uri.path()));
            }

        } else if let Some(root_uri) = params.root_uri {
            self.state.set_workspace_path(PathBuf::from(root_uri.path()));
        }
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.state.initialize().await
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!(&format!("Opened {}", params.text_document.uri.path()));
        self.state.open_document(params.text_document.uri.path(), &params.text_document.text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::info!(&format!("Changed {}", params.text_document.uri.path()));
        if let Some(change) = params.content_changes.first() {
            self.state.change_document(params.text_document.uri.path(), &change.text);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::info!(&format!("Saved {}", params.text_document.uri.path()));
        self.state.save_document(params.text_document.uri.path());
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        log::info!(&format!("Closed {}", params.text_document.uri.path()));
        self.state.close_document(params.text_document.uri.path());
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        if let Some((detail, location)) = self.state.hover(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "gbc".to_string(),
                    value: detail.to_string()
                })),
                range: Some(location.range)
            }))

        } else {
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        let completions = self.state.completions(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await;
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        if let Some(symbol) = self.state.symbol(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            Ok(Some(GotoDefinitionResponse::Scalar(symbol.location)))

        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        if let Some(symbol) = self.state.symbol(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            Ok(Some(symbol.references))

        } else {
            Ok(None)
        }
    }

    async fn symbol(&self, _params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
        let symbols = self.state.workspace_symbols().await;
        Ok(Some(symbols))
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let file = params.text_document.uri.path();
        let symbols = self.state.document_symbols(PathBuf::from(file)).await;
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn experimental_any(&self, params: Value) -> Result<Option<Value>> {
        if let Value::Object(map) = params {
            if let Some(Value::Object(text_document)) = map.get("textDocument") {
                if let Some(Value::String(path)) = text_document.get("uri") {
                    let url = Url::parse(path).unwrap();
                    let hints = self.state.inlay_hints(PathBuf::from(url.path())).await;
                    return Ok(Some(serde_json::to_value(&hints).unwrap()))
                }
            }
        }
        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| {
        Backend {
            state: State::new(Arc::new(client))
        }
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}

