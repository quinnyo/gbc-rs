// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};
use serde::Deserialize;
use serde_json::Value;


// Modules --------------------------------------------------------------------
mod analyzer;
mod emulator;
mod parser;
mod types;
use self::analyzer::Analyzer;
use self::emulator::EmulatorCommand;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExperimentalParams {
    command: String,
    #[serde(default)]
    range: Range,
    text_document: TextDocumentIdentifier
}

// Backend Implementation -----------------------------------------------------
struct Backend {
    analyzer: Analyzer
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(folders) = params.workspace_folders {
            if let Some(folder) = folders.first() {
                self.analyzer.set_workspace_path(PathBuf::from(folder.uri.path())).await;
            }

        } else if let Some(root_uri) = params.root_uri {
            self.analyzer.set_workspace_path(PathBuf::from(root_uri.path())).await;
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
        self.analyzer.initialize().await
    }

    async fn shutdown(&self) -> Result<()> {
        self.analyzer.shutdown().await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!(&format!("Opened {}", params.text_document.uri.path()));
        self.analyzer.open_document(params.text_document.uri.path(), &params.text_document.text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::info!(&format!("Changed {}", params.text_document.uri.path()));
        if let Some(change) = params.content_changes.first() {
            self.analyzer.change_document(params.text_document.uri.path(), &change.text);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::info!(&format!("Saved {}", params.text_document.uri.path()));
        self.analyzer.save_document(params.text_document.uri.path());
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        log::info!(&format!("Closed {}", params.text_document.uri.path()));
        self.analyzer.close_document(params.text_document.uri.path());
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        if let Some((detail, location)) = self.analyzer.hover(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
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
        let completions = self.analyzer.completions(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await;
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        if let Some(symbol) = self.analyzer.symbol(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            Ok(Some(GotoDefinitionResponse::Scalar(symbol.location)))

        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        if let Some(mut symbol) = self.analyzer.symbol(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            let mut references = symbol.references;
            references.append(&mut symbol.calls);
            references.append(&mut symbol.reads);
            references.append(&mut symbol.writes);
            references.push(symbol.location);
            Ok(Some(references))

        } else {
            Ok(None)
        }
    }

    async fn symbol(&self, _params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
        let symbols = self.analyzer.workspace_symbols().await;
        Ok(Some(symbols))
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let file = params.text_document.uri.path();
        let symbols = self.analyzer.document_symbols(PathBuf::from(file)).await;
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn experimental_any(&self, params: Value) -> Result<Option<Value>> {
        if let Ok(params) = serde_json::from_value::<ExperimentalParams>(params) {
            match params.command.as_str() {
                "view/inlay_hints" => {
                    let hints = self.analyzer.inlay_hints(PathBuf::from(params.text_document.uri.path())).await;
                    return Ok(Some(serde_json::to_value(&hints).unwrap()))
                },
                "debugger/toggle_breakpoint" => {
                    self.analyzer.toggle_breakpoint(Location {
                        uri: params.text_document.uri,
                        range: params.range

                    }).await;
                },
                "debugger/step" => {
                    self.analyzer.emulator_command(EmulatorCommand::DebuggerStep).await;
                },
                "debugger/step_over" => {
                    self.analyzer.emulator_command(EmulatorCommand::DebuggerStepOver).await;
                },
                "debugger/finish" => {
                    self.analyzer.emulator_command(EmulatorCommand::DebuggerFinish).await;
                },
                "debugger/continue" => {
                    self.analyzer.emulator_command(EmulatorCommand::DebuggerContinue).await;
                },
                _ => {}
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
            analyzer: Analyzer::new(Arc::new(client))
        }
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}

