// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};
use serde::Deserialize;
use serde_json::Value;
use gbd::Model;


// Modules --------------------------------------------------------------------
mod analyzer;
mod emulator;
mod parser;
mod types;
mod state;
use self::analyzer::Analyzer;


// Structs --------------------------------------------------------------------
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommandDocumentParams {
    #[serde(default)]
    range: Range,
    text_document: TextDocumentIdentifier
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WriteMemoryParams {
    address: String,
    value: String
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelParams {
    model: Model
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
                    TextDocumentSyncKind::FULL
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![
                        "project/runnables".to_string(),
                        "view/inlay_hints".to_string(),
                        "debugger/toggle_breakpoint".to_string(),
                        "debugger/step".to_string(),
                        "debugger/step_over".to_string(),
                        "debugger/finish".to_string(),
                        "debugger/continue".to_string(),
                        "debugger/undo".to_string(),
                        "emulator/start".to_string(),
                        "emulator/write_memory".to_string(),
                        "emulator/stop".to_string(),
                        "build/rom".to_string()
                    ],
                    work_done_progress_options: Default::default(),
                }),
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
        self.analyzer.emulator_stop().await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!("{}", format!("Opened {}", params.text_document.uri.path()));
        self.analyzer.open_document(params.text_document.uri.path(), &params.text_document.text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::info!("{}", format!("Changed {}", params.text_document.uri.path()));
        if let Some(change) = params.content_changes.first() {
            self.analyzer.change_document(params.text_document.uri.path(), &change.text);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::info!("{}", format!("Saved {}", params.text_document.uri.path()));
        self.analyzer.save_document(params.text_document.uri.path());
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        log::info!("{}", format!("Closed {}", params.text_document.uri.path()));
        self.analyzer.close_document(params.text_document.uri.path());
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        if let Some((detail, location)) = self.analyzer.hover(PathBuf::from(file), pos.position.line as usize, pos.position.character as usize).await {
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "gbc".to_string(),
                    value: detail
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

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        match params.command.as_str() {
            "project/runnables" => {
                let runnables = self.analyzer.runnables();
                return Ok(Some(serde_json::to_value(&runnables).unwrap()))
            }
            "view/inlay_hints" => if let Some(Ok(params)) = params.arguments.first().map(|v| serde_json::from_value::<CommandDocumentParams>(v.clone())) {
                let hints = self.analyzer.inlay_hints(PathBuf::from(params.text_document.uri.path())).await;
                return Ok(Some(serde_json::to_value(&hints).unwrap()))
            },
            "debugger/toggle_breakpoint" => if let Some(Ok(params)) = params.arguments.first().map(|v| serde_json::from_value::<CommandDocumentParams>(v.clone())) {
                self.analyzer.toggle_breakpoint(Location {
                    uri: params.text_document.uri,
                    range: params.range

                }).await;
            },
            "emulator/start" => {
                let options = params.arguments.get(1).map(|v| serde_json::from_value::<ModelParams>(v.clone()));
                log::info!("Start params: {:?} -> {:?}", params.arguments, options);
                if let Some(Ok(params)) = options {
                    self.analyzer.emulator_start(Some(params.model)).await;

                } else {
                    self.analyzer.emulator_start(None).await;
                }
            },
            "emulator/stop" => {
                self.analyzer.emulator_stop().await;
            },
            "build/rom" => {
                self.analyzer.build_rom().await;
            },
            _ => {}
        }
        Ok(None)
    }
}

async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| {
        Backend {
            analyzer: Analyzer::new(Arc::new(client))
        }
    });
    Server::new(stdin, stdout, messages)
        .serve(service)
        .await;

    // Exit with force here so that emulator on the main thread also exits
    std::process::exit(0);
}

fn main() {
    env_logger::Builder::new().filter_module("lsp", log::LevelFilter::Info).init();

    // Workaround needed to make the built-in emulator UI run on the main thread
    // otherwise winit will panic due to compatability issues
    std::thread::spawn(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                run_server().await;
            });
    });
    emulator::Emulator::main_thread_loop();
}

