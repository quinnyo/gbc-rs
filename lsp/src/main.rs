// External Dependencies ------------------------------------------------------
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use serde::{Serialize, Deserialize};
use serde_json::{Map, Value};


// Internal Dependencies ------------------------------------------------------
use compiler::linker::Completion;


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

// Backend Implementation -----------------------------------------------------
struct Backend {
    client: Client,
    state: State
}

impl Backend {
    //#[rpc(name = "gbc-analyzer/inlayHints")]
    //async fn inlay_hints(&self, params: InlayHintsParams) -> Result<Vec<InlayHint>> {
    //    let file = params.text_document.uri.path();
    //    log::info!(&format!("Inlay Hints {}", file));
    //    Ok(Vec::new())
    //}

    fn do_completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        log::info!(&format!("Complete {} {}:{}", file, pos.position.line, pos.position.character));

        if let Ok(completions) = self.state.completions(file, pos.position.line as usize, pos.position.character as usize) {
            Ok(Some(CompletionResponse::Array(completions.into_iter().map(|item| {
                match item {
                    Completion::GlobalLabel { name, info } => CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::Function),
                        detail: info,
                        documentation: None,
                        deprecated: None,
                        preselect: None,
                        sort_text: None,
                        filter_text: None,
                        insert_text: None,
                        insert_text_format: None,
                        insert_text_mode: None,
                        text_edit: None,
                        additional_text_edits: None,
                        command: None,
                        commit_characters: None,
                        data: None,
                        tags: None
                    },
                    Completion::LocalLabel { name } => CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::Field),
                        detail: None,
                        documentation: None,
                        deprecated: None,
                        preselect: None,
                        sort_text: None,
                        filter_text: None,
                        insert_text: None,
                        insert_text_format: None,
                        insert_text_mode: None,
                        text_edit: None,
                        additional_text_edits: None,
                        command: None,
                        commit_characters: None,
                        data: None,
                        tags: None
                    },
                    Completion::Constant { name, info } => CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::Variable),
                        detail: info,
                        documentation: None,
                        deprecated: None,
                        preselect: None,
                        sort_text: None,
                        filter_text: None,
                        insert_text: None,
                        insert_text_format: None,
                        insert_text_mode: None,
                        text_edit: None,
                        additional_text_edits: None,
                        command: None,
                        commit_characters: None,
                        data: None,
                        tags: None
                    },
                }
            }).collect())))

        } else {
            Ok(None)
        }
    }

    fn do_hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        log::info!(&format!("Hover {} {}:{}", file, pos.position.line, pos.position.character));

        if let Ok(Some((lookup, range))) = self.state.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::from_markdown(
                    format!("# {}\n\n{}\n\nDefined in {}:{}:{}", lookup.name, lookup.description, lookup.path, lookup.start.0 + 1, lookup.start.1 + 1)
                )),
                range: Some(range)
            }))

        } else {
            Ok(None)
        }
    }

    fn do_goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        log::info!(&format!("Goto Definition {} {}:{}", file, pos.position.line, pos.position.character));

        if let Ok(Some((lookup, _))) = self.state.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
            Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: Url::from_file_path(lookup.path).unwrap(),
                range: Range {
                    start: Position {
                        line: lookup.start.0 as u32,
                        character: lookup.start.1 as u32
                    },
                    end: Position {
                        line: lookup.end.0 as u32,
                        character: lookup.end.1 as u32
                    }
                }
            })))

        } else {
            Ok(None)
        }
    }

    fn do_references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        log::info!(&format!("References {} {}:{}", file, pos.position.line, pos.position.character));

        if let Ok(Some((lookup, _))) = self.state.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
            Ok(Some(lookup.references.into_iter().map(|(path, line, col, eline, ecol)| {
                // TODO check if we can actual expression entry range instead of the parent
                // instruction entry (check code in section.rs and the xyz.start_index)
                Location {
                    uri: Url::from_file_path(path).unwrap(),
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
                }
            }).collect()))

        } else {
            Ok(None)
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
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

    async fn initialized(&self, _: InitializedParams) {
        self.client.send_custom_notification::<ServerStatusNotification>(ServerStatusParams {
            quiescent: true,
            message: Some("Ready".to_string())

            }).await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!(&format!("Opened {}", params.text_document.uri.path()));
        self.state.open_document(params.text_document.uri.path(), &params.text_document.text);
        /*
        self.client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: NumberOrString::Number(0)

            }).await;
        self.client
            .show_progress_begin(NumberOrString::Number(0), "Initializing")
            .await;
        self.client
            .show_progress_end(NumberOrString::Number(0), "Initialized")
            .await;*/
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

    // TODO symbols()

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.do_hover(params)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.do_completion(params)
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        self.do_goto_definition(params)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        self.do_references(params)
    }

    async fn experimental_any(&self, params: Value) -> Result<Option<Value>> {
        if let Value::Object(map) = params {
            if let Some(Value::Object(text_document)) = map.get("textDocument") {
                if let Some(Value::String(path)) = text_document.get("uri") {
                    log::info!(&format!("ExperimentalAny {}", Url::parse(path).unwrap().path()));

                    let mut hints = Vec::new();
                    hints.push(InlayHint {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0
                            },
                            end: Position {
                                line: 0,
                                character: 1
                            }
                        },
                        kind: InlayKind::TypeHint,
                        label: "XYZ".to_string()
                    });

                    let hints = serde_json::to_value(&hints).unwrap();
                    return Ok(Some(hints))
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

    let (service, messages) = LspService::new(|client| Backend {
        client,
        state: State::new()
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}

