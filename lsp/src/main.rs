// STD Dependencies -----------------------------------------------------------
use std::sync::Mutex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use file_io::Logger;
use compiler::linker::Completion;
use project::{ProjectConfig, ProjectReader};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};



// Internal Dependencies ------------------------------------------------------
use compiler::linker::Lookup;
use compiler::lexer::LexerToken;


#[derive(Debug, Default)]
struct State {
    documents: HashMap<String, String>
}

#[derive(Debug)]
struct Backend {
    client: Client,
    state: Mutex<RefCell<State>>
}

impl Backend {
    fn do_completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let pos = params.text_document_position;
        let file = pos.text_document.uri.path();
        log::info!(&format!("Complete {} {}:{}", file, pos.position.line, pos.position.character));

        // TODO cache if files remain unchanged
        let mut logger = Logger::new();
        logger.set_silent();
        let config = ProjectConfig::load(
            &mut logger,
            // TODO Overlay modified files from LSP
            &ProjectReader::from_absolute(PathBuf::from(file))
        );
        log::info!(&format!("Config {:?}", config));
        if let Ok(completions) = ProjectConfig::complete(&config, &mut logger, file) {
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
                //CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string(read))
            }).collect())))

        } else {
            Ok(None)
        }
    }

    fn lookup_symbol(&self, file: &str, line: usize, col: usize) -> Result<Option<(Lookup, Range)>> {
        // TODO cache if files remain unchanged
        let mut logger = Logger::new();
        logger.set_silent();
        let config = ProjectConfig::load(
            &mut logger,
            // TODO Overlay modified files from LSP
            &ProjectReader::from_absolute(PathBuf::from(file))
        );
        log::info!(&format!("Config {:?}", config));
        if let Ok(Some((token, file))) = ProjectConfig::tokenize(&config, &mut logger, file, line, col) {
            let token = token.inner();
            if let Ok(Some(lookup)) = ProjectConfig::lookup(&config, &mut logger, token.value.to_string()) {
                let (sl, sc) = file.get_line_and_col(token.start_index);
                let (el, ec) = file.get_line_and_col(token.end_index);
                let range = Range {
                    start: Position {
                        line: sl as u32,
                        character: sc as u32
                    },
                    end: Position {
                        line: el as u32,
                        character: ec as u32
                    }
                };
                return Ok(Some((lookup, range)))
            }
        }
        Ok(None)
    }

    fn do_hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos = params.text_document_position_params;
        let file = pos.text_document.uri.path();
        log::info!(&format!("Hover {} {}:{}", file, pos.position.line, pos.position.character));

        if let Ok(Some((lookup, range))) = self.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
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

        if let Ok(Some((lookup, _))) = self.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
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

        if let Ok(Some((lookup, _))) = self.lookup_symbol(file, pos.position.line as usize, pos.position.character as usize) {
            Ok(Some(lookup.references.into_iter().map(|(path, line, col, eline, ecol)| {
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
        self.client
            .log_message(MessageType::Info, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        log::info!(&format!("Opened {}", params.text_document.uri.path()));
        if let Ok(state) = self.state.lock() {
            let mut state = state.borrow_mut();
            state.documents.insert(params.text_document.uri.path().to_string(), params.text_document.text);
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        log::info!(&format!("Changed {}", params.text_document.uri.path()));
        if let Ok(state) = self.state.lock() {
            let mut state = state.borrow_mut();
            if let Some(change) = params.content_changes.first() {
                state.documents.insert(params.text_document.uri.path().to_string(), change.text.clone());
            }
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        log::info!(&format!("Saved {}", params.text_document.uri.path()));
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        log::info!(&format!("Closed {}", params.text_document.uri.path()));
        if let Ok(state) = self.state.lock() {
            let mut state = state.borrow_mut();
            state.documents.remove(params.text_document.uri.path());
        }
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        self.do_goto_definition(params)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.do_hover(params)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        self.do_references(params)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.do_completion(params)
    }
}
#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend {
        client,
        state: Mutex::new(RefCell::new(State::default()))
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
