// STD Dependencies -----------------------------------------------------------
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::MutexGuard;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicUsize;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use tower_lsp::Client;
use tower_lsp::lsp_types::{
    Diagnostic,
    Url, NumberOrString,
    PublishDiagnosticsParams,
    ProgressParams,
    ProgressParamsValue,
    WorkDoneProgress,
    WorkDoneProgressBegin,
    WorkDoneProgressEnd,
    WorkDoneProgressCreateParams,
};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::notification::{Progress, PublishDiagnostics};


// Internal Dependencies ------------------------------------------------------
use compiler::lexer::{stage::include::IncludeToken, LexerFile};
use compiler::linker::{AnalysisSymbol, AnalysisHint, AnalysisMacroExpansion};
use crate::types::{ServerStatusParams, ServerStatusNotification};


// Types ----------------------------------------------------------------------
type DocumentMap = HashMap<String, String>;
type TokenMap = HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>;
type SymbolData = (Vec<AnalysisSymbol>, Vec<AnalysisMacroExpansion>, Vec<AnalysisHint>);
type DiagnosticsMap = HashMap<Url, Vec<Diagnostic>>;
type Error = (Url, usize, usize);


// Analyzer State -------------------------------------------------------------
#[derive(Clone)]
pub struct State {
    client: Arc<Client>,
    progress_id: Arc<AtomicUsize>,

    // Diagnostics
    error: Arc<Mutex<Option<Error>>>,
    diagnostics: Arc<Mutex<DiagnosticsMap>>,

    // Symbol Data
    symbols: Arc<Mutex<Option<SymbolData>>>,

    // Document Data
    workspace_path: Arc<Mutex<Option<PathBuf>>>,
    documents: Arc<Mutex<DocumentMap>>,
    tokens: Arc<Mutex<TokenMap>>
}

impl State {
    pub fn from_client(client: Arc<Client>) -> Self {
        Self {
            client,
            progress_id: Arc::new(AtomicUsize::new(0)),

            // Diagnostics
            error: Arc::new(Mutex::new(None)),
            diagnostics: Arc::new(Mutex::new(HashMap::new())),

            // Symbol Data
            symbols: Arc::new(Mutex::new(None)),

            // Document Data
            workspace_path: Arc::new(Mutex::new(None)),
            documents: Arc::new(Mutex::new(HashMap::new())),
            tokens: Arc::new(Mutex::new(HashMap::new()))
        }
    }

    pub fn set_error(&self, err: Option<Error>) {
        if let Ok(mut error) = self.error.lock() {
            *error = err;
        }
    }

    pub fn set_workspace_path(&self, path: PathBuf) {
        if let Ok(mut ws) = self.workspace_path.lock() {
            *ws = Some(path);
        }
    }

    pub fn set_symbols(&self, symbols: SymbolData) {
        if let Ok(mut data) = self.symbols.lock() {
            *data = Some(symbols);
        }
    }

    pub fn error(&self) -> MutexGuard<Option<Error>> {
        self.error.lock().expect("Error Lock failed")
    }

    pub fn workspace_path(&self) -> MutexGuard<Option<PathBuf>> {
        self.workspace_path.lock().expect("WorkspacePath Lock failed")
    }

    pub fn diagnostics(&self) -> MutexGuard<DiagnosticsMap> {
        self.diagnostics.lock().expect("Diagnostics Lock failed")
    }

    pub fn documents(&self) -> MutexGuard<DocumentMap> {
        self.documents.lock().expect("Documents Lock failed")
    }

    pub fn tokens(&self) -> MutexGuard<TokenMap> {
        self.tokens.lock().expect("Tokens Lock failed")
    }

    pub fn has_symbols(&self) -> bool {
        self.symbols.lock().expect("Symbols Lock failed").is_some()
    }

    pub fn symbols_all(&self) -> MutexGuard<Option<SymbolData>> {
        self.symbols.lock().expect("Symbols Lock failed")
    }

    pub fn symbols_cloned(&self) -> Option<Vec<AnalysisSymbol>> {
        self.symbols.lock().expect("Symbols Lock failed").as_ref().map(|s| s.0.clone())
    }

    pub fn client(&self) -> Arc<Client> {
        self.client.clone()
    }
}

impl State {
    pub async fn update_server_status<S: Into<String>>(&self, message: S) {
        self.client.send_notification::<ServerStatusNotification>(ServerStatusParams {
            quiescent: true,
            message: Some(message.into())

        }).await;
    }

    pub async fn start_progress<S: Display>(&self, title: S, message: S) -> Option<NumberOrString> {
        let progress_id = self.progress_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let token = NumberOrString::Number(progress_id as i32);
        if self.client.send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: token.clone()

        }).await.is_ok() {
            self.client.send_notification::<Progress>(ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(WorkDoneProgressBegin {
                    title: title.to_string(),
                    cancellable: Some(false),
                    message: Some(message.to_string()),
                    percentage: None
                }))
            }).await;
            Some(token)

        } else {
            None
        }
    }

    pub async fn end_progress<S: Display>(&self, token: Option<NumberOrString>, message: S) {
        if let Some(token) = token {
            self.client.send_notification::<Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(message.to_string())
                }))
            }).await;
        }
    }

    pub async fn publish_diagnostics(&self) {
        // Remove any Information diagnostics
        for (_, diagnostics) in self.diagnostics().iter_mut() {
            *diagnostics = diagnostics.iter().cloned().filter(|d| {
                !d.message.starts_with("Debugger")

            }).collect();
        }

        // Send diagnostics for all files
        let diagnostics = self.diagnostics().clone();
        for (uri, diagnostics) in &diagnostics {
            self.client.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None

            }).await;
        }
    }
}

