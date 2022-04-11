// STD Dependencies -----------------------------------------------------------
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::MutexGuard;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicUsize;
use std::sync::mpsc::Sender;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use tower_lsp::Client;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity,
    Url, Location, NumberOrString,
    PublishDiagnosticsParams,
    ProgressParams,
    ProgressParamsValue,
    WorkDoneProgress,
    WorkDoneProgressBegin,
    WorkDoneProgressReport,
    WorkDoneProgressEnd,
    WorkDoneProgressCreateParams,
};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::notification::{Progress, PublishDiagnostics};
use gbd::{EmulatorCommand, EmulatorStatus};


// Internal Dependencies ------------------------------------------------------
use compiler::lexer::{stage::include::IncludeToken, LexerFile};
use crate::{
    emulator::Emulator,
    types::{
        ServerStatusParams, ServerStatusNotification,
        InlayHintsNotification, InlayHintsParams,
        GBCSymbol, MacroExpansion, Optimizations
    }
};


// Types ----------------------------------------------------------------------
type DocumentMap = HashMap<String, String>;
type TokenMap = HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>;
type SymbolData = (Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations);
type DiagnosticsMap = HashMap<Url, Vec<Diagnostic>>;
type AddressesMap = HashMap<usize, Location>;
type Error = (Url, usize, usize);
type ResultMap = HashMap<u16, u8>;


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
    tokens: Arc<Mutex<TokenMap>>,

    // Address<->Location Lookup
    addresses: Arc<Mutex<AddressesMap>>,

    // Emulator
    emulator: Arc<Mutex<Option<Emulator>>>,
    status: Arc<Mutex<Option<EmulatorStatus>>>,
    results: Arc<Mutex<ResultMap>>,
    commands: Arc<Mutex<Option<Sender<EmulatorCommand>>>>
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
            tokens: Arc::new(Mutex::new(HashMap::new())),

            // Address<->Location Lookup
            addresses: Arc::new(Mutex::new(HashMap::new())),

            // Emulator
            emulator: Arc::new(Mutex::new(None)),
            status: Arc::new(Mutex::new(None)),
            results: Arc::new(Mutex::new(HashMap::new())),
            commands: Arc::new(Mutex::new(None)),
        }
    }

    pub fn send_command(&self, command: EmulatorCommand) {
        if let Ok(commands) = self.commands.lock() {
            if let Some(commands) = commands.as_ref() {
                commands.send(command).ok();
            }
        }
    }

    pub fn set_emulator(&self, p: Option<Emulator>) {
        if let Ok(mut process) = self.emulator.lock() {
            *process = p;
        }
    }

    pub fn set_status(&self, st: Option<EmulatorStatus>) {
        if let Ok(mut status) = self.status.lock() {
            *status = st;
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

    pub fn set_address_locations(&self, addresses: AddressesMap) {
        if let Ok(mut map) = self.addresses.lock() {
            *map = addresses;
        }
    }

    pub fn set_symbols(&self, symbols: SymbolData) {
        if let Ok(mut data) = self.symbols.lock() {
            *data = Some(symbols);
        }
    }

    pub fn set_command_sender(&self, s: Option<Sender<EmulatorCommand>>) {
        if let Ok(mut commands) = self.commands.lock() {
            *commands = s;
        }
    }

    pub fn emulator<'a>(&'a self) -> MutexGuard<'a, Option<Emulator>> {
        self.emulator.lock().expect("Emulator Lock failed")
    }

    pub fn status<'a>(&'a self) -> MutexGuard<'a, Option<EmulatorStatus>> {
        self.status.lock().expect("Status Lock failed")
    }

    pub fn error<'a>(&'a self) -> MutexGuard<'a, Option<Error>> {
        self.error.lock().expect("Error Lock failed")
    }

    pub fn workspace_path<'a>(&'a self) -> MutexGuard<'a, Option<PathBuf>> {
        self.workspace_path.lock().expect("WorkspacePath Lock failed")
    }

    pub fn diagnostics<'a>(&'a self) -> MutexGuard<'a, DiagnosticsMap> {
        self.diagnostics.lock().expect("Diagnostics Lock failed")
    }

    pub fn documents<'a>(&'a self) -> MutexGuard<'a, DocumentMap> {
        self.documents.lock().expect("Documents Lock failed")
    }

    pub fn address_locations<'a>(&'a self) -> MutexGuard<'a, AddressesMap> {
        self.addresses.lock().expect("Addresses Lock failed")
    }

    pub fn tokens<'a>(&'a self) -> MutexGuard<'a, TokenMap> {
        self.tokens.lock().expect("Tokens Lock failed")
    }

    pub fn results<'a>(&'a self) -> MutexGuard<'a, ResultMap> {
        self.results.lock().expect("Result Lock failed")
    }

    pub fn has_symbols(&self) -> bool {
        self.symbols.lock().expect("Symbols Lock failed").is_some()
    }

    pub fn symbols_all<'a>(&'a self) -> MutexGuard<'a, Option<SymbolData>> {
        self.symbols.lock().expect("Symbols Lock failed")
    }

    pub fn symbols_cloned(&self) -> Option<Vec<GBCSymbol>> {
        self.symbols.lock().expect("Symbols Lock failed").as_ref().map(|s| s.0.clone())
    }

    pub fn client(&self) -> Arc<Client> {
        self.client.clone()
    }
}

impl State {
    pub async fn update_server_status<S: Into<String>>(&self, message: S) {
        self.client.send_custom_notification::<ServerStatusNotification>(ServerStatusParams {
            quiescent: true,
            message: Some(message.into())

        }).await;
    }

    pub async fn start_progress<S: Display>(&self, title: S, message: S) -> Option<NumberOrString> {
        let progress_id = self.progress_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let token = NumberOrString::Number(progress_id as i32);
        if self.client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: token.clone()

        }).await.is_ok() {
            self.client.send_custom_notification::<Progress>(ProgressParams {
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

    pub async fn update_progress<S: Display>(&self, token: Option<NumberOrString>, message: S) {
        if let Some(token) = token {
            self.client.send_custom_notification::<Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(WorkDoneProgressReport {
                    cancellable: Some(false),
                    message: Some(message.to_string()),
                    percentage: None
                }))
            }).await;
        }
    }

    pub async fn end_progress<S: Display>(&self, token: Option<NumberOrString>, message: S) {
        if let Some(token) = token {
            self.client.send_custom_notification::<Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(message.to_string())
                }))
            }).await;
        }
    }

    pub async fn trigger_client_hints_refresh(&self) {
        self.client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
    }

    pub async fn publish_diagnostics(&self) {
        // Remove any Information diagnostics
        for (_, diagnostics) in self.diagnostics().iter_mut() {
            *diagnostics = diagnostics.iter().cloned().filter(|d| {
                !d.message.starts_with("Debugger")

            }).collect();
        }

        // Add debugger location
        if let Some(status) = self.status().clone() {
            for b in &status.breakpoints {
                if let Some(location) = self.address_locations().get(&(b.address as usize)) {
                    let info = Diagnostic {
                        message: format!("Debugger Breakpoint @ ${:0>4X}", b.address),
                        range: location.range,
                        severity: Some(DiagnosticSeverity::Warning),
                        .. Diagnostic::default()
                    };
                    self.diagnostics().entry(location.uri.clone()).or_insert_with(Vec::new).push(info);
                }
            }
            if status.halted {
                if let Some(location) = self.address_locations().get(&(status.pc as usize)) {
                    let info = Diagnostic {
                        message: "Debugger halted here".to_string(),
                        range: location.range,
                        severity: Some(DiagnosticSeverity::Hint),
                        .. Diagnostic::default()
                    };
                    self.diagnostics().entry(location.uri.clone()).or_insert_with(Vec::new).push(info);
                }
            }
        }

        // Send diagnostics for all files
        let diagnostics = self.diagnostics().clone();
        for (uri, diagnostics) in &diagnostics {
            self.client.send_custom_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None

            }).await;
        }
    }
}
