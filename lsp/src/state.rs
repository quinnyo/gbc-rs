// STD Dependencies -----------------------------------------------------------
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::MutexGuard;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicUsize;
use std::collections::{HashMap, VecDeque};


// External Dependencies ------------------------------------------------------
use tower_lsp::Client;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Location, NumberOrString, WorkDoneProgressCreateParams, PublishDiagnosticsParams, Url};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::notification::PublishDiagnostics;


// Internal Dependencies ------------------------------------------------------
use compiler::lexer::{stage::include::IncludeToken, LexerFile};
use crate::{
    emulator::{EmulatorCommand, EmulatorStatus},
    types::{
        InlayHintsNotification, InlayHintsParams,
        DebuggerOutlineNotification, DebuggerOutlineParams, DebuggerOutlineLocation,
        GBCSymbol, MacroExpansion, Optimizations
    }
};


// Types ----------------------------------------------------------------------
type DocumentMap = HashMap<String, String>;
type TokenMap = HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>;
type SymbolData = (Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations);
type DiagnosticsMap = HashMap<Url, Vec<Diagnostic>>;
type AddressesMap = HashMap<usize, Location>;
type LabelList = Vec<(u16, String, String, usize, usize)>;
type Error = (Url, usize, usize);
type CommandQueue = VecDeque<EmulatorCommand>;
type ResultMap = HashMap<u16, u8>;


// Analyzer State -------------------------------------------------------------
#[derive(Clone)]
pub struct State {
    client: Arc<Client>,
    progress_id: Arc<AtomicUsize>,

    // Diagnostics
    error: Arc<Mutex<Option<Error>>>,
    diagnostics: Arc<Mutex<DiagnosticsMap>>,

    // Parsed Symbols
    symbols: Arc<Mutex<Option<SymbolData>>>,

    // Document Data
    workspace_path: Arc<Mutex<Option<PathBuf>>>,
    documents: Arc<Mutex<DocumentMap>>,
    tokens: Arc<Mutex<TokenMap>>,

    // Address<->Location Lookup
    addresses: Arc<Mutex<AddressesMap>>,
    labels: Arc<Mutex<LabelList>>,

    // Emulator
    status: Arc<Mutex<Option<EmulatorStatus>>>,
    commands: Arc<Mutex<CommandQueue>>,
    results: Arc<Mutex<ResultMap>>,

}

impl State {
    pub fn from_client(client: Arc<Client>) -> Self {
        Self {
            client,
            progress_id: Arc::new(AtomicUsize::new(0)),

            error: Arc::new(Mutex::new(None)),
            diagnostics: Arc::new(Mutex::new(HashMap::new())),

            symbols: Arc::new(Mutex::new(None)),

            workspace_path: Arc::new(Mutex::new(None)),
            documents: Arc::new(Mutex::new(HashMap::new())),
            tokens: Arc::new(Mutex::new(HashMap::new())),

            addresses: Arc::new(Mutex::new(HashMap::new())),
            labels: Arc::new(Mutex::new(Vec::new())),

            status: Arc::new(Mutex::new(None)),
            commands: Arc::new(Mutex::new(VecDeque::new())),
            results: Arc::new(Mutex::new(HashMap::new())),
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

    pub fn set_addresses(&self, addresses: AddressesMap) {
        if let Ok(mut map) = self.addresses.lock() {
            *map = addresses;
        }
    }

    pub fn set_labels(&self, labels: LabelList) {
        if let Ok(mut list) = self.labels.lock() {
            *list = labels;
        }
    }

    pub fn set_symbols(&self, symbols: SymbolData) {
        if let Ok(mut data) = self.symbols.lock() {
            *data = Some(symbols);
        }
    }

    pub fn commands<'a>(&'a self) -> MutexGuard<'a, CommandQueue> {
        self.commands.lock().expect("Commands Lock failed")
    }

    pub fn results<'a>(&'a self) -> MutexGuard<'a, ResultMap> {
        self.results.lock().expect("Result Lock failed")
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

    pub fn addresses<'a>(&'a self) -> MutexGuard<'a, AddressesMap> {
        self.addresses.lock().expect("Addresses Lock failed")
    }

    pub fn tokens<'a>(&'a self) -> MutexGuard<'a, TokenMap> {
        self.tokens.lock().expect("Tokens Lock failed")
    }

    pub fn labels<'a>(&'a self) -> MutexGuard<'a, LabelList> {
        self.labels.lock().expect("Labels Lock failed")
    }

    pub fn symbols<'a>(&'a self) -> MutexGuard<'a, Option<SymbolData>> {
        self.symbols.lock().expect("Symbols Lock failed")
    }

    pub fn client(&self) -> Arc<Client> {
        self.client.clone()
    }
}

impl State {
    pub async fn start_progress<S: Display>(&self, title: S, message: S) -> Option<NumberOrString> {
        let progress_id = self.progress_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let token = NumberOrString::Number(progress_id as i32);
        if self.client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: token.clone()

        }).await.is_ok() {
            self.client.show_progress_begin(token.clone(), title, message).await;
            Some(token)

        } else {
            None
        }
    }

    pub async fn update_progress<S: Display>(&self, token: Option<NumberOrString>, message: S) {
        if let Some(token) = token {
            self.client.show_progress_report(token, message).await;
        }
    }

    pub async fn end_progress<S: Display>(&self, token: Option<NumberOrString>, message: S) {
        if let Some(token) = token {
            self.client.show_progress_end(token, message).await;
        }
    }

    pub async fn publish_emulator_outline(&self, lines: Vec<String>, locations: HashMap<usize, DebuggerOutlineLocation>) {
        self.client.send_custom_notification::<DebuggerOutlineNotification>(DebuggerOutlineParams {
            lines,
            locations

        }).await;
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
                if let Some(location) = self.addresses().get(&(b.address as usize)) {
                    let info = Diagnostic {
                        message: format!("Debugger Breakpoint @ ${:0>4X}", b.address),
                        range: location.range,
                        severity: Some(DiagnosticSeverity::Warning),
                        .. Diagnostic::default()
                    };
                    self.diagnostics().entry(location.uri.clone()).or_insert_with(Vec::new).push(info);
                }
            }
            if status.debugger == 1 {
                if let Some(location) = self.addresses().get(&(status.pc as usize)) {
                    let info = Diagnostic {
                        message: format!(
                            "Debugger Halt\nAF={:0>4X}\nBC={:0>4X}\nDE={:0>4X}\nHL={:0>4X}\nSP={:0>4X}\nPC={:0>4X}",
                            status.registers[0],
                            status.registers[1],
                            status.registers[2],
                            status.registers[3],
                            status.registers[4],
                            status.pc,
                        ),
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

