// STD Dependencies -----------------------------------------------------------
use std::io::prelude::*;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};
use std::net::{SocketAddr, TcpStream};
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::Ordering;

// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use serde::Deserialize;
use tower_lsp::Client;
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::{Diagnostic, Location, Url, NumberOrString, WorkDoneProgressCreateParams};


use crate::analyzer::Analyzer;
// Internal Dependencies ------------------------------------------------------
use crate::types::{InlayHintsNotification, InlayHintsParams};


// Types ----------------------------------------------------------------------
#[derive(Debug)]
pub enum EmulatorCommand {
    QueryAddressValue(u16),
    DebuggerToggleBreakpoint(u16),
    DebuggerStep,
    DebuggerStepOver,
    DebuggerFinish,
    DebuggerContinue,
}

#[derive(Debug, Default, Eq, PartialEq, Deserialize, Clone)]
pub struct EmulatorBreakpoint {
    pub address: u16,
    pub bank: u16
}

#[derive(Debug, Default, Deserialize, Clone)]
pub struct EmulatorStatus {
    emulator: String,
    pub debugger: u8,
    pub pc: u16,
    pub registers: [u16; 5],
    paused: u8,
    menu: u8,
    pub breakpoints: Vec<EmulatorBreakpoint>,
    filename: String,
    crc: u32,
    title: String
}

#[derive(Debug, Deserialize)]
pub struct EmulatorAddressValue {
    address: u16,
    value: u8
}

// Emulator Connection --------------------------------------------------------
pub struct EmulatorConnection {
    client: Arc<Client>,
    diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
    addresses: Arc<Mutex<HashMap<usize, Location>>>,
    shutdown: Arc<AtomicBool>,
    running: Arc<AtomicBool>,
    connection_closed: Arc<AtomicBool>,
    pub status: Arc<Mutex<Option<EmulatorStatus>>>,
    pub commands: Arc<Mutex<VecDeque<EmulatorCommand>>>,
    pub results: Arc<Mutex<HashMap<u16, u8>>>,
}

impl EmulatorConnection {
    pub fn new(
        client: Arc<Client>,
        diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
        addresses: Arc<Mutex<HashMap<usize, Location>>>

    ) -> Self {
        Self {
            client,
            diagnostics,
            addresses,
            shutdown: Arc::new(AtomicBool::new(false)),
            running: Arc::new(AtomicBool::new(false)),
            connection_closed: Arc::new(AtomicBool::new(false)),
            status: Arc::new(Mutex::new(None)),
            commands: Arc::new(Mutex::new(VecDeque::new())),
            results: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn listen(&self, rom_path: PathBuf) {

        // Stop any running instance of the emulator connection
        if self.running.load(Ordering::SeqCst) {
            self.shutdown().await;
        }

        // Setup Running State
        self.shutdown.store(false, Ordering::SeqCst);
        self.running.store(true, Ordering::SeqCst);
        self.connection_closed.store(false, Ordering::SeqCst);

        let handle = Handle::current();
        let client = self.client.clone();
        let status = self.status.clone();
        let diagnostics = self.diagnostics.clone();
        let addresses = self.addresses.clone();
        let commands = self.commands.clone();
        let results = self.results.clone();
        let shutdown = self.shutdown.clone();
        let running = self.running.clone();
        let connection_closed = self.connection_closed.clone();
        handle.spawn(async move {
            tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
            Self::connect(
                client,
                rom_path,
                status,
                diagnostics,
                addresses,
                commands,
                results,
                running,
                shutdown,
                connection_closed
            ).await;
        });
    }

    pub async fn shutdown(&self) {
        if self.running.load(Ordering::SeqCst) {
            // Setup shutdown state
            self.shutdown.store(true, Ordering::SeqCst);
            self.running.store(false, Ordering::SeqCst);
            self.connection_closed.store(false, Ordering::SeqCst);

            // Wait for connection to be closed
            while !self.connection_closed.load(Ordering::SeqCst) {
                tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
            }
        }
    }

    async fn connect(
        client: Arc<Client>,
        rom_path: PathBuf,
        status: Arc<Mutex<Option<EmulatorStatus>>>,
        diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
        addresses: Arc<Mutex<HashMap<usize, Location>>>,
        commands: Arc<Mutex<VecDeque<EmulatorCommand>>>,
        results: Arc<Mutex<HashMap<u16, u8>>>,
        running: Arc<AtomicBool>,
        shutdown: Arc<AtomicBool>,
        connection_closed: Arc<AtomicBool>
    ) {
        let addr: SocketAddr = "127.0.0.1:8765".parse().unwrap();
        while running.load(Ordering::SeqCst) {
            if let Ok(stream) = TcpStream::connect_timeout(&addr, Duration::from_millis(10)) {
                log::info!(&format!("Emulator connected for {}", rom_path.display()));

                // Handle connection
                let inner_results = results.clone();
                Self::connection(
                    stream,
                    client.clone(),
                    rom_path.clone(),
                    status.clone(),
                    diagnostics.clone(),
                    addresses.clone(),
                    commands.clone(),
                    shutdown.clone(),
                    move |msg| {

                    if let Ok(status) = serde_json::from_str::<EmulatorStatus>(msg) {
                        Some(status)

                    } else if let Ok(value) = serde_json::from_str::<EmulatorAddressValue>(msg) {
                        if let Ok(mut r) = inner_results.lock() {
                            r.insert(value.address, value.value);
                        }
                        None

                    } else {
                        log::info!(&format!("Unknown Emulator Message: {:?}", msg));
                        None
                    }

                }).await;
                log::info!("Emulator disconnected");
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }
        connection_closed.store(true, Ordering::SeqCst);
    }

    async fn connection<C: FnMut(&str) -> Option<EmulatorStatus>>(
        mut stream: TcpStream,
        client: Arc<Client>,
        rom_path: PathBuf,
        status: Arc<Mutex<Option<EmulatorStatus>>>,
        diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
        addresses: Arc<Mutex<HashMap<usize, Location>>>,
        commands: Arc<Mutex<VecDeque<EmulatorCommand>>>,
        shutdown: Arc<AtomicBool>,
        mut message: C
    ) {
        // TCP Stream setup
        stream.set_nodelay(true).ok();
        stream.set_read_timeout(Some(Duration::from_millis(10))).ok();
        stream.write_all(&[0x00, 0x00, 0x00]).ok();

        // Report Progress to Editor
        let token = NumberOrString::String("emulation".to_string());
        client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: token.clone()

        }).await.ok();
        client.show_progress_begin(token.clone(), "Emulating", "Connecting...").await;

        // Clear any outstand commands
        if let Ok(mut c) = commands.try_lock() {
            c.clear();
        }

        // Timers
        let mut query_status_timer = Instant::now();
        let mut trigger_hints_refresh = Instant::now();
        let mut last_status_response = Instant::now();

        // Handle Emulator connection
        let mut last_status: Option<EmulatorStatus> = None;
        let mut buffer: Vec<u8> = Vec::new();
        let mut received = [0; 1024];
        loop {
            // Close connection if LSP is shutting down...
            if shutdown.load(std::sync::atomic::Ordering::SeqCst) {
                log::info!("Emulator closing connection...");
                break;

            // ...or emulator stopped sending status responses
            } else if last_status_response.elapsed() > Duration::from_millis(2500) {
                log::info!("Emulator no status within 2500ms...");
                break;
            }

            // Receive messages from emulator
            let mut received_status = None;
            match stream.read(&mut received) {
                Ok(0) => break,
                Ok(n) => {
                    for i in &received[0..n] {
                        buffer.push(*i);
                        if *i == '\t' as u8 {
                            let part = std::mem::replace(&mut buffer, Vec::new());
                            if let Ok(json) = String::from_utf8(part) {
                                if let Some(s) = message(json.trim()) {
                                    received_status = Some(s);
                                }
                            }
                        }
                    }
                },
                Err(e) => if e.kind() != std::io::ErrorKind::WouldBlock {
                    log::warn!(&format!("Emulator Error: {:?}", e.kind()));
                    break;
                }
            }

            // Query emulator status periodically
            if query_status_timer.elapsed() > Duration::from_millis(200) {
                stream.write_all(&[0x00, 0x00, 0x00]).ok();
                query_status_timer = Instant::now();
            }

            // Forward emulation status to editor
            if let Some(s) = received_status {

                // TODO verify file path and ROM crc and report mismatch / ignore any commands by
                // not setting the status on the outside Analyzer
                let does_rom_match = PathBuf::from(&s.filename) == rom_path;

                // Update Progress Report
                let info = format!("{} via {}", s.title, s.emulator);
                let message = if !does_rom_match {
                    format!("ROM path does not match")

                } else if s.debugger == 1 {
                    format!("{} [STOPPED] [BRK @ ${:0>4X}]", info, s.pc)

                } else if s.menu == 1 {
                    format!("{} [STOPPED] [IN MENU]", info)

                } else if s.paused == 1 {
                    format!("{} [PAUSED]", info)

                } else {
                    format!("{} [RUNNING]", info)
                };
                client.show_progress_report(token.clone(), message).await;

                // Only update and set status if ROM file matches
                if does_rom_match {
                    // Update Status Info in Analyzer
                    if let Ok(mut status) = status.lock() {
                        *status = Some(s.clone());
                    }
                    let mut update_diagnostics = false;
                    let update_hints = if let Some(ref ls) = last_status {
                        if s.debugger != ls.debugger {
                            update_diagnostics = true;
                            true

                        } else if s.menu != ls.menu {
                            true

                        } else if s.paused != ls.paused {
                            true

                        // Trigger when PC changes during active debugger
                        } else if s.debugger == 1 && (s.pc != ls.pc) {
                            update_diagnostics = true;
                            true

                        // Otherwise periodically update the hints
                        } else if trigger_hints_refresh.elapsed() > Duration::from_millis(5000) {
                            trigger_hints_refresh = Instant::now();
                            true

                        } else if s.breakpoints != ls.breakpoints {
                            log::info!(&format!("Breakpoints: {:?}", s.breakpoints));
                            update_diagnostics = true;
                            false

                        } else {
                            false
                        }
                    } else {
                        update_diagnostics = s.debugger == 1;
                        true
                    };
                    if update_diagnostics {
                        Analyzer::publish_diagnostics(
                            client.clone(),
                            diagnostics.clone(),
                            addresses.clone(),
                            status.clone()

                        ).await;
                    }
                    if update_hints {
                        client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
                    }
                    last_status = Some(s);
                }
                last_status_response = Instant::now();
            }

            // Send commands to emulator
            if last_status.is_some() {
                if let Ok(mut c) = commands.try_lock() {
                    while let Some(command) = c.pop_front() {
                        match command {
                            EmulatorCommand::QueryAddressValue(address) => {
                                stream.write_all(&[0x80, address as u8, (address >> 8) as u8]).ok();
                            },
                            EmulatorCommand::DebuggerToggleBreakpoint(address) => {
                                stream.write_all(&[0x10, address as u8, (address >> 8) as u8]).ok();
                            },
                            EmulatorCommand::DebuggerStep => {
                                stream.write_all(&[0x20, 0x00, 0x00]).ok();
                            },
                            EmulatorCommand::DebuggerStepOver => {
                                stream.write_all(&[0x21, 0x00, 0x00]).ok();
                            },
                            EmulatorCommand::DebuggerFinish => {
                                stream.write_all(&[0x22, 0x00, 0x00]).ok();
                            },
                            EmulatorCommand::DebuggerContinue => {
                                stream.write_all(&[0x23, 0x00, 0x00]).ok();
                            }
                        }
                    }
                }
            }
        }

        // Reset Editor Status
        if let Ok(mut status) = status.lock() {
            *status = None;
        }
        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        Analyzer::publish_diagnostics(client.clone(), diagnostics.clone(), addresses.clone(), status.clone()).await;
        client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
        client.show_progress_end(token, "Emulating stopped").await;
    }
}

