// STD Dependencies -----------------------------------------------------------
use std::io::prelude::*;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};
use std::net::{SocketAddr, TcpStream};
use std::collections::HashMap;
use std::sync::atomic::Ordering;


// External Dependencies ------------------------------------------------------
use tokio::runtime::Handle;
use serde::Deserialize;


// Internal Dependencies ------------------------------------------------------
use crate::state::State;


// Modules --------------------------------------------------------------------
mod status;
pub use self::status::EmulatorStatus;


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

#[derive(Debug, Deserialize)]
pub struct EmulatorAddressValue {
    address: u16,
    value: u8
}


// Emulator Connection --------------------------------------------------------
pub struct EmulatorConnection {
    state: State,
    shutdown: Arc<AtomicBool>,
    running: Arc<AtomicBool>,
    connection_closed: Arc<AtomicBool>,
}

impl EmulatorConnection {
    pub fn new(state: State) -> Self {
        Self {
            state,
            shutdown: Arc::new(AtomicBool::new(false)),
            running: Arc::new(AtomicBool::new(false)),
            connection_closed: Arc::new(AtomicBool::new(false)),
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
        let state = self.state.clone();
        let shutdown = self.shutdown.clone();
        let running = self.running.clone();
        let connection_closed = self.connection_closed.clone();
        handle.spawn(async move {
            tokio::time::sleep(tokio::time::Duration::from_millis(250)).await;
            Self::connect(state, rom_path, running, shutdown, connection_closed).await;
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
        state: State,
        rom_path: PathBuf,
        running: Arc<AtomicBool>,
        shutdown: Arc<AtomicBool>,
        connection_closed: Arc<AtomicBool>
    ) {
        let addr: SocketAddr = "127.0.0.1:8765".parse().unwrap();
        while running.load(Ordering::SeqCst) {
            if let Ok(stream) = TcpStream::connect_timeout(&addr, Duration::from_millis(10)) {
                log::info!(&format!("Emulator connected for {}", rom_path.display()));

                // Handle connection
                let inner_state = state.clone();
                Self::connection( stream, state.clone(), rom_path.clone(), shutdown.clone(), move |msg| {

                    if let Ok(status) = serde_json::from_str::<EmulatorStatus>(msg) {
                        Some(status)

                    } else if let Ok(value) = serde_json::from_str::<EmulatorAddressValue>(msg) {
                        inner_state.results().insert(value.address, value.value);
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
        state: State,
        rom_path: PathBuf,
        shutdown: Arc<AtomicBool>,
        mut message: C
    ) {
        // TCP Stream setup
        stream.set_nodelay(true).ok();
        stream.set_read_timeout(Some(Duration::from_millis(10))).ok();
        stream.write_all(&[0x00, 0x00, 0x00]).ok();

        // Report Progress to Editor
        let progress_token = state.start_progress("Emulating", "Connecting...").await;

        // Clear any outstand commands
        state.commands().clear();

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

                // TODO also compare CRC
                let does_rom_match = PathBuf::from(&s.filename) == rom_path;

                // Update Progress Report
                state.update_progress(progress_token.clone(), s.to_status_message(does_rom_match)).await;

                // Only update and set status if ROM file matches
                if does_rom_match {
                    // Update Status Info in Analyzer
                    state.set_status(Some(s.clone()));
                    let (update_hints, update_diagnostics, update_outline) = if let Some(ref ls) = last_status {
                        if s.debugger != ls.debugger {
                            (true, true, true)

                        } else if s.menu != ls.menu {
                            (true, false, false)

                        } else if s.paused != ls.paused {
                            (true, false, false)

                        // Trigger when PC changes during active debugger
                        } else if s.debugger == 1 && (s.pc != ls.pc) {
                            (true, true, true)

                        // Otherwise periodically update the hints and outline
                        } else if s.debugger == 0 && trigger_hints_refresh.elapsed() > Duration::from_millis(5000) {
                            trigger_hints_refresh = Instant::now();
                            (true, false, true)

                        } else if s.breakpoints != ls.breakpoints {
                            (false, true, true)

                        } else {
                            (false, false, false)
                        }
                    } else {
                        (true, s.debugger == 1, true)
                    };
                    if update_diagnostics {
                        state.publish_diagnostics().await;
                    }
                    if update_outline {
                        let (outline, locations) = s.to_outline(&state, does_rom_match);
                        state.publish_emulator_outline(
                            outline.split('\n').into_iter().map(|s| s.to_string()).collect(),
                            locations
                        ).await;
                    }
                    if update_hints {
                        state.trigger_client_hints_refresh().await;
                    }
                    last_status = Some(s);
                }
                last_status_response = Instant::now();
            }

            // Forward commands to emulator
            if last_status.is_some() {
                let mut commands = state.commands();
                while let Some(command) = commands.pop_front() {
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

        // Reset Editor Status
        state.set_status(None);
        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

        state.publish_diagnostics().await;
        state.publish_emulator_outline(vec!["Emulator disconnected.".to_string()], HashMap::new()).await;
        state.trigger_client_hints_refresh().await;
        state.end_progress(progress_token, "Emulation stopped").await;
    }
}

