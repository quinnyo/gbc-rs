// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::error::Error;
use std::path::PathBuf;
use std::io::prelude::*;
use std::collections::HashMap;
use std::sync::atomic::Ordering;
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};
use std::net::{SocketAddr, TcpStream};


// External Dependencies ------------------------------------------------------
use serde::Deserialize;
use serde_json::de::IoRead;
use tokio::runtime::Handle;
use tower_lsp::lsp_types::Location;


// Internal Dependencies ------------------------------------------------------
use crate::state::State;


// Modules --------------------------------------------------------------------
mod status;
mod process;
pub use self::status::EmulatorStatus;
pub use self::process::EmulatorProcess;


// Types ----------------------------------------------------------------------
#[derive(Debug)]
pub enum EmulatorCommand {
    ReadAddressValue(u16),
    WriteAddressValue(u32, u8),
    DebuggerToggleBreakpoint(Location),
    DebuggerStep,
    DebuggerNext,
    DebuggerFinish,
    DebuggerContinue,
    DebuggerUndo
}

#[derive(Debug, Deserialize)]
pub struct EmulatorAddressValue {
    address: u16,
    value: u8
}

#[derive(Debug, Deserialize)]
pub enum EmulatorResponse {
    Status(EmulatorStatus),
    AddressValue(EmulatorAddressValue)
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
                log::info!("Emulator connected for {}", rom_path.display());
                Self::connection(stream, state.clone(), rom_path.clone(), shutdown.clone()).await;
                log::info!("Emulator disconnected");
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }
        connection_closed.store(true, Ordering::SeqCst);
    }

    async fn connection(mut stream: TcpStream, state: State, rom_path: PathBuf, shutdown: Arc<AtomicBool>) {
        // Timers
        let mut status_query_timer = Instant::now();
        let mut status_response_timer = Instant::now();
        let mut hints_refresh_timer = Instant::now();

        // Report Progress to Editor
        let progress_token = state.start_progress("Emulating", "Connecting...").await;

        // Clear any outstand commands
        state.commands().clear();

        // TCP Stream setup
        stream.set_nodelay(true).ok();
        stream.set_read_timeout(Some(Duration::from_millis(10))).ok();
        stream.write_all(&[0x00]).ok();

        // Handle Emulator connection
        let mut last_status: Option<EmulatorStatus> = None;
        let mut sender = stream.try_clone().unwrap();
        let mut de = serde_json::Deserializer::from_reader(stream);
        'outer: loop {

            // Close connection if LSP is shutting down...
            if shutdown.load(std::sync::atomic::Ordering::SeqCst) {
                log::info!("Emulator closing connection...");
                break;

            // ...or emulator stopped sending status responses
            } else if status_response_timer.elapsed() > Duration::from_millis(2500) {
                log::info!("Emulator no status within 2500ms...");
                break;
            }

            // Receive messages
            loop {
                match Self::read_message::<EmulatorResponse>(&mut de) {
                    Ok(Some(EmulatorResponse::Status(status))) => {
                        // TODO also compare CRC
                        let does_rom_match = PathBuf::from(&status.filename) == rom_path;

                        // Update Progress Report
                        state.update_progress(progress_token.clone(), status.to_status_message(does_rom_match)).await;

                        // Only update and set status if ROM file matches
                        if does_rom_match {
                            // Update Status Info in Analyzer
                            state.set_status(Some(status.clone()));
                            let (update_hints, update_diagnostics, update_outline) = if let Some(ref ls) = last_status {
                                if status.stopped != ls.stopped {
                                    (true, true, true)

                                } else if status.menu != ls.menu {
                                    (true, false, false)

                                } else if status.paused != ls.paused {
                                    (true, false, false)

                                // Trigger when PC changes during active stopped
                                } else if status.stopped && (status.pc != ls.pc) {
                                    (true, true, true)

                                // Otherwise periodically update the hints and outline
                                } else if !status.stopped && hints_refresh_timer.elapsed() > Duration::from_millis(5000) {
                                    hints_refresh_timer = Instant::now();
                                    (true, false, true)

                                } else if status.breakpoints != ls.breakpoints {
                                    (false, true, true)

                                } else {
                                    (false, false, false)
                                }
                            } else {
                                (true, status.stopped, true)
                            };
                            if update_diagnostics {
                                state.publish_diagnostics().await;
                            }
                            if update_outline {
                                let (outline, locations) = status.to_outline(&state, does_rom_match);
                                state.publish_emulator_outline(
                                    outline.split('\n').into_iter().map(|s| s.to_string()).collect(),
                                    locations
                                ).await;
                            }
                            if update_hints {
                                state.trigger_client_hints_refresh().await;
                            }
                            last_status = Some(status);
                        }
                        status_response_timer = Instant::now();
                    },
                    Ok(Some(EmulatorResponse::AddressValue(address))) => {
                        state.results().insert(address.address, address.value);
                    },
                    Ok(None) => break,
                    Err(_) => break 'outer
                }
            }

            // Query emulator status periodically
            if status_query_timer.elapsed() > Duration::from_millis(200) {
                sender.write_all(&[0x00]).ok();
                status_query_timer = Instant::now();
            }

            // Forward commands to emulator
            if last_status.is_some() {
                let mut commands = state.commands();
                while let Some(command) = commands.pop_front() {
                    match command {
                        EmulatorCommand::ReadAddressValue(address) => {
                            sender.write_all(&[0x80, address as u8, (address >> 8) as u8]).ok();
                        },
                        EmulatorCommand::WriteAddressValue(address, value) => {
                            // TODO support u32 and higher rom addresses
                            sender.write_all(&[0x40, address as u8, (address >> 8) as u8, value]).ok();
                        },
                        EmulatorCommand::DebuggerToggleBreakpoint(location) => {
                            for (address, loc) in state.address_locations().iter() {
                                if location.uri == loc.uri && location.range.start.line == loc.range.start.line {
                                    sender.write_all(&[0x10, *address as u8, (*address >> 8) as u8]).ok();
                                    break;
                                }
                            }
                        },
                        EmulatorCommand::DebuggerStep => {
                            if let Some(s) = state.emulator().as_mut() {
                                s.send(b"step\n");

                            } else {
                                sender.write_all(&[0x20]).ok();
                            }
                        },
                        EmulatorCommand::DebuggerNext => {
                            if let Some(s) = state.emulator().as_mut() {
                                s.send(b"next\n");

                            } else {
                                sender.write_all(&[0x21]).ok();
                            }
                        },
                        EmulatorCommand::DebuggerFinish => {
                            if let Some(s) = state.emulator().as_mut() {
                                s.send(b"finish\n");

                            } else {
                                sender.write_all(&[0x22]).ok();
                            }
                        },
                        EmulatorCommand::DebuggerContinue => {
                            if let Some(s) = state.emulator().as_mut() {
                                s.send(b"continue\n");

                            } else {
                                sender.write_all(&[0x23]).ok();
                            }
                        },
                        EmulatorCommand::DebuggerUndo => {
                            if let Some(s) = state.emulator().as_mut() {
                                s.send(b"undo\n");

                            } else {
                                sender.write_all(&[0x24]).ok();
                            }
                        }
                    }
                }
            }
        }

        // Reset Editor Status
        state.set_status(None);
        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

        state.publish_diagnostics().await;
        state.publish_emulator_outline(vec![], HashMap::new()).await;
        state.trigger_client_hints_refresh().await;
        state.end_progress(progress_token, "Emulation stopped").await;
    }

    fn read_message<'a, T: Deserialize<'a>>(de: &'a mut serde_json::Deserializer<IoRead<TcpStream>>) -> Result<Option<T>, serde_json::Error> {
        match T::deserialize(de) {
            Ok(v) => Ok(Some(v)),
            Err(err) => if let Some(source) = err.source() {
                if let Some(e) = source.downcast_ref::<std::io::Error>() {
                    if e.kind() == std::io::ErrorKind::WouldBlock {
                        Ok(None)

                    } else {
                        Err(err)
                    }

                } else {
                    Err(err)
                }

            } else if err.classify() == serde_json::error::Category::Eof {
                Err(err)

            } else {
                Ok(None)
            }
        }
    }
}

