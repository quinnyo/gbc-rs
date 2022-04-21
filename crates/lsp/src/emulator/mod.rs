// STD Dependencies -----------------------------------------------------------
use std::thread;
use std::time::{Instant, Duration};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicBool;
use std::sync::mpsc::{self, Receiver, Sender};


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;
use tokio::runtime::Handle;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};
use gbd::{App, EmulatorAddress, EmulatorCommand, EmulatorStatus, EmulatorResponse, Model};


// Internal Dependencies ------------------------------------------------------
use compiler::linker::{AnalysisLint, SectionEntry};
use crate::state::State;


// Statics --------------------------------------------------------------------
type EmulatorData = (Arc<AtomicBool>, State, Sender<Receiver<EmulatorResponse>>, Vec<(u16, u16, String)>, Vec<u8>, Option<Model>);
lazy_static! {
    static ref EMULATOR_QUEUE: Arc<Mutex<(Sender<EmulatorData>, Receiver<EmulatorData>)>> = Arc::new(Mutex::new(mpsc::channel()));
}


// Emulator Handling ----------------------------------------------------------
pub struct Emulator {
    state: State,
    entries: HashMap<usize, SectionEntry>,
    running: Arc<AtomicBool>
}

impl Emulator {
    pub fn main_thread_loop() {
        loop {
            let data = EMULATOR_QUEUE.lock().expect("Lock failed").1.try_recv();
            if let Ok((running, state, s, labels, rom, model)) = data {
                log::info!("Starting emulator...");

                // Create emulator
                let (mut emulator, handle) = gbd::Emulator::new(model, rom);
                emulator.enable_debugger(labels, false);

                // Move out sender and forward receiver to watcher inside event loop
                let (sender, receiver) = handle.split();
                state.set_command_sender(Some(sender));
                s.send(receiver).ok();

                // Run emulator
                running.store(true, std::sync::atomic::Ordering::SeqCst);
                emulator.run().ok();
                running.store(false, std::sync::atomic::Ordering::SeqCst);
                state.set_command_sender(None);
                log::info!("Emulator stopped");
            }
            thread::sleep(Duration::from_millis(100));
        }
    }

    pub fn launch(
        state: State,
        entries: HashMap<usize, SectionEntry>,
        labels: Vec<(u16, u16, String)>,
        rom: Vec<u8>,
        model: Option<Model>

    ) -> Self {
        let running = Arc::new(AtomicBool::new(false));
        let inner_state = state.clone();
        let inner_running = running.clone();
        let (s, r) = mpsc::channel::<Receiver<EmulatorResponse>>();
        Handle::current().spawn(async move {
            // Wait for receiver to be send over from main thread
            let l = r.recv();
            if let Ok(receiver) = l {
                Self::watch(receiver, inner_running, inner_state).await;
            }
        });
        EMULATOR_QUEUE.lock().expect("Lock failed").0.send((running.clone(), state.clone(), s, labels, rom, model)).ok();
        Self {
            state,
            entries,
            running
        }
    }

    pub fn update_entries(&mut self, entries: HashMap<usize, SectionEntry>) -> Vec<AnalysisLint> {
        let mut lints = Vec::new();
        if self.is_running() {
            /*
            for address in self.entries.keys() {
                if !entries.contains_key(&address) {
                    self.lint(&mut lints, *address, &entries, "ROM entry added / removed or changed, restart emulator to synchronize");
                    return lints;
                }
            }*/

            // TODO double check and rework detection maybe only write in case all changes can be synced?
            let mut writes = Vec::new();
            for (address, entry) in &entries {
                // Check if entry with same address, type and size already exists in the rom
                if let Some(existing) = self.entries.get_mut(address) {
                    if entry.typ() == existing.typ() && entry.size == existing.size {
                        if entry.rom_bytes() != existing.rom_bytes() {
                            if let Some(bytes) = entry.rom_bytes() {
                                for (index, b) in bytes.iter().enumerate() {
                                    let address = (entry.offset as usize).saturating_add(index as usize);
                                    writes.push((EmulatorAddress::from_raw(address), *b));
                                }
                            }
                            self.lint(&mut lints, *address, &entries, "ROM entry changed, temporarily synced to emulator ROM");
                        }

                    } else {
                        // self.lint(&mut lints, *address, &entries, "ROM entry changed, restart emulator to synchronize");
                        break
                    }

                } else {
                    // self.lint(&mut lints, *address, &entries, "ROM entry added, restart emulator to synchronize");
                    break
                }
            }
            if !writes.is_empty() {
                self.state.send_command(EmulatorCommand::WriteRomMemory(writes));
            }
        }
        lints
    }

    fn lint<S: Into<String>>(&self, lints: &mut Vec<AnalysisLint>, address: usize, entries: &HashMap<usize, SectionEntry>, message: S) {
        let address = if entries.contains_key(&address) {
            address

        } else {
            let mut valid_addresses: Vec<usize> = entries.keys().cloned().collect();
            valid_addresses.sort_unstable();
            let mut nearest_address = 0;
            for addr in valid_addresses {
                if addr <= address {
                    nearest_address = addr;

                } else {
                    break;
                }
            }
            nearest_address
        };

        if let Some(loc) = self.state.address_locations().get(&address) {
            lints.push(AnalysisLint {
                uri: loc.uri.clone(),
                context: "".to_string(),
                detail: Diagnostic {
                    range: loc.range,
                    severity: Some(DiagnosticSeverity::WARNING),
                    message: message.into(),
                    code: None,
                    code_description: None,
                    source: None,
                    related_information: None,
                    tags: None,
                    data: None
                }
            });
        }
    }

    pub fn is_running(&self) -> bool {
        self.running.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn reset(
        &mut self,
        entries: HashMap<usize, SectionEntry>,
        labels: Vec<(u16, u16, String)>,
        rom: Vec<u8>,
        model: Option<Model>
    ) -> bool {
        if self.is_running() {
            self.entries = entries;
            self.state.send_command(EmulatorCommand::Reset {
                model,
                labels,
                rom
            });
            true

        } else {
            false
        }
    }

    pub fn stop(&mut self) {
        self.state.send_command(EmulatorCommand::Exit);
    }

    async fn watch(receiver: Receiver<EmulatorResponse>, running: Arc<AtomicBool>, state: State) {
        // Watch emulator progress
        let progress_token = state.start_progress("Emulator", "Starting...").await;
        let mut last_status = EmulatorStatus::default();
        let mut hint_refresh_timer = Instant::now();
        while running.load(std::sync::atomic::Ordering::SeqCst) {
            let mut updated_status = None;
            while let Ok(response) = receiver.try_recv() {
                match response {
                    EmulatorResponse::ReadMemory(results) => {
                        let mut map = state.results();
                        for (addr, value) in results {
                            map.insert(addr.address, value);
                        }
                    },
                    EmulatorResponse::Status(status) => {
                        updated_status = Some(status.clone());
                        state.set_status(Some(status));
                    },
                    EmulatorResponse::GotoAddress(_addr) => {
                        // TODO trigger editor to jump to the location of the address
                    }
                }
            }
            if let Some(status) = updated_status {
                let (update_hints, update_diagnostics) = if status.halted != last_status.halted {
                    (true, true)

                } else if status.halted && status.pc != last_status.pc {
                    (true, true)

                } else if !status.halted && hint_refresh_timer.elapsed() > Duration::from_millis(1000) {
                    hint_refresh_timer = Instant::now();
                    (true, false)

                } else if status.breakpoints != last_status.breakpoints {
                    (false, true)

                } else {
                    (false, false)
                };
                if update_diagnostics {
                    state.publish_diagnostics().await;
                }
                if update_hints {
                    state.trigger_client_hints_refresh().await;
                }
                if status.halted {
                    state.update_progress(progress_token.clone(), format!("Emulator halted @ ${:0>4X}", status.pc)).await;

                } else {
                    state.update_progress(progress_token.clone(), "Emulator running").await;
                }
                last_status = status;
            }
            state.send_command(EmulatorCommand::QueryStatus);
            thread::sleep(Duration::from_millis(100));
        }

        state.set_status(None);
        thread::sleep(Duration::from_millis(100));
        state.trigger_client_hints_refresh().await;
        state.end_progress(progress_token, "Emulator stopped").await;
    }
}

