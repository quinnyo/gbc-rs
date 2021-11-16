// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::io::prelude::*;
use std::process::Stdio;
use std::collections::HashMap;
use std::process::{Child, Command};


// Internal Dependencies ------------------------------------------------------
use crate::state::State;
use compiler::linker::SectionEntry;
use super::EmulatorCommand;


// Emulator Process -----------------------------------------------------------
pub struct EmulatorProcess {
    state: State,
    child: Child,
    rom_path: PathBuf,
    entries: HashMap<usize, SectionEntry>,
}

impl EmulatorProcess {
    pub fn launch(state: State, rom_path: PathBuf, entries: HashMap<usize, SectionEntry>) -> Option<Self> {
        match Command::new("sameboy").stderr(Stdio::piped()).stdin(Stdio::piped()).arg(rom_path.clone()).spawn() {
            Ok(child) => {
                log::info!("Emulator started for \"{}\"", rom_path.display());
                Some(Self {
                    state,
                    child,
                    rom_path,
                    entries
                })
            },
            Err(_) => None
        }
    }

    pub fn update_entries(&mut self, entries: HashMap<usize, SectionEntry>) {
        log::info!("Emulator computing entry diff for \"{}\"", self.rom_path.display());
        for (address, entry) in entries {
            // Check if entry with same address, type and size already exists in the rom
            if let Some(existing) = self.entries.get_mut(&address) {
                if entry.typ() == existing.typ() && entry.size == existing.size {
                    if entry.rom_bytes() != existing.rom_bytes() {
                        if let Some(bytes) = entry.rom_bytes() {
                            for (index, b) in bytes.iter().enumerate() {
                                self.state.commands().push_back(EmulatorCommand::WriteAddressValue(
                                    (entry.offset as u16).saturating_add(index as u16),
                                    *b
                                ));
                            }
                        }
                        *existing = entry;
                    }
                } else {
                    // TODO notify editor if there are other differences
                }
            }
        }
        // TODO notify editor if there are more / less entries
    }

    pub fn reset(&mut self, entries: HashMap<usize, SectionEntry>) -> bool {
        match self.child.try_wait() {
            Ok(Some(_)) => false,
            Ok(None) => {
                if let Some(stdin) = self.child.stdin.as_mut() {
                    log::info!("Emulator reloading \"{}\"", self.rom_path.display());
                    stdin.write_all(b"reset\n").ok();
                    self.entries = entries;
                    true

                } else {
                    false
                }
            },
            Err(_) => false,
        }
    }

    pub fn stop(&mut self) {
        if let Some(stdin) = self.child.stdin.as_mut() {
            log::info!("Emulator stopping for \"{}\"", self.rom_path.display());
            stdin.write_all(b"quit\n").ok();
        }
    }
}

