// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::io::prelude::*;
use std::process::Stdio;
use std::collections::HashMap;
use std::process::{Child, Command};


// External Dependencies ------------------------------------------------------
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Url};


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

    pub fn update_entries(&mut self, entries: HashMap<usize, SectionEntry>) -> Vec<(Url, Diagnostic)> {
        let mut diagnostics = Vec::new();
        if self.is_running() {
            for address in self.entries.keys() {
                if !entries.contains_key(&address) {
                    self.diagnostic(&mut diagnostics, *address, "ROM entry removed, restart emulator to synchronize");
                    return diagnostics;
                }
            }
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
                        self.diagnostic(&mut diagnostics, address, "ROM entry changed, restart emulator to synchronize");
                        break
                    }

                } else {
                    self.diagnostic(&mut diagnostics, address, "ROM entry added, restart emulator to synchronize");
                    break
                }
            }
        }
        diagnostics
    }

    fn diagnostic<S: Into<String>>(&self, diagnostics: &mut Vec<(Url, Diagnostic)>, address: usize, message: S) {
        if let Some(loc) = self.state.address_locations().get(&address) {
            diagnostics.push((loc.uri.clone(), Diagnostic {
                range: loc.range,
                severity: Some(DiagnosticSeverity::Warning),
                message: message.into(),
                code: None,
                code_description: None,
                source: None,
                related_information: None,
                tags: None,
                data: None
            }));
        }
    }

    pub fn is_running(&mut self) -> bool {
        match self.child.try_wait() {
            Ok(None) => true,
            _ => false
        }
    }

    pub fn send(&mut self, input: &[u8]) {
        if let Some(stdin) = self.child.stdin.as_mut() {
            stdin.write_all(input).ok();
        }
    }

    pub fn reset(&mut self, entries: HashMap<usize, SectionEntry>) -> bool {
        if self.is_running() {
            if let Some(stdin) = self.child.stdin.as_mut() {
                log::info!("Emulator reloading \"{}\"", self.rom_path.display());
                stdin.write_all(b"reset\n").ok();
                self.entries = entries;
                true

            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn stop(&mut self) {
        if let Some(stdin) = self.child.stdin.as_mut() {
            log::info!("Emulator stopping for \"{}\"", self.rom_path.display());
            stdin.write_all(b"quit\n").ok();
        }
    }
}

