// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::Deserialize;


// Internal Dependencies ------------------------------------------------------
use crate::state::State;
use crate::types::DebuggerOutlineLocation;


// Types ----------------------------------------------------------------------
#[derive(Debug, Default, Eq, PartialEq, Deserialize, Clone)]
pub struct EmulatorAddressInfo {
    pub address: u16,
    pub bank: u16
}


// Emulator Status -----------------------------------------------------------
#[derive(Debug, Default, Deserialize, Clone)]
pub struct EmulatorRegisters {
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
}

#[derive(Debug, Default, Deserialize, Clone)]
pub struct EmulatorStatus {
    pub name: String,
    pub paused: bool,
    pub menu: bool,
    pub filename: String,
    pub crc: u32,

    pub title: String,
    pub stopped: bool,
    pub pc: u16,
    pub bank: u16,
    pub registers: EmulatorRegisters,
    pub ime: bool,
    pub breakpoints: Vec<EmulatorAddressInfo>,
    pub backtrace: Vec<EmulatorAddressInfo>,
}

impl EmulatorStatus {
    pub(in crate::emulator) fn to_status_message(&self, does_rom_match: bool) -> String {
        let info = format!("{} via {}", self.title, self.name);
        if !does_rom_match {
            format!("ROM path does not match")

        } else if self.stopped {
            format!("{} [STOPPED] [BRK @ ${:0>4X}]", info, self.pc)

        } else if self.menu {
            format!("{} [STOPPED] [IN MENU]", info)

        } else if self.paused {
            format!("{} [PAUSED]", info)

        } else {
            format!("{} [RUNNING]", info)
        }
    }

    pub(in crate::emulator) fn to_outline(
        &self,
        data: &State,
        does_rom_match: bool

    ) -> (String, HashMap<usize, DebuggerOutlineLocation>) {

        // TODO display RUNNING / HALTED info
        let mut locations: HashMap<usize, DebuggerOutlineLocation> = HashMap::new();
        let mut outline = format!("Status:\n  Emulator \"{}\" connected\n", self.name);
        if !does_rom_match {
            return (outline, locations);
        }

        // TODO use bank correctly
        let mut resolve_symbol_with_offset = |address: u16, _bank: u16, line: usize| {
            let mut label_address = 0;
            let mut label_name = "Unknown".to_string();
            let mut location = None;

            // Find the closest labeled address
            for (addr, name, filename, line, character) in data.labels().iter() {
                if *addr > address {
                    break

                } else {
                    label_address = *addr;
                    label_name = name.clone();
                    location = Some(DebuggerOutlineLocation {
                        filename: filename.clone(),
                        line: *line,
                        character: *character
                    });
                }
            }

            // Use location from addresses if available otherwise fallback to closest matching label
            if let Some(location) = data.addresses().get(&(address as usize)) {
                locations.insert(line, DebuggerOutlineLocation {
                    filename: location.uri.path().to_string(),
                    line: location.range.start.line as usize,
                    character: location.range.start.character as usize
                });

            } else if let Some(location) = location {
                locations.insert(line, location);
            }

            let offset = address.saturating_sub(label_address);
            if offset > 0 {
                format!("{}+${:0>3X}", label_name, offset)

            } else {
                label_name.to_string()
            }

        };

        // Registers
        let f = self.registers.af as u8;
        outline.push_str(&format!(
            "\nRegisters:\n  AF = ${:0>4X} ({}{}{}{})\n  BC = ${:0>4X} ({})\n  DE = ${:0>4X} ({})\n  HL = ${:0>4X} ({})\n  SP = ${:0>4X} ({})\n  PC = ${:0>4X} ({})\n  IME = {}\n\nBacktrace:\n",
            self.registers.af,
            if (f & 16) == 16 { "C" } else { "-" },
            if (f & 32) == 32 { "H" } else { "-" },
            if (f & 64) == 64 { "N" } else { "-" },
            if (f & 128) == 128 { "Z" } else { "-" },
            self.registers.bc,
            resolve_symbol_with_offset(self.registers.bc, self.bank, 5),
            self.registers.de,
            resolve_symbol_with_offset(self.registers.de, self.bank, 6),
            self.registers.hl,
            resolve_symbol_with_offset(self.registers.hl, self.bank, 7),
            self.registers.sp,
            resolve_symbol_with_offset(self.registers.sp, self.bank, 8),
            self.pc,
            resolve_symbol_with_offset(self.pc, self.bank, 9),
            if self.ime { "Enabled"} else { "Disabled"}
        ));

        // Backtrace
        let breakpoint_offset = 15 + self.backtrace.len();
        for (i, b) in self.backtrace.iter().enumerate() {
            outline.push_str(&format!(
                "{: >2}. {} (${:0>2X}:${:0>4X})\n",
                i + 1,
                resolve_symbol_with_offset(b.address, b.bank, 13 + i),
                b.bank,
                b.address
            ));
        }

        // Breakpoints
        outline.push_str("\nBreakpoints:\n");
        for (i, b) in self.breakpoints.iter().enumerate() {
            outline.push_str(&format!(
                "{: >2}. {} (${:0>2X}:${:0>4X})\n",
                i + 1,
                resolve_symbol_with_offset(b.address, b.bank, breakpoint_offset),
                if b.bank == 0xFFFF { 0 } else { b.bank },
                b.address
            ));
        }

        (outline, locations)
    }
}

