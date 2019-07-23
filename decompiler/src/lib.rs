// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use colored::Colorize;
use gb_cpu::{Instruction, self};
use file_io::{FileReader, FileWriter};


// Structs --------------------------------------------------------------------
#[derive(Debug)]
struct AddressEntry {
    offset: usize,
    instruction: Option<Instruction>,
    label: Option<String>,
    jumps_from: HashSet<usize>,
    calls_from: HashSet<usize>,
    jumps_to: Option<usize>,
    calls_to: Option<usize>,
    returns_from: Option<usize>
}

impl AddressEntry {

    fn new(offset: usize) -> Self {
        Self {
            offset,
            instruction: None,
            label: None,
            jumps_from: HashSet::new(),
            calls_from: HashSet::new(),
            jumps_to: None,
            calls_to: None,
            returns_from: None
        }
    }

    fn set_instruction(&mut self, instr: Instruction) {
        self.instruction = Some(instr);
    }

    fn record_jump_from(&mut self, address: usize) {
        self.jumps_from.insert(address);
        if self.label.is_none() {
            self.generate_label();
        }
    }

    fn record_call_from(&mut self, address: usize) {
        self.calls_from.insert(address);
        if self.label.is_none() {
            self.generate_label();
        }
    }

    fn record_jump_to(&mut self, address: usize) {
        self.jumps_to = Some(address);
    }

    fn record_call_to(&mut self, address: usize) {
        self.calls_to = Some(address);
    }

    fn record_return_from(&mut self, address: usize) {
        self.returns_from = Some(address);
    }

    fn set_label(&mut self, label: String) {
        self.label = Some(label);
    }

    fn generate_label(&mut self) {
        self.label = Some(format!("unknown_0x{:0>4X}", self.offset));
    }

    fn to_string(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        format!(
            "{}0x{:0>4X}{}{}",

            // Show Label
            if let Some(label) = self.label.as_ref() {
                format!("\n{}:\n    ", label)

            } else {
                "    ".to_string()
            },
            self.offset,

            // Show Instruction
            if let Some(instr) = self.instruction.as_ref() {
                format!(
                    " {}{}{}",
                    instr,
                    // Show Jump target
                    if let Some(to) = self.jumps_to {
                        format!("; {}", Self::address_label(entries, to))

                    } else {
                        "".to_string()

                    },
                    // Show Call Target
                    if let Some(to) = self.calls_to {
                        format!("; {}", Self::address_label(entries, to))

                    } else {
                        "".to_string()
                    }
                )

            } else {
                "".to_string()
            },
            if let Some(parent) = self.returns_from {
                format!("; {} -> {}", Self::address_label(entries, parent), if let Some(parent) = entries.get(&parent) {
                    parent.calls_from.iter().map(|caller| {
                        Self::address_label(entries, *caller)

                    }).collect::<Vec<String>>().join(", ")

                } else {
                    "???".to_string()
                })

            } else {
                "".to_string()
            }
        )
    }

    fn address_label(entries: &HashMap<usize, AddressEntry>, address: usize) -> String {
        if let Some(entry) = entries.get(&address) {
            if let Some(label) = entry.label.as_ref() {
                label.to_string()

            } else {
                format!("0x{:0>4X}", address)
            }

        } else {
            format!("0x{:0>4X}", address)
        }
    }

}

// GameBoy Instruction Decompiler ---------------------------------------------
pub struct Decompiler {
    instructions: Vec<Instruction>,
    addresses: HashMap<usize, AddressEntry>,
    visited: HashSet<usize>,
    silent: bool,
    output: Vec<String>
}

impl Decompiler {

    // TODO load symbol file
    // TODO replace and set symbols
    // TODO support showing memory address symbols instead of instruction values
    pub fn new() -> Self {
        Self {
            instructions: gb_cpu::instruction_list(),
            addresses: HashMap::new(),
            visited: HashSet::new(),
            silent: false,
            output: Vec::new()
        }
    }

    pub fn set_silent(&mut self) {
        self.silent = true;
    }

    pub fn decompile_file<T: FileReader + FileWriter>(
        &mut self,
        io: &mut T,
        file: PathBuf

    ) -> Result<String, (String, DecompilationError)> {

        self.address_entry(0x0040).set_label("IE_VBLANK".to_string());
        self.address_entry(0x0050).set_label("IE_TIMER".to_string());
        self.address_entry(0x0058).set_label("IE_SERIAL".to_string());
        self.address_entry(0x0100).set_label("BOOT_ROM_EXIT".to_string());

        let rom_buffer = self.read_rom_file(io, &file).map_err(|e| self.error("instruction parsing", e))?;
        self.analyze_from(&rom_buffer, vec![
            (0x0100, None, Some(0x0100)),
            (0x0040, None, Some(0x0040)),
            (0x0050, None, Some(0x0050)),

        ]).map_err(|e| self.error("instruction parsing", e))?;
        for adr in self.range_to_string(0x0, 0x4000) {
            self.log(adr);
        }
        Ok(self.output.join("\n"))
    }

    fn read_rom_file<T: FileReader + FileWriter>(&self, io: &mut T, file: &PathBuf) -> Result<Vec<u8>, RomError>{
        let (_, contents) = io.read_binary_file(None, &file).map_err(|err| {
            RomError::new(0, format!("Failed to read ROM file \"{}\": {}", err.path.display(), err.io))
        })?;
        Ok(contents)
    }

    fn analyze_from(
        &mut self,
        rom_buffer: &[u8],
        mut call_stack: Vec<(usize, Option<usize>, Option<usize>)>,

    ) -> Result<(), RomError> {
        while let Some((mut address, caller_address, callee_address)) = call_stack.pop() {


            // Ignore addresses outside of ROM space
            if address > rom_buffer.len() - 1 {
                // TODO handle bank mapping
                continue;
            }

            while let Some(instr) = Instruction::decode(&rom_buffer[address..], &self.instructions, false) {
                let size = instr.byte_size();
                let flow_stopped = match instr.name {
                    "jr" => {
                        let target = (address as i32 + signed_byte(instr.value.unwrap_or(0) as i32)) as usize + size;
                        self.address_entry(target).record_jump_from(address);
                        self.address_entry(address).record_jump_to(target);
                        let stopped = instr.layout.len() == 1;
                        if !self.address_visited(target) {
                            call_stack.push((target, caller_address, callee_address ));
                        }
                        stopped
                    },
                    "jp" => {
                        let target = instr.value.unwrap_or(0) as usize;
                        self.address_entry(target).record_jump_from(address);
                        self.address_entry(address).record_jump_to(target);

                        let stopped = instr.layout.len() == 1;
                        if !self.address_visited(target) {
                            call_stack.push((target, caller_address, callee_address ));
                        }
                        stopped
                    },
                    "call" => {
                        let target = instr.value.unwrap_or(0) as usize;
                        self.address_entry(target).record_call_from(address);
                        self.address_entry(address).record_call_to(target);

                        //if !self.address_visited(target) {
                            // TODO do we need to run multiple times or should we try and work
                            // call / return address issues in another way
                            call_stack.push((target, Some(address), Some(target)));
                        //}
                        false
                    },
                    "ret" => {
                        if let Some(caller) = caller_address {
                            if let Some(callee) = callee_address {
                                self.address_entry(address).record_return_from(callee);
                            }
                        }
                        true
                    },
                    _ => false
                };

                self.address_entry(address).set_instruction(instr);
                address += size;

                if flow_stopped {
                    break;
                }
            }
        }

        Ok(())

    }

    fn address_visited(&mut self, address: usize) -> bool {
        if self.visited.contains(&address) {
            true

        } else{
            self.visited.insert(address);
            false
        }
    }

    fn address_entry(&mut self, address: usize) -> &mut AddressEntry {
        self.addresses.entry(address).or_insert_with(|| {
            AddressEntry::new(address)
        })
    }

    fn range_to_string(&self, from: usize, to: usize) -> Vec<String> {
        let mut addresses = Vec::new();
        for address in from..to {
            if let Some(address) = self.addresses.get(&address) {
                addresses.push(address.to_string(&self.addresses));
            }
        }
        addresses
    }

    // Helpers ----------------------------------------------------------------
    // TODO dry with compiler
    fn log<S: Into<String>>(&mut self, s: S) {
        if !self.silent  {
            self.output.push(s.into());
        }
    }

    fn warning<S: Into<String>>(&mut self, s: S) {
        if !self.silent {
            self.output.push(format!("     {} {}", "Warning".bright_yellow(), s.into()));
        }
    }

    fn info<S: Into<String>>(&mut self, s: S) {
        if !self.silent {
            self.output.push(format!("        {} {}", "Info".bright_blue(), s.into()));
        }
    }

    fn error(&self, stage: &str, error: RomError) -> (String, DecompilationError) {
        (self.output.join("\n"), DecompilationError::new(stage, error))
    }

}

fn signed_byte(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte
    }
}

// Decompiler Error Abstraction -----------------------------------------------
#[derive(Debug)]
pub struct RomError {
    pub rom_offset: usize,
    pub message: String
}

impl RomError {

    pub fn new(rom_offset: usize, message: String) -> Self {
        Self {
            rom_offset,
            message
        }
    }

}

impl fmt::Display for RomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ROM ${:0>4X} {}", self.rom_offset, self.message)
    }
}

#[derive(Debug)]
pub struct DecompilationError {
    stage: String,
    error: Option<RomError>,
    message: Option<String>
}

impl DecompilationError {

    fn new(stage: &str, error: RomError) -> Self {
        Self {
            stage: stage.to_string(),
            error: Some(error),
            message: None
        }
    }

    fn from_string<S: Into<String>>(message: S) -> Self {
        Self {
            stage: "instruction parsing".to_string(),
            error: None,
            message: Some(message.into())
        }
    }

}

impl fmt::Display for DecompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(source) = self.error.as_ref() {
            write!(f, "       {} De-compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, source)

        } else if let Some(message) = self.message.as_ref() {
            write!(f, "       {} Compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, message)

        } else {
            write!(f, "       {} Compilation failed during {} phase!", "Error".bright_red(), self.stage)
        }
    }
}

