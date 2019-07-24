// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use colored::Colorize;
use gb_cpu::{Instruction, self};
use file_io::{FileReader, FileWriter};


// Modules --------------------------------------------------------------------
mod address_entry;
mod error;

use self::address_entry::AddressEntry;
use self::error::{DecompilationError, RomError};


// GameBoy Instruction Decompiler ---------------------------------------------
pub struct Decompiler {
    instructions: Vec<Instruction>,
    addresses: HashMap<usize, AddressEntry>,
    visited: HashSet<usize>,
    silent: bool,
    output: Vec<String>
}

impl Decompiler {

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

        let (rom_buffer, symbols) = self.read_rom_file(io, &file).map_err(|e| self.error("instruction parsing", e))?;
        // TODO return symbols locations in tuple format to pass in below
        self.parse_symbols(symbols);

        // self.address_entry(0x0100).record_call_from(0x00);

        self.analyze_from(&rom_buffer, vec![
            (0x0100, None, Some(0x0100)),
            (0x0040, None, None),
            (0x0050, None, None),
            (0x0058, None, None),
            // TODO include known parent labels for cases where they are never called directly
            // (0x1B64, None, Some(0x1B64)),

        ]).map_err(|e| self.error("instruction parsing", e))?;

        // TODO record all callees in a hashset

        // TODO process and detect "functions"
            // TODO go through all callees
            // TODO assign a UID
            // TODO go forward until we reach another callee
                // TODO remember the largest address of any ret
                // TODO remember if the last token was jp without any conditions

            // TODO assign all addresses up until the largest ret the UID
            // TODO if the last token was a jp XX then assign the UID up until that and treat it as
            // a tail call

        for adr in self.range_to_string(0x0000, 0x400) {
            self.log(adr);
        }
        Ok(self.output.join("\n"))
    }

    fn parse_symbols(&mut self, symbols: Vec<String>) {
        for s in symbols {
            let mut line = s.split(' ');
            if let (Some(location), Some(name)) = (line.next(), line.next()) {
                let mut location = location.split(':');
                if let (Some(bank), Some(address)) = (location.next(), location.next()) {
                    if let Ok(bank) = u16::from_str_radix(bank, 16) {
                        if let Ok(addr) = u16::from_str_radix(address, 16) {
                            self.address_entry(addr as usize + bank as usize * 0x4000).set_label(name.to_string());
                        }
                    }
                }
            }
        }
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

                // TODO detect debug messages and breakpoints

                let flow_stopped = match instr.name {
                    "jr" => {
                        let always = instr.layout.len() == 1;
                        let target = (address as i32 + signed_byte(instr.value.unwrap_or(0) as i32)) as usize;

                        // Backwards jumps are considered to be always taken
                        // TODO does this always work?
                        self.address_entry(target).record_jump_from(address, always || target <= address);
                        self.address_entry(address).record_jump_to(target);

                        if !self.address_visited(target) {
                            call_stack.push((target, caller_address, callee_address));
                        }

                        always
                    },
                    // TODO check for obvious jump tables in case of jp [hl] and parse their
                    // addresses
                    "jp" => {
                        let always = instr.layout.len() == 1;
                        let target = instr.value.unwrap_or(0) as usize;
                        self.address_entry(target).record_jump_from(address, always);
                        self.address_entry(address).record_jump_to(target);

                        if !self.address_visited(target) {
                            call_stack.push((target, caller_address, callee_address ));
                        }
                        always
                    },
                    "call" => {
                        let target = instr.value.unwrap_or(0) as usize;
                        self.address_entry(target).record_call_from(address);
                        self.address_entry(address).record_call_to(target);

                        // if !self.address_visited(target) {
                            // TODO do we need to run multiple times or should we try and work
                            // call / return address issues in another way
                            call_stack.push((target, Some(address), Some(target)));
                        // }
                        false
                    },
                    "ret" | "reti" => {
                        if let Some(callee) = callee_address {
                            self.address_entry(address).record_return_from(callee);
                        }
                        instr.layout.len() == 0
                    },
                    _ => false
                };

                let size = instr.size;
                self.address_entry(address).set_instruction(instr);
                address += size;

                if flow_stopped {
                    break;
                }
            }
        }

        // Find function bodies
        let mut fid = 0;
        for address in self.function_addresses() {
            let addresses = self.find_function_body_addresses(address);
            for (index, address) in addresses.into_iter().enumerate() {
                let entry = self.address_entry(address);
                entry.set_function_id(fid);

                // Mark all inner labels as local
                if index > 0 {
                    entry.set_local_label();
                }
            }
            fid += 1;
        }

        Ok(())

    }

    fn find_function_body_addresses(&self, address: usize) -> Vec<usize> {
        let mut visited = HashSet::new();
        let mut locations = vec![address];
        let mut addresses = Vec::new();
        while let Some(mut address) = locations.pop() {

            if visited.contains(&address) {
                continue;
            }
            visited.insert(address);

            loop {
                if let Some(entry) = self.addresses.get(&address) {
                    if let Some(instr) = entry.instruction() {

                        // Avoid merging fallthough functions
                        if entry.is_called() && !addresses.is_empty() {
                            break;
                        }

                        addresses.push(address);
                        let len = instr.layout.len();
                        if instr.name == "ret" && len == 0 {
                            break;

                        } else if instr.name == "reti" {
                            break;

                        } else if (instr.name == "jr" || instr.name == "jp") && len == 1 {
                            break;

                        } else {
                            // Follow conditional jumps to other parts of the function
                            if instr.name == "jr" {
                                let target = (address as i32 + signed_byte(instr.value.unwrap_or(0) as i32)) as usize;
                                locations.push(target);

                            } else if instr.name == "jp" {
                                let target = instr.value.unwrap_or(0) as usize;
                                locations.push(target);
                            }
                            address += instr.size;
                        }

                    } else {
                        break;
                    }

                } else {
                    break;
                }
            }
        }
        addresses
    }

    fn function_addresses(&self) -> Vec<usize> {
        let mut labels = Vec::new();
        for address in 0x0000..0x4000 {
            if let Some(entry) = self.addresses.get(&address) {
                if entry.is_potential_function() {
                    labels.push(address);
                }
            }
        }
        labels
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

    fn read_rom_file<T: FileReader + FileWriter>(&self, io: &mut T, file: &PathBuf) -> Result<(Vec<u8>, Vec<String>), RomError>{
        let (_, contents) = io.read_binary_file(None, &file).map_err(|err| {
            RomError::new(0, format!("Failed to read ROM file \"{}\": {}", err.path.display(), err.io))
        })?;
        let mut symbol_file = file.clone();
        symbol_file.set_extension("sym");
        let symbols = if let Ok((_, symbols)) = io.read_file(None, &symbol_file) {
            symbols.split('\n').map(|s| s.to_string()).collect()

        } else {
            Vec::new()
        };
        Ok((contents, symbols))
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
        byte + 2
    }
}

