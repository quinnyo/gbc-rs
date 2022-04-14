// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use gb_cpu::{Instruction, self};
use file_io::{FileReader, FileWriter, Logger};


// Modules --------------------------------------------------------------------
mod address_entry;
mod error;

use self::address_entry::AddressEntry;
use self::error::{DecompilationError, RomError};


// GameBoy Instruction Decompiler ---------------------------------------------
pub struct Decompiler {
    instructions: Vec<Instruction>,
    addresses: HashMap<usize, AddressEntry>,
    visited: HashSet<usize>
}

impl Decompiler {

    pub fn new() -> Self {
        Self {
            instructions: gb_cpu::instruction_list(),
            addresses: HashMap::new(),
            visited: HashSet::new()
        }
    }

    pub fn decompile_file<T: FileReader + FileWriter>(
        &mut self,
        logger: &mut Logger,
        io: &mut T,
        file: PathBuf

    ) -> Result<(), DecompilationError> {

        self.address_entry(0x0040).set_label("IV_VBLANK".to_string());
        self.address_entry(0x0040).record_call_from(0xFFFF);
        self.address_entry(0x0050).set_label("IV_TIMER".to_string());
        self.address_entry(0x0050).record_call_from(0xFFFF);
        self.address_entry(0x0058).set_label("IV_SERIAL".to_string());
        self.address_entry(0x0100).set_label("BOOT_ROM_EXIT".to_string());

        let (rom_buffer, symbols) = self.read_rom_file(io, &file).map_err(|e| DecompilationError::new("instruction parsing", e))?;

        let mut known_addresses = vec![
            (0x0100, Some(0x0100)),
            (0x0040, None),
            (0x0050, None),
            (0x0058, None),
            // TODO include known parent labels for cases where they are never called directly
            // (0x1B64, None, Some(0x1B64)),
        ];

        // TODO differentiate betweeen functions and data locations
        // TODO tweak the symbol file?
        known_addresses.append(&mut self.parse_symbols(symbols));

        self.analyze_from(&rom_buffer, known_addresses).map_err(|e| DecompilationError::new("instruction parsing", e))?;

        // TODO detect data blocks
        // TODO handle msg / brk instructions in instruction decoder
        // TODO display known argument labels inline

        for adr in self.range_to_string(0x0000, 0x4000) {
            logger.log(adr);
        }
        Ok(())
    }

    fn parse_symbols(&mut self, symbols: Vec<String>) -> Vec<(usize, Option<usize>)> {
        let mut addresses = Vec::new();
        for s in symbols {
            let mut line = s.split(' ');
            if let (Some(location), Some(name)) = (line.next(), line.next()) {
                let mut location = location.split(':');
                if let (Some(bank), Some(address)) = (location.next(), location.next()) {
                    if let Ok(bank) = u16::from_str_radix(bank, 16) {
                        if let Ok(addr) = u16::from_str_radix(address, 16) {
                            let addr = addr as usize + bank as usize * 0x4000;
                            self.address_entry(addr).set_label(name.to_string());
                            addresses.push((addr, None));
                        }
                    }
                }
            }
        }
        addresses
    }

    fn analyze_from(
        &mut self,
        rom_buffer: &[u8],
        mut call_stack: Vec<(usize, Option<usize>)>,

    ) -> Result<(), RomError> {
        while let Some((mut address, callee_address)) = call_stack.pop() {

            // Ignore addresses outside of ROM space
            if address > rom_buffer.len() - 1 {
                // TODO handle bank mapping
                continue;
            }

            while let Some(instr) = Instruction::decode(&rom_buffer[address..], &self.instructions, false) {

                // TODO detect debug messages and breakpoints
                let branching = if instr.is_relative_jump() {
                    let target = instr.target(address);

                    // Backwards jumps are considered to be always taken
                    self.address_entry(target).record_jump_from(address, !instr.is_conditional() || target <= address);
                    self.address_entry(address).record_jump_to(target);
                    if !self.address_visited(target) {
                        call_stack.push((target, callee_address));
                    }
                    instr.is_conditional()

                // TODO check for obvious jump tables in case of jp [hl] and parse their addresses
                } else if instr.is_absolute_jump() {
                    let target = instr.target(address);
                    self.address_entry(target).record_jump_from(address, !instr.is_conditional());
                    self.address_entry(address).record_jump_to(target);
                    if !self.address_visited(target) {
                        call_stack.push((target, callee_address ));
                    }
                    instr.is_conditional()

                } else if instr.is_call() {
                    let target = instr.target(address);
                    self.address_entry(target).record_call_from(address);
                    self.address_entry(address).record_call_to(target);
                    call_stack.push((target, Some(target)));
                    true

                } else if instr.is_return() {
                    if let Some(callee) = callee_address {
                        self.address_entry(address).record_return_from(callee);
                    }
                    instr.is_conditional()

                } else {
                    true
                };

                let size = instr.size;
                self.address_entry(address).set_instruction(instr);
                address += size;

                if !branching {
                    break;
                }
            }
        }

        // Find function bodies
        let mut id = 0;
        for address in self.function_addresses() {
            let addresses = self.find_function_body_addresses(address);
            for (index, address) in addresses.into_iter().enumerate() {
                let entry = self.address_entry(address);
                entry.set_function_id(id);
                if index > 0 {
                    // Mark all inner labels as local
                    entry.make_local_label();
                }
            }
            id += 1;
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

            } else {
                visited.insert(address);
            }

            loop {
                if let Some(entry) = self.addresses.get(&address) {
                    if let Some(instr) = entry.instruction() {

                        // Avoid merging fallthough functions
                        if entry.has_callers() && !addresses.is_empty() {
                            break;
                        }

                        addresses.push(address);

                        // Detect and of function body
                        let len = instr.layout.len();
                        if instr.name == "ret" && len == 0 {
                            break;

                        } else if instr.name == "reti" {
                            break;

                        } else if instr.is_jump() && len == 1 {
                            break;

                        // Follow conditional jumps to other parts of the function body
                        } else {
                            if instr.is_relative_jump() {
                                let target = (address as i32 + signed_byte(instr.value.unwrap_or(0) as i32)) as usize;
                                locations.push(target);

                            } else if instr.is_absolute_jump() {
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

}

fn signed_byte(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte + 2
    }
}

