// STD Dependencies -----------------------------------------------------------
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use gb_cpu::Instruction;


// Address Entry Abstraction --------------------------------------------------
#[derive(Debug)]
pub struct AddressEntry {
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

    pub fn new(offset: usize) -> Self {
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

    pub fn set_instruction(&mut self, instr: Instruction) {
        self.instruction = Some(instr);
    }

    pub fn record_jump_from(&mut self, address: usize) {
        self.jumps_from.insert(address);
        if self.label.is_none() {
            self.generate_label();
        }
    }

    pub fn record_call_from(&mut self, address: usize) {
        self.calls_from.insert(address);
        if self.label.is_none() {
            self.generate_label();
        }
    }

    pub fn record_jump_to(&mut self, address: usize) {
        self.jumps_to = Some(address);
    }

    pub fn record_call_to(&mut self, address: usize) {
        self.calls_to = Some(address);
    }

    pub fn record_return_from(&mut self, address: usize) {
        self.returns_from = Some(address);
    }

    pub fn set_label(&mut self, label: String) {
        self.label = Some(label);
    }

    pub fn generate_label(&mut self) {
        self.label = Some(format!("unknown_0x{:0>4X}", self.offset));
    }

    pub fn to_string(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        let callers = Self::callers(entries, self.offset);
        format!(
            "{}0x{:0>4X}{}{}",

            // Show Label
            if let Some(label) = self.label.as_ref() {
                // Show callers
                if callers.is_empty() {
                    format!("\n{}:\n    ", label)

                } else {
                    format!("\n; Callers: {}\n{}:\n    ", callers, label)
                }

            } else {
                "    ".to_string()
            },
            self.offset,

            // Show Instruction
            if let Some(instr) = self.instruction.as_ref() {
                format!(
                    "  {: <16}{}{}",
                    instr.to_string(),
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
                "  ???".to_string()
            },

            // Show Parent
            if let Some(parent) = self.returns_from {
                // TODO try and "undo" tail-call optimizations when showing parent label
                format!("; {}", Self::address_label(entries, parent))

            } else {
                "".to_string()
            },

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

    fn callers(entries: &HashMap<usize, AddressEntry>, address: usize) -> String {
        if let Some(parent) = entries.get(&address) {
            parent.calls_from.iter().map(|caller| {
                Self::address_label(entries, *caller)

            }).collect::<Vec<String>>().join(", ")

        } else {
            "???".to_string()
        }
    }
}

