// STD Dependencies -----------------------------------------------------------
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use gb_cpu::Instruction;


// Address Entry Abstraction --------------------------------------------------
#[derive(Debug)]
pub struct AddressEntry {
    address: usize,
    instruction: Option<Instruction>,
    label: Option<String>,
    global_label: bool,
    function_id: Option<usize>,
    jumps_from: HashSet<usize>,
    calls_from: HashSet<usize>,
    jumps_to: Option<usize>,
    calls_to: Option<usize>,
    returns_from: Option<usize>
}

impl AddressEntry {

    pub fn new(address: usize) -> Self {
        Self {
            address,
            instruction: None,
            label: None,
            function_id: None,
            global_label: true,
            jumps_from: HashSet::new(),
            calls_from: HashSet::new(),
            jumps_to: None,
            calls_to: None,
            returns_from: None
        }
    }

    pub fn set_label(&mut self, label: String) {
        self.label = Some(label);
    }

    pub fn instruction(&self) -> Option<&Instruction> {
        self.instruction.as_ref()
    }

    pub fn is_function_body(&self) -> bool {
        self.function_id.is_some()
    }

    pub(crate) fn set_function_id(&mut self, uid: usize) {
        self.function_id = Some(uid);
    }

    pub(crate) fn make_local_label(&mut self) {
        self.global_label = false;
    }

    pub(crate) fn is_potential_function(&self) -> bool {
        !self.calls_from.is_empty() || (!self.jumps_from.is_empty() && self.global_label)
    }

    pub(crate) fn has_callers(&self) -> bool {
        !self.calls_from.is_empty()
    }

    pub(crate) fn set_instruction(&mut self, instr: Instruction) {
        self.instruction = Some(instr);
    }

    pub(crate) fn record_jump_from(&mut self, address: usize, always: bool) {
        self.jumps_from.insert(address);
        if !always {
            self.global_label = false;
        }
        if self.label.is_none() {
            self.generate_label();
        }
    }

    pub(crate) fn record_call_from(&mut self, address: usize) {
        self.calls_from.insert(address);
        self.global_label = true;
        if self.label.is_none() {
            self.generate_label();
        }
    }

    pub(crate) fn record_jump_to(&mut self, address: usize) {
        self.jumps_to = Some(address);
    }

    pub(crate) fn record_call_to(&mut self, address: usize) {
        self.calls_to = Some(address);
    }

    pub(crate) fn record_return_from(&mut self, address: usize) {
        self.returns_from = Some(address);
    }

    pub(crate) fn to_string(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        let indent = if self.is_function_body() {
            if let Some(instr) = self.instruction.as_ref() {
                if instr.is_return() && !instr.is_conditional() {
                    " \\  ".to_string()

                } else {
                    " |  ".to_string()
                }

            } else {
                "    ".to_string()
            }

        } else {
            "    ".to_string()
        };
        format!(
            "{}{}0x{:0>4X}{}",
            self.format_header(entries),
            indent,
            self.address,
            self.format_instruction(entries),
        )
    }

    fn format_header(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        let label = self.format_label(entries);
        if label.is_empty() {
            "".to_string()

        } else {
            let callers = self.format_callers(entries);
            let froms = self.format_froms(entries);
            format!("\n{}{}{}", callers, froms, label)
        }
    }

    fn format_callers(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        let callers = Self::callers(entries, self.address);
        if callers.is_empty() {
            "".to_string()

        } else {
            format!("; Callers: {}\n", callers)
        }
    }

    fn format_froms(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        let froms = Self::froms(entries, self.address);
        if froms.is_empty() {
            "".to_string()

        } else {
            format!("; From: {}\n", froms)
        }
    }

    fn format_label(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        if self.label.is_some() {
            let label = Self::address_label(entries, self.address);
            if self.global_label && self.function_id.is_some() {
                format!("func {}:\n", label)

            } else {
                format!("{}:\n", label)
            }

        } else {
            "".to_string()
        }
    }

    fn format_instruction(&self, entries: &HashMap<usize, AddressEntry>) -> String {
        if let Some(instr) = self.instruction.as_ref() {
            let arg_label = if let Some(arg) = instr.address_argument(self.address as u16) {
                Some(Self::address_label(entries, arg as usize))

            } else if let Some(arg) = instr.memory_argument() {
                Some(Self::address_label(entries, arg as usize))

            } else {
                None
            };
            let bytes = instr.to_raw_bytes().into_iter().map(|b| format!("{:0>2X}", b)).collect::<Vec<String>>().join(" ");
            format!("   {: <8}   {}", bytes, instr.to_string(arg_label.as_ref()))

        } else {
            "  ???".to_string()
        }
    }

    fn generate_label(&mut self) {
        self.label = Some(format!("unknown_0x{:0>4X}", self.address));
    }

    fn address_label(entries: &HashMap<usize, AddressEntry>, address: usize) -> String {
        if let Some(entry) = entries.get(&address) {
            if let Some(label) = entry.label.as_ref() {
                if entry.global_label {
                    label.to_string()

                } else {
                    format!(".{}", label)
                }

            } else {
                format!("${:0>4X}", address)
            }

        } else {
            format!("${:0>4X}", address)
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

    fn froms(entries: &HashMap<usize, AddressEntry>, address: usize) -> String {
        if let Some(parent) = entries.get(&address) {
            parent.jumps_from.iter().map(|caller| {
                Self::address_label(entries, *caller)

            }).collect::<Vec<String>>().join(", ")

        } else {
            "???".to_string()
        }
    }

}

