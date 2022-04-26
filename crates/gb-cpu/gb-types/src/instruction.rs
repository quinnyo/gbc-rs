// Internal Dependencies ------------------------------------------------------
use crate::Argument;


// Instruction ----------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct Instruction {
    pub code: u16,
    pub value: Option<u16>,
    pub prefix: Option<usize>,
    pub name: &'static str,
    pub size: usize,
    pub layout: [Argument; 2],
    pub argument: Option<Argument>,
    pub offsets: Option<[(usize, u16); 8]>
}

impl Instruction {

    pub fn is_call_op_code(op_code: u16) -> bool {
        op_code == 0xC4 || op_code == 0xD4 ||
        op_code == 0xCC || op_code == 0xDC ||
        op_code == 0xCD
    }

    pub fn is_jump_op_code(op_code: u16) -> bool {
        op_code == 0xC2 || op_code == 0xC3 ||
        op_code == 0xD2 || op_code == 0xCA ||
        op_code == 0xDA || op_code == 0xE9 ||
        op_code == 0x20 || op_code == 0x30 ||
        op_code == 0x28 || op_code == 0x38 ||
        op_code == 0x18
    }

    pub fn does_not_modify_accumulator(op_code: u16) -> bool {
        match op_code {
            0x00..=0x06 => true,
            0x08..=0x09 => true,
            0x0B..=0x0E => true,

            0x10..=0x16 => true,
            0x19 => true,
            0x1B..=0x1E => true,

            0x21..=0x26 => true,
            0x29 => true,
            0x2B..=0x2E => true,

            0x31..=0x36 => true,
            0x39 => true,
            0x3B => true,

            0x40..=0x77 => true,
            0xA7 => true,
            0xB7..=0xBF => true,

            0xC0 | 0xC1 | 0xC5 | 0xC8 | 0xC9 => true,
            0xD0 | 0xD1 | 0xD5 | 0xD8 => true,
            0xE0 | 0xE1 | 0xE2 | 0xE5 | 0xE8 | 0xEA => true,
            0xF8 | 0xF9 | 0xFE => true,
            // TODO CB prefix
            _ => false
        }
    }

    pub fn is_accumlator_store_into(op_code: u16) -> bool {
        op_code == 0x02 || op_code == 0x12 ||
        op_code == 0x22 || op_code == 0x32 ||
        op_code == 0x47 || op_code == 0x4F ||
        op_code == 0x57 || op_code == 0x5F ||
        op_code == 0x67 || op_code == 0x6F ||
        op_code == 0x77 || op_code == 0x7F ||
        op_code == 0xE0 || op_code == 0xE2 ||
        op_code == 0xEA
    }

    pub fn is_memory_read_op_code(op_code: u16) -> bool {
        op_code == 0xF0 || op_code == 0xFA
    }

    pub fn is_memory_write_op_code(op_code: u16) -> bool {
        op_code == 0xE0 || op_code == 0xEA || op_code == 0x08
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        if self.code == 16 {
            // Pad STOP with a NOP
            vec![16, 0]

        } else if self.code > 255 {
            vec![0xCB, (self.code % 256) as u8]

        } else {
            vec![self.code as u8]
        }
    }

    pub fn to_raw_bytes(&self) -> Vec<u8> {
        if self.code > 255 {
            vec![0xCB, (self.code % 256) as u8]

        } else if self.size == 1 {
            vec![self.code as u8]

        } else if self.size == 2 {
            vec![self.code as u8, self.value.unwrap_or(0) as u8]

        } else {
            let value = self.value.unwrap_or(0);
            vec![self.code as u8, value as u8, (value >> 8) as u8]
        }
    }

    pub fn is_jump(&self) -> bool {
        self.is_absolute_jump() || self.is_relative_jump()
    }

    pub fn is_relative_jump(&self) -> bool {
        self.name == "jr"
    }

    pub fn is_absolute_jump(&self) -> bool {
        self.name == "jp"
    }

    pub fn is_call(&self) -> bool {
        self.name == "call"
    }

    pub fn is_return(&self) -> bool {
        self.name == "ret" || self.name == "reti"
    }

    pub fn target(&self, origin: usize) -> usize {
        self.address_argument(origin as u16).unwrap_or(0) as usize
    }

    pub fn memory_argument(&self) -> Option<u16> {
        if let Argument::MemoryLookupWordValue = self.layout[0] {
            Some(self.value.unwrap_or(0))

        } else if let Argument::MemoryLookupWordValue = self.layout[1] {
            Some(self.value.unwrap_or(0))

        } else if let Argument::MemoryLookupByteValue = self.layout[0] {
            Some(0xFF00 + self.value.unwrap_or(0))

        } else if let Argument::MemoryLookupByteValue = self.layout[1] {
            Some(0xFF00 + self.value.unwrap_or(0))

        } else {
            None
        }
    }

    pub fn address_argument(&self, origin: u16) -> Option<u16> {
        if self.is_relative_jump() {
            Some((i32::from(origin) + signed_jump(i32::from(self.value.unwrap_or(0)))) as u16)

        } else if self.is_absolute_jump() || self.is_call() {
            Some(self.value.unwrap_or(0))

        } else {
            None
        }
    }

    pub fn is_conditional(&self) -> bool {
        if self.is_jump() || self.is_call() {
            self.layout[1] != Argument::Unused

        } else if self.is_return() {
            self.layout[0] != Argument::Unused

        } else {
            false
        }
    }

    pub fn decode(
        buffer: &[u8],
        instructions: &[Instruction],
        extended: bool

    ) -> Option<Instruction> {
        if buffer.is_empty() {
            None

        } else {
            match buffer[0] {
                0xCB => Self::decode(&buffer[1..], instructions, true),
                op => if extended {
                    instructions[256 + op as usize].clone().with_argument(&buffer[1..])

                } else {
                    instructions[op as usize].clone().with_argument(&buffer[1..])
                }
            }
        }
    }

    pub fn to_string(&self, label: Option<&String>) -> String {
        format!("{: <8} {}", self.name, self.layout.iter().filter(|arg| *arg != &Argument::Unused).map(|arg| {
            arg.to_string(self.value, label)

        }).collect::<Vec<String>>().join(", "))
    }

    fn with_argument(mut self, buffer: &[u8]) -> Option<Self> {
        if buffer.len() < self.size - 1 {
            Some(self)

        } else if self.size == 2 {
            self.value = Some(u16::from(buffer[0]));
            Some(self)

        } else if self.size == 3 {
            self.value = Some(u16::from(buffer[0]) | (u16::from(buffer[1])) << 8);
            Some(self)

        } else {
            Some(self)
        }
    }

}


// Helpers --------------------------------------------------------------------
fn signed_jump(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte + 2
    }
}

