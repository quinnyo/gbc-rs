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

