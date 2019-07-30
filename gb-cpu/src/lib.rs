// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::convert::Into;


// Modules --------------------------------------------------------------------
mod instructions;
pub use self::instructions::instruction_max_arg_count;


// Functions ------------------------------------------------------------------
pub fn instruction_list() -> Vec<Instruction> {
    instructions::instructions()
}

pub fn instruction_is_conditional(mnemonic: &str) -> bool {
    match mnemonic {
        "jr" | "jp" | "call" | "ret" => true,
        _ => false
    }
}

// Flags ----------------------------------------------------------------------
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Flag {
    Zero,
    NoZero,
    Carry,
    NoCarry
}

impl From<&str> for Flag {
    fn from(s: &str) -> Self {
        match s {
            "z" => Flag::Zero,
            "nz" => Flag::NoZero,
            "c" => Flag::Carry,
            "nc" => Flag::NoCarry,
            f => unreachable!("Invalid flag: {}", f)
        }
    }
}

impl fmt::Debug for Flag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Flag::Zero => "Flag::Zero",
            Flag::NoZero => "Flag::NoZero",
            Flag::Carry => "Flag::Carry",
            Flag::NoCarry => "Flag::NoCarry",
        })
    }
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Flag::Zero => "z",
            Flag::NoZero => "nz",
            Flag::Carry => "c",
            Flag::NoCarry => "nc",
        })
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum FlagModifier {
    Keep,
    Set,
    Clear,
    Result
}

impl From<&str> for FlagModifier {
    fn from(s: &str) -> Self {
        match s {
            "0" => FlagModifier::Clear,
            "1" => FlagModifier::Set,
            "-" => FlagModifier::Keep,
            _ => FlagModifier::Result
        }
    }
}

impl fmt::Debug for FlagModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FlagModifier::Keep => write!(f, "FlagModifier::Keep"),
            FlagModifier::Set => write!(f, "FlagModifier::Set"),
            FlagModifier::Clear => write!(f, "FlagModifier::Clear"),
            FlagModifier::Result => write!(f, "FlagModifier::Result")
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FlagState {
    pub z: FlagModifier,
    pub n: FlagModifier,
    pub c: FlagModifier,
    pub h: FlagModifier
}


// Registers ------------------------------------------------------------------
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Register {
    Accumulator,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
    HLIncrement,
    HLDecrement,
    SP
}

impl Register {
    pub fn to_pair(&self) -> (Register, Register) {
        match self {
            Register::Accumulator => (Register::Accumulator, Register::Accumulator),
            Register::B => (Register::B, Register::B),
            Register::C => (Register::C, Register::C),
            Register::D => (Register::D, Register::D),
            Register::E => (Register::E, Register::E),
            Register::H => (Register::H, Register::H),
            Register::L => (Register::L, Register::L),
            Register::AF => (Register::Accumulator, Register::Accumulator),
            Register::BC => (Register::B, Register::C),
            Register::DE => (Register::D, Register::E),
            Register::HL => (Register::H, Register::L),
            Register::HLIncrement => (Register::H, Register::L),
            Register::HLDecrement => (Register::H, Register::L),
            Register::SP => (Register::Accumulator, Register::Accumulator)
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Register::Accumulator => 1,
            Register::B => 1,
            Register::C => 1,
            Register::D => 1,
            Register::E => 1,
            Register::H => 1,
            Register::L => 1,
            Register::AF => 2,
            Register::BC => 2,
            Register::DE => 2,
            Register::HL => 2,
            Register::HLIncrement => 2,
            Register::HLDecrement => 2,
            Register::SP => 2,
        }
    }

    pub fn instruction_offset(&self) -> u16 {
        match self {
            Register::Accumulator => 7,
            Register::B => 0,
            Register::C => 1,
            Register::D => 2,
            Register::E => 3,
            Register::H => 4,
            Register::L => 5,
            Register::AF => 48,
            Register::BC => 0,
            Register::DE => 16,
            Register::HL => 32,
            Register::HLIncrement => 0,
            Register::HLDecrement => 0,
            Register::SP => 0,
        }
    }

    pub fn instruction_offset_into_a(&self) -> u16 {
        match self {
            Register::Accumulator => 0,
            Register::B => 0,
            Register::C => 8,
            Register::D => 16,
            Register::E => 24,
            Register::H => 32,
            Register::L => 40,
            Register::AF => 0,
            Register::BC => 0,
            Register::DE => 0,
            Register::HL => 0,
            Register::HLIncrement => 0,
            Register::HLDecrement => 0,
            Register::SP => 0,
        }

    }

}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        match s {
            "a" => Register::Accumulator,
            "b" => Register::B,
            "c" => Register::C,
            "d" => Register::D,
            "e" => Register::E,
            "h" => Register::H,
            "l" => Register::L,
            "af" => Register::AF,
            "bc" => Register::BC,
            "de" => Register::DE,
            "hl" => Register::HL,
            "hli" => Register::HLIncrement,
            "hld" => Register::HLDecrement,
            "sp" => Register::SP,
            r => unreachable!("Invalid Register: {}", r)
        }
    }
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Register::Accumulator => "Register::Accumulator",
            Register::B => "Register::B",
            Register::C => "Register::C",
            Register::D => "Register::D",
            Register::E => "Register::E",
            Register::H => "Register::H",
            Register::L => "Register::L",
            Register::AF => "Register::AF",
            Register::BC => "Register::BC",
            Register::DE => "Register::DE",
            Register::HL => "Register::HL",
            Register::HLIncrement => "Register::HLIncrement",
            Register::HLDecrement => "Register::HLDecrement",
            Register::SP => "Register::SP"
        })
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Register::Accumulator => "a",
            Register::B => "b",
            Register::C => "c",
            Register::D => "d",
            Register::E => "e",
            Register::H => "h",
            Register::L => "l",
            Register::AF => "af",
            Register::BC => "bc",
            Register::DE => "de",
            Register::HL => "hl",
            Register::HLIncrement => "hli",
            Register::HLDecrement => "hld",
            Register::SP => "sp"
        })
    }
}


// Instructions ---------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct Instruction {
    pub code: u16,
    pub value: Option<u16>,
    pub prefix: Option<usize>,
    pub name: &'static str,
    pub size: usize,
    pub cycles: usize,
    pub cycles_min: Option<usize>,
    pub layout: [Argument; 2],
    pub argument: Option<Argument>,
    pub offsets: Option<[(usize, u16); 8]>,
    pub flags: FlagState
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
            Some((origin as i32 + signed_jump(self.value.unwrap_or(0) as i32)) as u16)

        } else if self.is_absolute_jump() {
            Some(self.value.unwrap_or(0))

        } else if self.is_call() {
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
            self.value = Some(buffer[0] as u16);
            Some(self)

        } else if self.size == 3 {
            self.value = Some(buffer[0] as u16 | (buffer[1] as u16) << 8);
            Some(self)

        } else {
            Some(self)
        }
    }

}

#[derive(Hash, Eq, PartialEq, Clone)]
pub enum LexerArgument {
    MemoryLookupValue,
    MemoryLookupRegister(Register),
    Value,
    Register(Register),
    Flag(Flag)
}

impl fmt::Display for LexerArgument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            LexerArgument::MemoryLookupValue => "[<value>]".to_string(),
            LexerArgument::MemoryLookupRegister(r) => format!("[{}]", r),
            LexerArgument::Value => "<value>".to_string(),
            LexerArgument::Register(r) => r.to_string(),
            LexerArgument::Flag(f) => f.to_string()
        })
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum Argument {
    MemoryLookupByteValue,
    MemoryLookupWordValue,
    MemoryLookupRegister(Register),
    ByteValue,
    SignedByteValue,
    WordValue,
    ConstantValue(usize),
    Register(Register),
    Flag(Flag),
    Unused
}

impl Argument {
    fn to_string(&self, value: Option<u16>, label: Option<&String>) -> String {
        match self {
            Argument::MemoryLookupByteValue => {
                if let Some(label) = label {
                    format!("[{}]", label)

                } else {
                    format!("[${:0>2X}]", value.unwrap_or(0))
                }
            },
            Argument::MemoryLookupWordValue => {
                if let Some(label) = label {
                    format!("[{}]", label)

                } else {
                    format!("[${:0>4X}]", value.unwrap_or(0))
                }
            },
            Argument::SignedByteValue => {
                if let Some(label) = label {
                    format!("{}", label)

                } else {
                    format!("{}", signed_byte(value.unwrap_or(0) as i32))
                }
            },
            Argument::MemoryLookupRegister(r) => format!("[{}]", r),
            Argument::ByteValue => format!("${:0>2X}", value.unwrap_or(0)),
            Argument::WordValue => {
                if let Some(label) = label {
                    format!("{}", label)

                } else {
                    format!("${:0>4X}", value.unwrap_or(0))
                }
            },
            Argument::ConstantValue(c) => if *c < 16 {
                format!("{}", c)

            } else {
                format!("${:0>2X}", c)
            },
            Argument::Register(r) => format!("{}", r),
            Argument::Flag(f) => format!("{}", f),
            Argument::Unused => "".to_string()
        }
    }
}

impl Into<LexerArgument> for Argument {
    fn into(self) -> LexerArgument {
        match self {
            Argument::MemoryLookupByteValue => LexerArgument::MemoryLookupValue,
            Argument::MemoryLookupWordValue => LexerArgument::MemoryLookupValue,
            Argument::MemoryLookupRegister(r) => LexerArgument::MemoryLookupRegister(r.clone()),
            Argument::ByteValue => LexerArgument::Value,
            Argument::SignedByteValue => LexerArgument::Value,
            Argument::WordValue => LexerArgument::Value,
            Argument::ConstantValue(_) => LexerArgument::Value,
            Argument::Register(r) => LexerArgument::Register(r.clone()),
            Argument::Flag(f) => LexerArgument::Flag(f.clone()),
            Argument::Unused => LexerArgument::Value
        }
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::MemoryLookupByteValue => write!(f, "Argument::MemoryLookupByteValue"),
            Argument::MemoryLookupWordValue => write!(f, "Argument::MemoryLookupWordValue"),
            Argument::MemoryLookupRegister(r) => write!(f, "Argument::MemoryLookupRegister({:?})", r),
            Argument::ByteValue => write!(f, "Argument::ByteValue"),
            Argument::SignedByteValue => write!(f, "Argument::SignedByteValue"),
            Argument::WordValue => write!(f, "Argument::WordValue"),
            Argument::ConstantValue(v) => write!(f, "Argument::ConstantValue({})", v),
            Argument::Register(r) => write!(f, "Argument::Register({:?})", r),
            Argument::Flag(r) => write!(f, "Argument::Flag({:?})", r),
            Argument::Unused => write!(f, "Argument::Unused")
        }
    }
}

impl Argument {
    pub fn from(a: &str, mnemonic: &str) -> Option<Self> {
        let cond = mnemonic == "jr" || mnemonic == "jp" || mnemonic == "call" || mnemonic == "ret";
        match a {
            "00h" => Some(Argument::ConstantValue(0)),
            "08h" => Some(Argument::ConstantValue(8)),
            "10h" => Some(Argument::ConstantValue(16)),
            "18h" => Some(Argument::ConstantValue(24)),
            "20h" => Some(Argument::ConstantValue(32)),
            "28h" => Some(Argument::ConstantValue(40)),
            "30h" => Some(Argument::ConstantValue(48)),
            "38h" => Some(Argument::ConstantValue(56)),

            "sp+r8" => Some(Argument::SignedByteValue),
            "r8" => Some(Argument::SignedByteValue),

            // Unsigned
            "a8" => Some(Argument::ByteValue),
            "a16" => Some(Argument::WordValue),
            "d8" => Some(Argument::ByteValue),
            "d16" => Some(Argument::WordValue),
            "[c]" => Some(Argument::MemoryLookupRegister("c".into())),
            "[a16]" => Some(Argument::MemoryLookupWordValue),
            "[a8]" => Some(Argument::MemoryLookupByteValue),

            // Registers
            "a" | "b" | "d" | "e" | "h" | "l" |
            "af" | "bc" | "de" | "hl" | "sp" => Some(Argument::Register(a.into())),
            "c" if !cond => Some(Argument::Register(a.into())),
            "[bc]" | "[de]" | "[hl]" => Some(Argument::MemoryLookupRegister(a[1..3].into())),
            "[hli]" => Some(Argument::MemoryLookupRegister("hli".into())),
            "[hld]" => Some(Argument::MemoryLookupRegister("hld".into())),

            // Flags
            "c" if cond => Some(Argument::Flag(a.into())),
            "nz" | "nc" | "z" => Some(Argument::Flag(a.into())),

            // Ignored
            "0" => None,
            "cb" => None,
            a => panic!("Unknown argument type: {}", a)
        }
    }
}

fn signed_jump(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte + 2
    }
}

fn signed_byte(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte
    }
}

