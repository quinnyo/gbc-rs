// STD Dependencies -----------------------------------------------------------
use std::fmt;

// Modules --------------------------------------------------------------------
// mod instructions;

// Flags ----------------------------------------------------------------------
#[derive(Eq, PartialEq)]
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

#[derive(Eq, PartialEq)]
pub enum FlagModifier {
    Untouched,
    Set,
    Unset,
    Operation
}

impl From<&str> for FlagModifier {
    fn from(s: &str) -> Self {
        match s {
            "0" => FlagModifier::Unset,
            "1" => FlagModifier::Set,
            "-" => FlagModifier::Untouched,
            _ => FlagModifier::Operation
        }
    }
}

impl fmt::Debug for FlagModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FlagModifier::Untouched => write!(f, "FlagModifier::Untouched"),
            FlagModifier::Set => write!(f, "FlagModifier::Set"),
            FlagModifier::Unset => write!(f, "FlagModifier::Unset"),
            FlagModifier::Operation => write!(f, "FlagModifier::Operation")
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FlagState {
    pub zero: FlagModifier,
    pub negative: FlagModifier,
    pub carry: FlagModifier,
    pub half_carry: FlagModifier
}


// Registers ------------------------------------------------------------------
#[derive(Eq, PartialEq)]
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


// Instructions ---------------------------------------------------------------
#[derive(Debug)]
pub struct Instruction {
    pub op_code: usize,
    pub prefix: Option<usize>,
    pub mnemonic: String,
    pub cycles: usize,
    pub cycles_min: Option<usize>,
    pub args: Vec<Argument>,
    pub flags: FlagState
}

// TODO Calculate size from data

pub enum Argument {
    /// A Flag
    Flag(Flag),

    /// A hard coded value
    Integer(usize),

    /// Value in register
    Register(Register),

    /// r8: -128 - 127 forced signage
    ByteSigned,

    /// d16: 0 - 65535 converted to two's compliment
    Word,

    /// d8: 0 - 255 converted to two's compliment
    Byte,

    /// [r]: Memory addressing via value of register
    MemoryLookupRegister(Register),

    /// a16: Memory addressing via value of word
    MemoryLookupWord,

    /// a8: Memory addressing via value of byte
    MemoryLookupByte
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::Flag(l) => {
                write!(f, "Argument::Flag({:?})", l)
            },
            Argument::Integer(v) => write!(f, "Argument::Integer({})", v),
            Argument::Register(r) => {
                write!(f, "Argument::Register({:?})", r)
            },
            Argument::MemoryLookupRegister(r) => {
                write!(f, "Argument::MemoryLookupRegister({:?})", r)
            },
            Argument::ByteSigned => write!(f, "Argument::ByteSigned"),
            Argument::Word => write!(f, "Argument::Word"),
            Argument::Byte => write!(f, "Argument::Byte"),
            Argument::MemoryLookupWord => write!(f, "Argument::MemoryLookupWord"),
            Argument::MemoryLookupByte => write!(f, "Argument::MemoryLookupByte"),
        }
    }
}

impl Argument {
    pub fn from(a: &str, mnemonic: &str) -> Option<Self> {
        let cond = mnemonic == "jr" || mnemonic == "jp" || mnemonic == "call";
        match a {
            "a8" => Some(Argument::MemoryLookupByte),
            "a16" => Some(Argument::MemoryLookupWord),
            "d8" => Some(Argument::Byte),
            "d16" => Some(Argument::Word),
            "r8" => Some(Argument::ByteSigned),
            "a" | "b" | "d" | "e" | "h" | "l" |
            "af" | "bc" | "de" | "hl" | "sp" => Some(Argument::Register(a.into())),
            "c" if cond => Some(Argument::Flag(a.into())),
            "c" if !cond => Some(Argument::Register(a.into())),
            "nz" | "nc" | "z" => Some(Argument::Flag(a.into())),
            "[bc]" | "[de]" | "[hl]" => Some(Argument::MemoryLookupRegister(a[1..3].into())),
            "[hli]" => Some(Argument::MemoryLookupRegister("hli".into())),
            "[hld]" => Some(Argument::MemoryLookupRegister("hld".into())),
            "[c]" => Some(Argument::MemoryLookupRegister("c".into())),
            "[a16]" => Some(Argument::MemoryLookupWord),
            "[a8]" => Some(Argument::MemoryLookupByte),
            "00h" => Some(Argument::Integer(0)),
            "08h" => Some(Argument::Integer(8)),
            "10h" => Some(Argument::Integer(16)),
            "18h" => Some(Argument::Integer(24)),
            "20h" => Some(Argument::Integer(32)),
            "28h" => Some(Argument::Integer(40)),
            "30h" => Some(Argument::Integer(48)),
            "38h" => Some(Argument::Integer(56)),
            "sp+r8" => Some(Argument::ByteSigned),
            "0" => None,
            "cb" => None,
            a => panic!("Unknown argument type: {}", a)
        }
    }
}

