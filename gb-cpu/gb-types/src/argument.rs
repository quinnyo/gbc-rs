// STD Dependencies -----------------------------------------------------------
use std::fmt;


// Internal Dependencies ------------------------------------------------------
use crate::{Register, Flag};


// Instruction Argument -------------------------------------------------------
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
    pub(crate) fn to_string(&self, value: Option<u16>, label: Option<&String>) -> String {
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
    pub fn is_user_provided(&self) -> bool {
        match self {
            Argument::MemoryLookupByteValue => true,
            Argument::MemoryLookupWordValue => true,
            Argument::MemoryLookupRegister(_) => false,
            Argument::ByteValue => true,
            Argument::SignedByteValue => true,
            Argument::WordValue => true,
            Argument::ConstantValue(_) => true,
            Argument::Register(_) => false,
            Argument::Flag(_) => false,
            Argument::Unused => false
        }
    }

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

            "sp+i8" => Some(Argument::SignedByteValue),
            "i8" => Some(Argument::SignedByteValue),

            // Unsigned
            "u8" => Some(Argument::ByteValue),
            "u16" => Some(Argument::WordValue),
            "[ff00+c]" => Some(Argument::MemoryLookupRegister("c".into())),
            "[u16]" => Some(Argument::MemoryLookupWordValue),
            "[u8]" => Some(Argument::MemoryLookupByteValue),
            "[ff00+u8]" => Some(Argument::MemoryLookupByteValue),

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

            // Bit Index
            "0" => Some(Argument::ConstantValue(0)),
            "1" => Some(Argument::ConstantValue(1)),
            "2" => Some(Argument::ConstantValue(2)),
            "3" => Some(Argument::ConstantValue(3)),
            "4" => Some(Argument::ConstantValue(4)),
            "5" => Some(Argument::ConstantValue(5)),
            "6" => Some(Argument::ConstantValue(6)),
            "7" => Some(Argument::ConstantValue(7)),
            "cb" => None,
            a => panic!("Unknown argument type: {}", a)
        }
    }
}

fn signed_byte(byte: i32) -> i32 {
    if byte > 127 {
        byte - 254

    } else {
        byte
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

