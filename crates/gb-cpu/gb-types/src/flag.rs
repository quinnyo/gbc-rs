// STD Dependencies -----------------------------------------------------------
use std::fmt;

// CPU Flag -------------------------------------------------------------------
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


