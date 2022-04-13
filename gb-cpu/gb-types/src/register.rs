// STD Dependencies -----------------------------------------------------------
use std::fmt;

// CPU Register ---------------------------------------------------------------
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

    pub fn byte_width(&self) -> usize {
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

    pub fn is_loadable(&self) -> bool {
        matches!(self, Register::Accumulator | Register::B | Register::C | Register::D | Register::E | Register::H | Register::L | Register::BC | Register::DE | Register::HL)
    }

    pub fn to_load_op_code(&self, other: Option<Register>) -> u16 {
        if let Some(reg) = other {
            let base = match self {
                Register::Accumulator => 0x78,
                Register::B => 0x40,
                Register::C => 0x48,
                Register::D => 0x50,
                Register::E => 0x58,
                Register::H => 0x60,
                Register::L => 0x68,
                _ => unreachable!(),
            };
            let offset = match reg {
                Register::Accumulator => 7,
                Register::B => 0,
                Register::C => 1,
                Register::D => 2,
                Register::E => 3,
                Register::H => 4,
                Register::L => 5,
                _ => unreachable!()
            };
            base + offset

        } else {
            match self {
                Register::Accumulator => 0x3E,
                Register::B => 0x06,
                Register::C => 0x0E,
                Register::D => 0x16,
                Register::E => 0x1E,
                Register::H => 0x26,
                Register::L => 0x2E,
                Register::BC => 0x01,
                Register::DE => 0x11,
                Register::HL => 0x21,
                _ => unreachable!(),
            }
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

