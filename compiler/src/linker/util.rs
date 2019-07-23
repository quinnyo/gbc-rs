// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;
use gb_cpu::Instruction;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use crate::lexer::InnerToken;
use crate::expression::ExpressionResult;

// Statics --------------------------------------------------------------------
lazy_static! {
    static ref INSTRUCTIONS: Vec<Instruction> = gb_cpu::instruction_list();
}

// Linker and Section Helpers -------------------------------------------------
pub fn integer_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<i32, SourceError> {
    match result {
        ExpressionResult::Integer(i) => {
            Ok(i)
        },
        _ => {
            Err(inner.error(format!("{}, expected a integer value instead.", msg)))
        }
    }
}

pub fn byte_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<u8, SourceError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -128 && i <= 255 {
                Ok(to_twos_byte(i))

            } else {
                Err(inner.error(format!("{} ({}), expected a byte value in the range of -128 to 255 instead.", msg, i)))
            }
        },
        _ => {
            Err(inner.error(format!("{}, expected a byte value in the range of -128 to 255 instead.", msg)))
        }
    }
}

pub fn word_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<u16, SourceError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -32768 && i <= 65535 {
                Ok(to_twos_word(i))

            } else {
                Err(inner.error(format!("{} ({}), expected a word value in the range of -32768 to 65535 instead.", msg, i)))
            }
        },
        _ => {
            Err(inner.error(format!("{}, expected a word value in the range of -32768 to 65535 instead.", msg)))
        }
    }
}

pub fn address_word_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<i32, SourceError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -65536 && i <= 65535 {
                Ok(i as i32)

            } else {
                Err(inner.error(format!("{} ({}), expected a word value in the range of -65536 to 65535 instead.", msg, i)))
            }
        },
        _ => {
            Err(inner.error(format!("{}, expected a word value in the range of -65536 to 65535 instead.", msg)))
        }
    }
}

pub fn signed_byte_value(
    inner: &InnerToken,
    i: i32,
    msg: &'static str

) -> Result<u8, SourceError> {
    if i >= -128 && i <= 127 {
        Ok(to_twos_byte(i))

    } else {
        Err(inner.error(format!("{}, expected a signed byte value in the range of -128 to 127 instead.", msg)))
    }
}

pub fn opt_string(
    inner: &InnerToken,
    result: Option<ExpressionResult>,
    msg: &'static str

) -> Result<Option<String>, SourceError> {
    match result {
        Some(ExpressionResult::String(s)) => {
            Ok(Some(s))
        },
        None => Ok(None),
        _ => {
            Err(inner.error(format!("{}, expected a string value instead.", msg)))
        }
    }
}

pub fn opt_integer(
    inner: &InnerToken,
    result: Option<ExpressionResult>,
    msg: &'static str

) -> Result<Option<usize>, SourceError> {
    match result {
        Some(ExpressionResult::Integer(i)) => {
            Ok(Some(positive_integer(inner, i, msg)?))
        },
        None => Ok(None),
        _ => {
            Err(inner.error(format!("{}, expected a positive integer value instead.", msg)))
        }
    }
}

pub fn positive_integer(
    inner: &InnerToken,
    i: i32,
    msg: &'static str

) -> Result<usize, SourceError> {
    if i >= 0 {
        Ok(i as usize)

    } else {
        Err(inner.error(format!("{}, expected a positive integer value instead.", msg)))
    }
}

fn to_twos_byte(i: i32) -> u8 {
    if i < 0 {
        (256 - (-i as u16)) as u8

    } else {
        i as u8
    }
}

fn to_twos_word(i: i32) -> u16 {
    if i < 0 {
        (65536 - (-i as u32)) as u16

    } else {
        i as u16
    }
}



// Helpers --------------------------------------------------------------------
pub mod instruction {
    use super::INSTRUCTIONS;
    use gb_cpu::Argument;

    pub fn jump_address(end_of_instruction: usize, bytes: &[u8]) -> Option<usize> {
        match bytes[0] {
            // jp
            0xC2 | 0xC3 | 0xCA | 0xD2 | 0xDA => {
                Some(bytes[1] as usize | ((bytes[2] as usize) << 8))
            },
            // jr
            0x18 | 0x20 | 0x28 | 0x30 | 0x38 => {
                Some((end_of_instruction as i32 + from_twos_byte(bytes[1])) as usize)
            },
            _ => None
        }
    }

    pub fn size(op_code: u16) -> usize {
        INSTRUCTIONS[op_code as usize].size
    }

    pub fn bytes(op_code: u16) -> Vec<u8> {
        INSTRUCTIONS[op_code as usize].to_bytes()
    }

    pub fn offsets(op_code: u16) -> Option<&'static Vec<(usize, u16)>> {
        INSTRUCTIONS[op_code as usize].offsets.as_ref()
    }

    pub fn argument(op_code: u16) -> Option<&'static Argument> {
        INSTRUCTIONS[op_code as usize].argument.as_ref()
    }

    fn from_twos_byte(i: u8) -> i32 {
        if i > 127 {
            i32::from(i) - 256

        } else {
            i32::from(i)
        }
    }

}

