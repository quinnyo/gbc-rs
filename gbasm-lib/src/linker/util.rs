// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;
use gbasm_cpu::Instruction;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, LexerError};
use crate::expression::ExpressionResult;

// Statics --------------------------------------------------------------------
lazy_static! {
    static ref INSTRUCTIONS: Vec<Instruction> = gbasm_cpu::instruction_list();
}

// Linker and Section Helpers -------------------------------------------------
pub fn constant_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<i32, LexerError> {
    match result {
        ExpressionResult::Integer(i) => {
            Ok(i)
        },
        _ => {
            Err(inner.error(format!("{}, expected a interger value instead.", msg)))
        }
    }
}

pub fn byte_value(
    inner: &InnerToken,
    result: ExpressionResult,
    msg: &'static str

) -> Result<u8, LexerError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -128 && i <= 255 {
                Ok(to_twos_byte(i))

            } else {
                Err(inner.error(format!("{}, expected a byte value in the range of -128 to 255 instead.", msg)))
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

) -> Result<u16, LexerError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -32768 && i <= 65535 {
                Ok(to_twos_word(i))

            } else {
                Err(inner.error(format!("{}, expected a word value in the range of -32768 to 65535 instead.", msg)))
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

) -> Result<i32, LexerError> {
    match result {
        ExpressionResult::Integer(i) => {
            if i >= -65536 && i <= 65535 {
                Ok(i as i32)

            } else {
                Err(inner.error(format!("{}, expected a word value in the range of -65536 to 65535 instead.", msg)))
            }
        },
        _ => {
            Err(inner.error(format!("{}, expected a word value in the range of -65536 to 65535 instead.", msg)))
        }
    }
}

pub fn signed_byte_value(
    inner: &InnerToken,
    value: i32,
    msg: &'static str

) -> Result<u8, LexerError> {
    if value >= -128 && value <= 127 {
        Ok(to_twos_byte(value))

    } else {
        Err(inner.error(format!("{}, expected a signed byte value in the range of -128 to 127 instead.", msg)))
    }
}

pub fn opt_string(
    inner: &InnerToken,
    result: Option<ExpressionResult>,
    msg: &'static str

) -> Result<Option<String>, LexerError> {
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

) -> Result<Option<usize>, LexerError> {
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

) -> Result<usize, LexerError> {
    if i >= 0 {
        Ok(i as usize)

    } else {
        Err(inner.error(format!("{}, expected a positive integer value instead.", msg)))
    }
}

pub fn to_twos_byte(i: i32) -> u8 {
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
    use gbasm_cpu::Argument;

    pub fn size(op_code: usize) -> usize {
        INSTRUCTIONS[op_code].size
    }

    pub fn bytes(op_code: usize) -> Vec<u8> {
        INSTRUCTIONS[op_code].to_bytes()
    }

    pub fn offsets(op_code: usize) -> Option<&'static Vec<(usize, usize)>> {
        INSTRUCTIONS[op_code].offsets.as_ref()
    }

    pub fn argument(op_code: usize) -> Option<&'static Argument> {
        INSTRUCTIONS[op_code].argument.as_ref()
    }
}

