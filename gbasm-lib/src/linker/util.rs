// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, LexerError};
use crate::expression::ExpressionResult;


// Linker Data Evaluation Utilities -------------------------------------------
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


