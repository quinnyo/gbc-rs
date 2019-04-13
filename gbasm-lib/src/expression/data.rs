// Internal Dependencies ------------------------------------------------------
use super::{DataExpression, ExpressionValue};


// Types ----------------------------------------------------------------------
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataEndianess {
    /// LL HH
    Little,
    /// HH LL
    Big
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataAlignment {
    /// Anywhere
    Byte,
    /// Low byte must be 00
    Word,
    /// May not cross a word boundary
    WithinWord
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataStorage {
    /// DB
    Byte,
    /// DW
    Word,
    /// INCBIN "..."
    Array(Vec<u8>),
    /// DB 1[, 2, 3]
    Bytes(Vec<DataExpression>),
    /// DW 1[, 2, 3]
    Words(Vec<DataExpression>),
    /// DS [length] [fill]
    Buffer(DataExpression, Option<DataExpression>)
}

impl DataStorage {
    pub fn replace_constant(&mut self, constant: &str, new_value: &ExpressionValue) {
        match self {
            DataStorage::Bytes(expressions) | DataStorage::Words(expressions) => {
                for expr in expressions {
                    expr.1.replace_constant(constant, new_value);
                }
            },
            DataStorage::Buffer(length, fill) => {
                length.1.replace_constant(constant, new_value);
                if let Some(ref mut fill) = fill {
                    fill.1.replace_constant(constant, new_value);
                }
            },
            _ => {}
        }
    }
}

