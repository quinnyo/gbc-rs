// Internal Dependencies ------------------------------------------------------
use super::DataExpression;


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum DataEndianess {
    /// LL HH
    Little,
    /// HH LL
    Big
}

#[derive(Debug, Eq, PartialEq)]
pub enum DataAlignment {
    /// Anywhere
    Byte,
    /// Low byte must be 00
    Word,
    /// May not cross a word boundary
    WithinWord
}

#[derive(Debug, Eq, PartialEq)]
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

