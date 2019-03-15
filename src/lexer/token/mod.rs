// Internal Dependencies ------------------------------------------------------
use super::LexerError;


// Modules --------------------------------------------------------------------
mod generator;
mod iterator;
pub use generator::{TokenGenerator, TokenChar};
pub use iterator::TokenIterator;


// Traits ---------------------------------------------------------------------
pub trait LexerToken {

    fn typ(&self) -> TokenType;

    fn inner(&self) -> &InnerToken;

    fn inner_mut(&mut self) -> &mut InnerToken;

    fn into_inner(self) -> InnerToken;

    fn error(&self, message: String) -> LexerError {
        self.inner().error(message)
    }

    fn index(&self) -> (usize, usize) {
        let inner = self.inner();
        (inner.file_index, inner.start_index)
    }

    fn is(&self, typ: TokenType) -> bool {
        self.typ() == typ
    }

    fn has_value(&self, value: &str) -> bool {
        self.inner().value == value
    }

    fn value(&self) -> &str {
        &self.inner().value
    }

}


// Inner Token Abstraction ----------------------------------------------------
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenType {
    Newline,
    Name,
    Constant,
    Register,
    Flag,
    Reserved,
    Instruction,
    Parameter,
    Offset,
    NumberLiteral,
    Integer,
    Float,
    StringLiteral,
    String,
    TokenGroup,
    BinaryFile,
    BuiltinCall,
    Comma,
    Point,
    Colon,
    Operator,
    GlobalLabelDef,
    GlobalLabelRef,
    LocalLabelDef,
    LocalLabelRef,
    Comment,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Expression,
    ConstExpression
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    pub file_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    pub start_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    pub end_index: usize,
    pub value: String,
    pub macro_call_id: Option<usize>
}

impl InnerToken {
    pub fn new(file_index: usize, start_index: usize, end_index: usize, value: String) -> Self {
        Self {
            file_index,
            start_index,
            end_index,
            value,
            macro_call_id: None
        }
    }

    pub fn macro_call_id(&self) -> Option<usize> {
        self.macro_call_id
    }

    pub fn set_macro_call_id(&mut self, id: usize) {
        self.macro_call_id = Some(id);
    }

    pub fn error(&self, message: String) -> LexerError {
        LexerError::with_macro_call_id(
            self.file_index,
            self.start_index,
            message,
            self.macro_call_id
        )
    }
}

