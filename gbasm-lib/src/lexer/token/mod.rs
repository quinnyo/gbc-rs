// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;


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

    fn error(&self, message: String) -> SourceError {
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
    // Include
    Newline,
    Name,
    Reserved,
    Segment,
    Instruction,
    MetaInstruction,
    Parameter,
    NumberLiteral,
    StringLiteral,
    TokenGroup,
    BinaryFile,
    Comma,
    Point,
    Colon,
    Operator,
    Comment,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,

    // Macro
    BuiltinCall,
    IfStatement,

    // Value Tokens
    Offset,
    Integer,
    Float,
    String,
    Constant,
    Register,
    Flag,
    GlobalLabelDef,
    GlobalLabelRef,
    LocalLabelDef,
    LocalLabelRef,

    // Expression Tokens
    Expression,
    ConstExpression,

    // Entry Tokens
    Data,
    InstructionWithArg,
    DebugInstruction,
    DebugInstructionWithArg,
    SectionDeclaration

}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    pub file_index: usize,
    pub start_index: usize,
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

    pub fn error(&self, message: String) -> SourceError {
        SourceError::with_macro_call_id(
            self.file_index,
            self.start_index,
            message,
            self.macro_call_id
        )
    }
}

