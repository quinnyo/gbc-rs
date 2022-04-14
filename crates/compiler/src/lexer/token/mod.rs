// STD Dependencies -----------------------------------------------------------
use std::fmt;


// Modules --------------------------------------------------------------------
mod generator;
mod iterator;
mod symbol;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
pub use generator::{TokenGenerator, TokenChar};
pub use iterator::TokenIterator;
pub use symbol::Symbol;


// Traits ---------------------------------------------------------------------
pub trait TokenType: PartialEq + fmt::Debug + Copy {}

pub trait LexerToken {

    type Typ: TokenType;

    fn typ(&self) -> Self::Typ;

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

    fn is(&self, typ: Self::Typ) -> bool {
        self.typ() == typ
    }

    fn is_symbol(&self, symbol: Symbol) -> bool {
        self.inner().value == symbol
    }

    fn symbol(&self) -> &Symbol {
        &self.inner().value
    }

}

// Inner Token Abstraction ----------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    pub file_index: usize,
    pub start_index: usize,
    pub end_index: usize,
    pub value: Symbol,
    pub macro_call_id: Option<usize>
}

impl InnerToken {
    pub fn new(file_index: usize, start_index: usize, end_index: usize, value: String) -> Self {
        Self {
            file_index,
            start_index,
            end_index,
            value: Symbol::from(value),
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

