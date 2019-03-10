// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{MacroLexer, InnerToken, TokenIterator, LexerFile, LexerError};
use super::macros::MacroToken;


// Value Specific Tokens ------------------------------------------------------
#[allow(unused)]
#[derive(Debug, Eq, PartialEq)]
pub enum ValueToken {
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    Offset(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
    BuiltinCall(InnerToken, Vec<Vec<MacroToken>>),
    Comma(InnerToken),
    OpenParen(InnerToken),
    CloseParen(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
    Number {
        inner: InnerToken,
        value: i32
    },
    String {
        inner: InnerToken,
        value: String
    },
    LabelDef {
        inner: InnerToken,
        name: String,
        local: bool
    },
    LabelRef {
        inner: InnerToken,
        name: String,
        local: bool
    },
    Operator {
        inner: InnerToken,
        op: ValueOperator
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueOperator {

}

// Value Level Lexer Implementation -------------------------------------------
pub struct ValueLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<ValueToken>
}

impl ValueLexer {

    pub fn try_from(lexer: MacroLexer) -> Result<ValueLexer, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let tokens = Self::from_tokens(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        Ok(Self {
            files,
            tokens
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    fn from_tokens(tokens: Vec<MacroToken>) -> Result<Vec<ValueToken>, LexerError> {
        // TODO drop comments
        let mut _tokens = TokenIterator::new(tokens);
        // TODO Parse Numbers
        // TODO Convert String Tokens
        // TODO Combine Label Defs and Refs prefix labels from macro expansions by their expansion_id
            // TODO check for label re-definition
        // TODO Combine and match Operators Tokens into actual Operators
        Ok(Vec::new())
    }

}

