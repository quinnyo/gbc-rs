// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{MacroLexer, InnerToken, TokenIterator, TokenType, LexerToken, LexerFile, LexerError};
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
    // TODO must also parse and convert MacroToken inside of BuiltinCalls
    BuiltinCall(InnerToken, Vec<Vec<ValueToken>>),
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

impl LexerToken for ValueToken {

    fn typ(&self) -> TokenType {
        match self {
            ValueToken::Name(_) => TokenType::Name,
            ValueToken::Reserved(_) => TokenType::Reserved,
            ValueToken::Instruction(_) => TokenType::Instruction,
            ValueToken::Offset(_) => TokenType::Offset,
            ValueToken::BinaryFile(_, _) => TokenType::BinaryFile,
            ValueToken::BuiltinCall(_, _) => TokenType::BuiltinCall,
            ValueToken::Comma(_) => TokenType::Comma,
            ValueToken::OpenParen(_) => TokenType::OpenParen,
            ValueToken::CloseParen(_) => TokenType::CloseParen,
            ValueToken::OpenBracket(_) => TokenType::OpenBracket,
            ValueToken::CloseBracket(_) => TokenType::CloseBracket,
            ValueToken::Number { .. } => TokenType::Number,
            ValueToken::String { .. } => TokenType::String,
            ValueToken::LabelDef { .. } => TokenType::LabelDef,
            ValueToken::LabelRef { .. } => TokenType::LabelRef,
            ValueToken::Operator { .. } => TokenType::Operator,
        }
    }

    fn inner(&self) -> &InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner) | ValueToken::Offset(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                &inner
            },
            ValueToken::Number { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                &inner
            }
        }
    }

    fn inner_mut(&mut self) -> &mut InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner) | ValueToken::Offset(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Number { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner) | ValueToken::Offset(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Number { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueOperator {
    // Plus,
    // Minus,
    // Modulo,
    // Divide,
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
        // TODO parse expressions here too
        Ok(Self {
            files,
            tokens
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    // TODO also use to convert BuiltinCalls Tokens
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


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{ValueLexer, ValueToken};
    use crate::lexer::mocks::macro_lex;

    fn value_lexer<S: Into<String>>(s: S) -> ValueLexer {
        ValueLexer::try_from(macro_lex(s)).expect("ValueLexer failed")
    }

    // fn value_lexer_error<S: Into<String>>(s: S) -> String {
    //     ValueLexer::try_from(macro_lex(s)).err().unwrap().to_string()
    // }

    fn tfv<S: Into<String>>(s: S) -> Vec<ValueToken> {
        value_lexer(s).tokens
    }

    #[test]
    fn test_empty() {
        assert_eq!(tfv(""), vec![]);
    }

}

