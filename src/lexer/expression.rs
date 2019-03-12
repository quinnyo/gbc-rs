// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{ValueLexer, InnerToken, TokenIterator, TokenType, LexerToken, LexerFile, LexerError};
use super::value::ValueToken;
use super::macros::MacroCall;


// Expression Abstraction -----------------------------------------------------
#[allow(unused)]
pub enum ExpressionType {
    Binary,
    Unary,
    Value
}

#[allow(unused)]
pub struct Expression {
    value: ValueToken,
    typ: ExpressionType,
    left: Option<Box<Expression>>,
    right: Option<Box<Expression>>
}


// Expression Specific Tokens -------------------------------------------------
#[allow(unused)]
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionToken {
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
    Comma(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
    Expression(),
    GlobalLabelDef {
        inner: InnerToken,
        name: String,
    },
    LocalLabelDef {
        inner: InnerToken,
        name: String
    }
}


// Expression Level Lexer Implementation --------------------------------------
pub struct ExpressionLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<ExpressionToken>,
    pub macro_calls: Vec<MacroCall>
}

impl ExpressionLexer {

    pub fn try_from(lexer: ValueLexer) -> Result<ExpressionLexer, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let tokens = Self::from_tokens(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        Ok(Self {
            files,
            tokens,
            macro_calls
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    fn from_tokens(tokens: Vec<ValueToken>) -> Result<Vec<ExpressionToken>, LexerError> {
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            // TODO check for expression start
        }
        Ok(Vec::new())
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{ExpressionLexer, ExpressionToken, ValueToken, InnerToken};
    use crate::lexer::mocks::value_lex;

    fn expr_lexer<S: Into<String>>(s: S) -> ExpressionLexer {
        ExpressionLexer::try_from(value_lex(s)).expect("ExpressionLexer failed")
    }

    fn expr_lexer_error<S: Into<String>>(s: S) -> String {
        ExpressionLexer::try_from(value_lex(s)).err().unwrap().to_string()
    }

    fn tfe<S: Into<String>>(s: S) -> Vec<ExpressionToken> {
        expr_lexer(s).tokens
    }

    // Expression Parsing -----------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfe(""), vec![]);
    }

}

