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

trait ExpressionMember {
    fn is_start(&self) -> bool;
    fn is_follow_up(&self, prev: &Self) -> bool;
}

impl ExpressionMember for TokenType {

    fn is_start(&self) -> bool {
        // TODO
        false
    }

    fn is_follow_up(&self, prev: &TokenType) -> bool {
        // TODO
        false
    }

}


// Expression Specific Tokens -------------------------------------------------
lexer_token!(ExpressionToken, (Debug, Eq, PartialEq), {
    Name(()),
    Reserved(()),
    Instruction(()),
    BinaryFile((Vec<u8>)),
    Comma(()),
    OpenBracket(()),
    CloseBracket(()),
    Expression(())
}, {
    GlobalLabelDef {
        name => String
    },
    LocalLabelDef {
        name => String
    }
});

impl From<ValueToken> for ExpressionToken {
    fn from(token: ValueToken) -> Self {
        match token {
            ValueToken::Reserved(inner) => ExpressionToken::Reserved(inner),
            ValueToken::Instruction(inner) => ExpressionToken::Instruction(inner),
            ValueToken::BinaryFile(inner, bytes) => ExpressionToken::BinaryFile(inner, bytes),
            ValueToken::Comma(inner) => ExpressionToken::Comma(inner),
            ValueToken::OpenBracket(inner) => ExpressionToken::OpenBracket(inner),
            ValueToken::CloseBracket(inner) => ExpressionToken::CloseBracket(inner),
            ValueToken::GlobalLabelDef { inner, name } => ExpressionToken::GlobalLabelDef {
                inner,
                name
            },
            ValueToken::LocalLabelDef { inner, name } => ExpressionToken::LocalLabelDef {
                inner,
                name
            },
            token => {
                unreachable!("Token {:?} may not be passed through ExpressionLexer", token)
            }
        }
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
        let mut tokens_without_expression = Vec::new();
        while let Some(token) = tokens.next() {

            // Check for start of expression
            let mut current_typ = token.typ();
            if current_typ.is_start() {

                // Collect all compatible tokens
                let mut expression_tokens = Vec::new();
                while let Some(next_typ) = tokens.peek_typ() {
                    if next_typ.is_follow_up(&current_typ) {
                        expression_tokens.push(tokens.next().unwrap());
                        current_typ = next_typ;

                    } else {
                        break;
                    }
                }

                // TODO parse collected tokens into expression tree
                println!("{:?}", expression_tokens);

            // Forward all non-expression tokens
            } else {
                tokens_without_expression.push(ExpressionToken::from(token));
            }

        }
        Ok(tokens_without_expression)
    }

    /*
    fn is_expression(previous: Option<TokenType>, current: TokenType) -> bool {
        match previous {
            Some(prev) => match prev {
                TokenType::Name =>
                TokenType::OpenParen =>
                TokenType::BuiltinCall =>
                TokenType::Float =>
            },
            // Expression Start
            None => match current {
                TokenType::Name => true,
                TokenType::OpenParen => true,
                TokenType::BuiltinCall => true,
                TokenType::Float | TokenType::Integer | TokenType::String => true,
                TokenType::GlobalLabelRef | TokenType::GlobalLabelDef => true,
                TokenType::Operator => true,
                _ => false
            }
        }
    }*/

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

    // TODO test single types

    // TODO test unary operators

    // TODO test binary operators

    // TODO test parenthesis

    // TODO test builtin calls

}

