// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::error::Error;


// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use super::{ValueLexer, InnerToken, TokenIterator, TokenType, LexerToken, LexerFile, LexerError};
use super::value::{Operator, ValueToken};
use super::macros::MacroCall;


// Expression Specific Tokens -------------------------------------------------
lexer_token!(ExpressionToken, (Debug, Eq, PartialEq), {
    Name(()),
    Reserved(()),
    Instruction(()),
    BinaryFile((Vec<u8>)),
    Comma(()),
    OpenBracket(()),
    CloseBracket(()),
    Expression((Expression))
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
        let parsed_tokens = Self::parse_expression(tokens, false)?;
        Ok(parsed_tokens)
    }

    fn parse_expression(tokens: Vec<ValueToken>, force_expression: bool) -> Result<Vec<ExpressionToken>, LexerError> {
        let mut expression_tokens = Vec::new();
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {

            let mut current_typ = token.typ();

            // Check for start of expression
            let expr_token = if ExpressionParser::is_start_token(current_typ) {

                // Collect all compatible tokens
                let inner = token.inner().clone();
                let mut value_tokens = vec![token];
                while let Some(next_typ) = tokens.peek_typ() {
                    if ExpressionParser::is_follow_up_token(next_typ, current_typ) {
                        value_tokens.push(tokens.next().unwrap());
                        current_typ = next_typ;

                    } else {
                        break;
                    }
                }

                // Try to build an expression tree from the tokens
                ExpressionToken::Expression(
                    inner,
                    Expression::from_tokens(value_tokens)?
                )

            } else if force_expression == false {
                // Forward all non-expression tokens
                ExpressionToken::from(token)

            } else {
                return Err(token.error(format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.value())));
            };

            expression_tokens.push(expr_token);

        }
        Ok(expression_tokens)
    }

}


// Expression Abstraction -----------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionType {
    Binary(Operator),
    Unary(Operator),
    Value,
    Call
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionValue {
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Expression {
    typ: ExpressionType,
    value: Option<ExpressionValue>,
    args: Option<Vec<Expression>>,
    left: Option<Box<Expression>>,
    right: Option<Box<Expression>>
}

impl Expression {

    fn from_tokens(tokens: Vec<ValueToken>) -> Result<Expression, LexerError> {
        ExpressionParser::new(tokens)?.parse_binary(0)
    }

    // TODO implement both type / type operator interaction as well as type / type value interaction
    // TODO macro return type based on weak type interactions in expression walk
    // TODO type check can only happen after name / value resolution
    // pub fn evaluate_typ()

}

pub struct ExpressionParser {
    token: Option<ValueToken>,
    tokens: TokenIterator<ValueToken>
}

impl ExpressionParser {

    fn new(tokens: Vec<ValueToken>) -> Result<ExpressionParser, LexerError> {
        let mut tokens = TokenIterator::new(tokens);
        Ok(Self {
            token: tokens.next(),
            tokens
        })
    }

    fn parse_binary(&mut self, prec: usize) -> Result<Expression, LexerError> {

        // Every potential binary expression starts with one unary
        let mut left = self.parse_unary()?;

        // Now we collect additional binary operators on the right as long as their
        // precedence is higher then the initial one
        while self.is_binary() && self.precedence() > prec {
            let op = self.expect_typ(TokenType::Operator, "Unexpected end of expression after binary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { typ, .. } = op {

                let right = self.parse_binary(typ.precedence() + typ.associativity())?;

                // Comine to a new lefthand expression
                left = Expression {
                    typ: ExpressionType::Binary(typ),
                    args: None,
                    value: None,
                    left: Some(Box::new(left)),
                    right: Some(Box::new(right))
                }

            } else {
                unreachable!();
            }
        }

        Ok(left)

    }

    fn next(&mut self) -> ValueToken {
        mem::replace(&mut self.token, self.tokens.next()).expect("ExpressionParser::consume failed")
    }

    fn expect<S: Into<String>>(&mut self, msg: S) -> Result<ValueToken, LexerError> {
        Ok(mem::replace(&mut self.token, Some(self.tokens.get(msg.into())?)).expect("ExpressionParser::expect failed"))
    }

    fn expect_typ<S: Into<String>>(&mut self, typ: TokenType, msg: S) -> Result<ValueToken, LexerError> {
        Ok(mem::replace(&mut self.token, Some(self.tokens.expect(typ, None, msg.into())?)).expect("ExpressionParser::expect_typ failed"))
    }

    fn parse_unary(&mut self) -> Result<Expression, LexerError> {

        // Parse unary operator and combine with it's right hand side value
        if self.is_unary() {
            let op = self.expect("Unexpected end of expression after unary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { typ, .. } = op {
                self.next();
                let right = self.parse_binary(typ.precedence())?;
                Ok(Expression {
                    typ: ExpressionType::Unary(typ),
                    args: None,
                    value: None,
                    left: None,
                    right: Some(Box::new(right))
                })

            } else {
                unreachable!();
            }

        // Handle parenthesis
        } else if self.is_paren() {
            self.expect("Unexpected end of expression after opening parenthesis, expected an inner expression.")?;
            let left = self.parse_binary(0)?;
            self.expect_typ(TokenType::CloseParen, "Expected a closing parenthesis after end of inner expression.")?;
            Ok(left)

        // Parse Values and Calls
        } else {
            let _value = self.next();

            /*
            // TODO parse parameter
            ValueToken::BuiltinCall(_, arguments) => {
                let mut expression_args = Vec::new();
                for tokens in arguments {
                    expression_args.push(Self::parse_expression(
                        tokens,
                        true
                    )?);
                }
            },
            }*/

            Ok(Expression {
                // TODO calls
                typ: ExpressionType::Value,
                // TODO call args
                args: None,
                // TODO value typ from above
                value: None,
                left: None,
                right: None
            })
        }
    }

    fn is_paren(&self) -> bool {
        match &self.token {
            Some(ValueToken::OpenParen(_)) => true,
            _ => false
        }
    }

    fn is_unary(&self) -> bool {
        match &self.token {
            Some(ValueToken::Operator { typ, .. }) => !typ.is_unary(),
            _ => false
        }
    }

    fn is_binary(&self) -> bool {
        match &self.token {
            Some(ValueToken::Operator { typ, .. })=> !typ.is_unary_exclusive(),
            _ => false
        }
    }

    fn precedence(&self) -> usize {
        match &self.token {
            Some(ValueToken::Operator { typ, .. })=> typ.precedence(),
            _ => 0
        }
    }

    fn is_start_token(current: TokenType) -> bool {
        match current {
            TokenType::Name => true,
            TokenType::BuiltinCall => true,
            TokenType::Operator => true,
            TokenType::Float | TokenType::Integer | TokenType::String => true,
            TokenType::GlobalLabelRef | TokenType::LocalLabelRef => true,
            TokenType::OpenParen => true,
            _ => false
        }
    }

    fn is_follow_up_token(next: TokenType, prev: TokenType) -> bool {
        // TODO build up for unary / binary operator support and parenthesis
        false
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

    macro_rules! itk {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $raw.into(), $parsed.into())
        }
    }

    macro_rules! etk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            ExpressionToken::$tok(itk!($start, $end, $raw, $parsed))
        }
    }


    // Expression Parsing -----------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfe(""), vec![]);
    }

    #[test]
    fn test_standalone() {
        assert_eq!(tfe("foo"), vec![]);
        assert_eq!(tfe("4"), vec![]);
        assert_eq!(tfe("4.2"), vec![]);
        assert_eq!(tfe("'Foo'"), vec![]);
    }

    #[test]
    fn test_builtin_call() {
        assert_eq!(tfe("DBG()"), vec![
            //ExpressionToken::Expression(itk!(0, 3, "DBG", "DBG"))

        ]);
        assert_eq!(tfe("MAX(4, MIN(1, 2))"), vec![]);
    }

    #[test]
    fn test_label_global_ref() {
        assert_eq!(tfe("global_label:\nglobal_label"), vec![
            ExpressionToken::GlobalLabelDef {
                inner: itk!(0, 13, "global_label", "global_label"),
                name: "global_label".to_string()
            }
        ]);
    }

    #[test]
    fn test_label_local_ref() {
        assert_eq!(tfe("global_label:\n.local_label:\n.local_label"), vec![
            ExpressionToken::GlobalLabelDef {
                inner: itk!(0, 13, "global_label", "global_label"),
                name: "global_label".to_string()
            },
            ExpressionToken::LocalLabelDef {
                inner: itk!(14, 27, ".", "."),
                name: "local_label".to_string()
            }
        ]);
    }

    // TODO test unary operators

    // TODO test binary operators

    // TODO test parenthesis

    // TODO test builtin calls

}

