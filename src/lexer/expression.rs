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
    Expression((usize, Expression))
}, {
    GlobalLabelDef {
        name => String
    },
    LocalLabelDef {
        name => String
    }
});

impl ExpressionToken {
    fn try_from(token: ValueToken) -> Result<ExpressionToken, ValueToken> {
        match token {
            ValueToken::Reserved(inner) => Ok(ExpressionToken::Reserved(inner)),
            ValueToken::Instruction(inner) => Ok(ExpressionToken::Instruction(inner)),
            ValueToken::BinaryFile(inner, bytes) => Ok(ExpressionToken::BinaryFile(inner, bytes)),
            ValueToken::Comma(inner) => Ok(ExpressionToken::Comma(inner)),
            ValueToken::OpenBracket(inner) => Ok(ExpressionToken::OpenBracket(inner)),
            ValueToken::CloseBracket(inner) => Ok(ExpressionToken::CloseBracket(inner)),
            ValueToken::GlobalLabelDef { inner, name } => Ok(ExpressionToken::GlobalLabelDef {
                inner,
                name
            }),
            ValueToken::LocalLabelDef { inner, name } => Ok(ExpressionToken::LocalLabelDef {
                inner,
                name
            }),
            token => Err(token)
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
        let mut expression_id = 0;
        let parsed_tokens = Self::parse_expression(tokens, &mut expression_id, false)?;
        Ok(parsed_tokens)
    }

    fn parse_expression(tokens: Vec<ValueToken>, expression_id: &mut usize, force_expression: bool) -> Result<Vec<ExpressionToken>, LexerError> {
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
                    if ExpressionParser::is_follow_up_token(current_typ, next_typ) {
                        value_tokens.push(tokens.next().unwrap());
                        current_typ = next_typ;

                    } else {
                        break;
                    }
                }

                // Try to build an expression tree from the tokens
                let expr = Expression::from_tokens(value_tokens, expression_id)?;
                let id = *expression_id;
                *expression_id += 1;
                ExpressionToken::Expression(inner, id, expr)

            } else if force_expression == false {
                // Forward all non-expression tokens
                match ExpressionToken::try_from(token) {
                    Ok(token) => token,
                    Err(token) => {
                        return Err(token.error(format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.value())));
                    }
                }

            } else {
                return Err(token.error(format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.value())));
            };

            expression_tokens.push(expr_token);

        }
        Ok(expression_tokens)
    }

    fn parse_expression_argument(tokens: Vec<ValueToken>, expression_id: &mut usize) -> Result<Expression, LexerError> {
        let mut expression_tokens = ExpressionLexer::parse_expression(tokens, expression_id, true)?;
        if expression_tokens.len() > 1 {
            // TODO can this even happen?
            return Err(expression_tokens[1].error("Unexpected expression after argument.".to_string()));
        }
        if let ExpressionToken::Expression(_, _, expr) = expression_tokens.remove(0) {
            Ok(expr)

        } else {
            unreachable!();
        }
    }

}


// Expression Abstraction -----------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionValue {
    VariableValue(InnerToken, String),
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String),
    OffsetAddress(InnerToken, i32),
    GlobalLabelAddress(InnerToken, String),
    LocalLabelAddress(InnerToken, String)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Binary {
        op: Operator,
        inner: InnerToken,
        left: Box<Expression>,
        right: Box<Expression>
    },
    Unary {
        op: Operator,
        inner: InnerToken,
        right: Box<Expression>
    },
    Value(ExpressionValue),
    BuiltinCall {
        inner: InnerToken,
        name: String,
        args: Vec<Expression>
    }
}

impl Expression {
    fn from_tokens(tokens: Vec<ValueToken>, expression_id: &mut usize) -> Result<Expression, LexerError> {
        ExpressionParser::new(tokens)?.parse_binary(expression_id, 0)
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

    fn parse_binary(&mut self, expression_id: &mut usize, prec: usize) -> Result<Expression, LexerError> {

        // Every potential binary expression starts with one unary
        let mut left = self.parse_unary(expression_id)?;

        // Now we collect additional binary operators on the right as long as their
        // precedence is higher then the initial one
        while self.is_binary() && self.precedence() > prec {
            let op = self.assert_typ(TokenType::Operator, "Unexpected end of expression after binary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { inner, typ } = op {

                let right = self.parse_binary(expression_id, typ.precedence() + typ.associativity())?;

                // Comine to a new left-hand side expression
                left = Expression::Binary {
                    op: typ,
                    inner,
                    left: Box::new(left),
                    right: Box::new(right)
                }

            } else {
                unreachable!();
            }
        }

        Ok(left)

    }

    fn next(&mut self) -> ValueToken {
        mem::replace(&mut self.token, self.tokens.next()).expect("ExpressionParser::next failed")
    }

    fn expect<S: Into<String>>(&mut self, msg: S) -> Result<ValueToken, LexerError> {
        Ok(mem::replace(&mut self.token, Some(self.tokens.get(msg.into())?)).expect("ExpressionParser::expect failed"))
    }

    fn assert_typ<S: Into<String>>(&mut self, typ: TokenType, msg: S) -> Result<ValueToken, LexerError> {
        match mem::replace(&mut self.token, self.tokens.next()) {
            Some(token) => if token.is(typ) {
                Ok(token)

            } else {
                Err(token.error(msg.into()))
            },
            _ => unreachable!("ExpressionParser::assert_typ failed")
        }
    }

    fn parse_unary(&mut self, expression_id: &mut usize) -> Result<Expression, LexerError> {

        // Parse unary operator and combine with it's right hand side value
        if self.is_unary() {
            let op = self.expect("Unexpected end of expression after unary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { inner, typ } = op {
                let right = self.parse_binary(expression_id, typ.precedence())?;
                Ok(Expression::Unary {
                    op: typ,
                    inner,
                    right: Box::new(right)
                })

            } else {
                unreachable!();
            }

        // Handle parenthesis
        } else if self.is_paren() {
            let _paren = self.expect("Unexpected end of expression after opening parenthesis, expected an inner expression.")?;
            let left = self.parse_binary(expression_id, 0)?;
            let _paren = self.assert_typ(TokenType::CloseParen, "Expected a closing parenthesis after end of inner expression.")?;
            Ok(left)

        // Parse Values and Calls
        } else {
            match self.next() {
                ValueToken::Name(inner) => {
                    let name = inner.value.clone();
                    Ok(Expression::Value(ExpressionValue::VariableValue(inner, name)))
                },
                ValueToken::Offset { inner, value } => Ok(
                    Expression::Value(ExpressionValue::OffsetAddress(inner, value)),
                ),
                ValueToken::Float { value, .. } => Ok(
                    Expression::Value(ExpressionValue::Float(value)),
                ),
                ValueToken::Integer { value, .. } => Ok(
                    Expression::Value(ExpressionValue::Integer(value)),
                ),
                ValueToken::String { value, .. } => Ok(
                    Expression::Value(ExpressionValue::String(value)),
                ),
                ValueToken::GlobalLabelRef { inner, name } => Ok(
                    Expression::Value(ExpressionValue::GlobalLabelAddress(inner, name)),
                ),
                ValueToken::LocalLabelRef { inner, name } => Ok(
                    Expression::Value(ExpressionValue::LocalLabelAddress(inner, name)),
                ),
                ValueToken::BuiltinCall(inner, arguments) => {
                    let mut args = Vec::new();
                    for tokens in arguments {
                        args.push(ExpressionLexer::parse_expression_argument(tokens, expression_id)?);
                    }
                    let name = inner.value.clone();
                    Ok(Expression::BuiltinCall {
                        inner,
                        name,
                        args
                    })
                }
                token => {
                    Err(token.error(format!("Unexpected \"{}\" token in incomplete expression.", token.value())))
                }
            }

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
            Some(ValueToken::Operator { typ, .. }) => typ.is_unary(),
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

    fn is_follow_up_token(prev: TokenType, next: TokenType) -> bool {
        if prev == TokenType::OpenParen {
            next == TokenType::OpenParen || next == TokenType::Operator || ExpressionParser::is_value_token_type(next)

        } else if prev == TokenType::CloseParen {
            next == TokenType::CloseParen || next == TokenType::Operator

        } else if prev == TokenType::Operator {
            next == TokenType::OpenParen || ExpressionParser::is_value_token_type(next)

        } else if ExpressionParser::is_value_token_type(prev) {
            next == TokenType::CloseParen || next == TokenType::Operator

        } else {
            false
        }
    }

    fn is_value_token_type(typ: TokenType) -> bool {
        match typ {
            TokenType::Integer | TokenType::Float | TokenType::String | TokenType::Name | TokenType::Offset | TokenType::GlobalLabelRef | TokenType::LocalLabelRef | TokenType::BuiltinCall => {
                true
            },
            _=> false
        }
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use super::{ExpressionLexer, ExpressionToken, InnerToken, Expression, ExpressionValue, Operator};
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
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }


    // Expression Parsing -----------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfe(""), vec![]);
    }

    #[test]
    fn test_standalone() {
        assert_eq!(tfe("foo"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "foo"),
                0,
                Expression::Value(ExpressionValue::VariableValue(itk!(0, 3, "foo"), "foo".to_string()))
            )
        ]);
        assert_eq!(tfe("4"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "4"),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("4.2"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "4.2"),
                0,
                Expression::Value(ExpressionValue::Float(OrderedFloat::from(4.2)))
            )
        ]);
        assert_eq!(tfe("'Foo'"), vec![
            ExpressionToken::Expression(
                itk!(0, 5, "Foo"),
                0,
                Expression::Value(ExpressionValue::String("Foo".to_string()))
            )
        ]);
    }

    #[test]
    fn test_expression_id() {
        assert_eq!(tfe("foo bar"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "foo"),
                0,
                Expression::Value(ExpressionValue::VariableValue(itk!(0, 3, "foo"), "foo".to_string()))
            ),
            ExpressionToken::Expression(
                itk!(4, 7, "bar"),
                1,
                Expression::Value(ExpressionValue::VariableValue(itk!(4, 7, "bar"), "bar".to_string()))
            )
        ]);
    }

    // Builtin Calls ----------------------------------------------------------
    #[test]
    fn test_builtin_call() {
        assert_eq!(tfe("DBG()"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "DBG"),
                0,
                Expression::BuiltinCall {
                    inner: itk!(0, 3, "DBG"),
                    name: "DBG".to_string(),
                    args: vec![]
                }
            )

        ]);
    }

    #[test]
    fn test_builtin_call_with_args() {
        assert_eq!(tfe("MAX(4, MIN(1, 2))"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "MAX"),
                4,
                Expression::BuiltinCall {
                    inner: itk!(0, 3, "MAX"),
                    name: "MAX".to_string(),
                    args: vec![
                        Expression::Value(ExpressionValue::Integer(4)),
                        Expression::BuiltinCall {
                            inner: itk!(7, 10, "MIN"),
                            name: "MIN".to_string(),
                            args: vec![
                                Expression::Value(ExpressionValue::Integer(1)),
                                Expression::Value(ExpressionValue::Integer(2))
                            ]
                        }
                    ]
                }
            )
        ]);
    }

    #[test]
    fn test_builtin_call_error_with_invalid_arg_expr() {
        assert_eq!(expr_lexer_error("CEIL((4)(4))"), "In file \"main.gb.s\" on line 1, column 9: Unexpected expression after argument.\n\nCEIL((4)(4))\n        ^--- Here");
    }

    // Label Refs -------------------------------------------------------------
    #[test]
    fn test_label_global_ref() {
        assert_eq!(tfe("global_label:\nglobal_label"), vec![
            ExpressionToken::GlobalLabelDef {
                inner: itk!(0, 13, "global_label"),
                name: "global_label".to_string()
            },
            ExpressionToken::Expression(
                itk!(14, 26, "global_label"),
                0,
                Expression::Value(ExpressionValue::GlobalLabelAddress (
                    itk!(14, 26, "global_label"),
                    "global_label".to_string()
                ))
            )
        ]);
    }

    #[test]
    fn test_label_local_ref() {
        assert_eq!(tfe("global_label:\n.local_label:\n.local_label"), vec![
            ExpressionToken::GlobalLabelDef {
                inner: itk!(0, 13, "global_label"),
                name: "global_label".to_string()
            },
            ExpressionToken::LocalLabelDef {
                inner: itk!(14, 27, "."),
                name: "local_label".to_string()
            },
            ExpressionToken::Expression(
                itk!(28, 40, "."),
                0,
                Expression::Value(ExpressionValue::LocalLabelAddress (
                    itk!(28, 40, "."),
                    "local_label".to_string()
                ))
            )
        ]);
    }

    // Unary ------------------------------------------------------------------
    #[test]
    fn test_unary_operator_plus() {
        assert_eq!(tfe("+4"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
        assert_eq!(tfe("+foo"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::VariableValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_minus() {
        assert_eq!(tfe("-foo"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "-"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(Expression::Value(ExpressionValue::VariableValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_logical_not() {
        assert_eq!(tfe("!4"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "!"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "!"),
                    op: Operator::LogicalNot,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
        assert_eq!(tfe("!foo"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "!"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "!"),
                    op: Operator::LogicalNot,
                    right: Box::new(Expression::Value(ExpressionValue::VariableValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_bit_negate() {
        assert_eq!(tfe("~4"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "~"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "~"),
                    op: Operator::BitNegate,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
        assert_eq!(tfe("~foo"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "~"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "~"),
                    op: Operator::BitNegate,
                    right: Box::new(Expression::Value(ExpressionValue::VariableValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_string() {
        assert_eq!(tfe("+'Foo'"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::String("Foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_builtin_call() {
        assert_eq!(tfe("+DBG()"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(
                        Expression::BuiltinCall {
                            inner: itk!(1, 4, "DBG"),
                            name: "DBG".to_string(),
                            args: vec![]
                        }
                    )
                }
            )
        ]);
    }

    #[test]
    fn test_unary_error_no_righthand() {
        assert_eq!(expr_lexer_error("+"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of expression after unary operator, expected a right-hand side value.\n\n+\n^--- Here");
    }

    #[test]
    fn test_unary_error_operator_righthand() {
        assert_eq!(expr_lexer_error("++"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of expression after unary operator, expected a right-hand side value.\n\n++\n^--- Here");
    }


    // Parenthesis ------------------------------------------------------------
    #[test]
    fn test_paren_error_no_inner() {
        assert_eq!(expr_lexer_error("()"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of expression after opening parenthesis, expected an inner expression.\n\n()\n^--- Here");
    }

    #[test]
    fn test_paren_error_close_start() {
        assert_eq!(expr_lexer_error(")"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \")\" token, expected the start of a expression instead.\n\n)\n^--- Here");
    }

    #[test]
    fn test_paren_error_unclosed() {
        assert_eq!(expr_lexer_error("("), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of expression after opening parenthesis, expected an inner expression.\n\n(\n^--- Here");
    }

    #[test]
    fn test_paren_wrap_inner() {
        assert_eq!(tfe("(4)"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("((4))"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("(((4)))"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_double_expr() {
        assert_eq!(tfe("(4)(4)"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            ),
            ExpressionToken::Expression(
                itk!(3, 4, "("),
                1,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_wrap_inner_unary() {
        assert_eq!(tfe("+(4)"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
        assert_eq!(tfe("+((4))"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
        assert_eq!(tfe("(+((4)))"), vec![
            ExpressionToken::Expression(
                itk!(0, 1, "("),
                0,
                Expression::Unary {
                    inner: itk!(1, 2, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
    }

    // TODO test binary operators

}

