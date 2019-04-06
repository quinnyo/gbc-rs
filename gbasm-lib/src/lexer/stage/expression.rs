// STD Dependencies -----------------------------------------------------------
use std::mem;


// External Dependencies ------------------------------------------------------
use gbasm_cpu::{Flag, Register};


// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::ValueStage;
use crate::expression::{Expression, ExpressionValue};
use super::value::ValueToken;
use super::super::{LexerStage, InnerToken, TokenIterator, TokenType, LexerToken, SourceError};


// Expression Specific Tokens -------------------------------------------------
lexer_token!(ExpressionToken, (Debug, Eq, PartialEq), {
    Constant(()),
    Reserved(()),
    Segment(()),
    Instruction(()),
    MetaInstruction(()),
    BinaryFile((Vec<u8>)),
    Comma(()),
    OpenBracket(()),
    CloseBracket(()),
    // TODO remove id and use name for constant evaluation loop detection?
    Expression((usize, Expression)),
    // TODO remove id and use name for constant evaluation loop detection?
    ConstExpression((usize, Expression)),
    GlobalLabelDef((usize)),
    LocalLabelDef((usize))

}, {
    Register {
        name => Register
    },
    Flag {
        typ => Flag
    }
});

impl ExpressionToken {
    fn try_from(token: ValueToken) -> Result<ExpressionToken, ValueToken> {
        match token {
            ValueToken::Segment(inner) => Ok(ExpressionToken::Segment(inner)),
            ValueToken::Reserved(inner) => Ok(ExpressionToken::Reserved(inner)),
            ValueToken::Instruction(inner) => Ok(ExpressionToken::Instruction(inner)),
            ValueToken::MetaInstruction(inner) => Ok(ExpressionToken::MetaInstruction(inner)),
            ValueToken::BinaryFile(inner, bytes) => Ok(ExpressionToken::BinaryFile(inner, bytes)),
            ValueToken::Comma(inner) => Ok(ExpressionToken::Comma(inner)),
            ValueToken::OpenBracket(inner) => Ok(ExpressionToken::OpenBracket(inner)),
            ValueToken::CloseBracket(inner) => Ok(ExpressionToken::CloseBracket(inner)),
            ValueToken::GlobalLabelDef(inner, id) => Ok(ExpressionToken::GlobalLabelDef(inner, id)),
            ValueToken::LocalLabelDef(inner, id) => Ok(ExpressionToken::LocalLabelDef(inner, id)),
            ValueToken::Register { inner, name } => Ok(ExpressionToken::Register {
                inner,
                name
            }),
            ValueToken::Flag { inner, typ } => Ok(ExpressionToken::Flag {
                inner,
                typ
            }),
            token => Err(token)
        }
    }
}


// Expression Level Lexer Implementation --------------------------------------
pub struct ExpressionStage;
impl LexerStage for ExpressionStage {

    type Input = ValueStage;
    type Output = ExpressionToken;
    type Data = ();

    fn from_tokens(
        tokens: Vec<<Self::Input as LexerStage>::Output>,
        _macro_calls: &mut Vec<MacroCall>,
        _data: &mut Vec<Self::Data>

    ) -> Result<Vec<Self::Output>, SourceError> {
        let mut expression_id = 0;
        let parsed_tokens = Self::parse_expression(tokens, &mut expression_id, false)?;
        Ok(parsed_tokens)
    }

}

impl ExpressionStage {

    fn parse_expression(tokens: Vec<ValueToken>, expression_id: &mut usize, is_argument: bool) -> Result<Vec<ExpressionToken>, SourceError> {
        let mut expression_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            if token.is(TokenType::Name) && (
                tokens.peek_is(TokenType::Reserved, Some("EQU")) ||
                tokens.peek_is(TokenType::Reserved, Some("EQUS"))

            ) {
                expression_tokens.push(ExpressionToken::Constant(token.into_inner()));

            } else {
                expression_tokens.push(Self::parse_expression_tokens(&mut tokens, token, is_argument, expression_id)?);
            }
        }
        Ok(expression_tokens)
    }

    fn parse_expression_tokens(
        tokens: &mut TokenIterator<ValueToken>,
        token: ValueToken,
        is_argument: bool,
        expression_id: &mut usize

    ) -> Result<ExpressionToken, SourceError> {
        // Check for start of expression
        let mut current_typ = token.typ();
        if ExpressionParser::is_start_token(current_typ) {

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
            let expr = ExpressionParser::parse_tokens(value_tokens, expression_id)?;
            let id = *expression_id;
            *expression_id += 1;
            if expr.is_constant() {
                Ok(ExpressionToken::ConstExpression(inner, id, expr))

            } else {
                Ok(ExpressionToken::Expression(inner, id, expr))
            }

        } else if !is_argument {

            // Forward all non-expression tokens
            match ExpressionToken::try_from(token) {
                Ok(token) => Ok(token),
                Err(token) => {
                    Err(token.error(
                        format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.value())
                    ))
                }
            }

        } else {
            Err(token.error(
                format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.value())
            ))
        }
    }

    fn parse_expression_argument(tokens: Vec<ValueToken>, expression_id: &mut usize) -> Result<Expression, SourceError> {
        let mut expression_tokens = Self::parse_expression(tokens, expression_id, true)?;
        if expression_tokens.len() > 1 {
            return Err(expression_tokens[1].error("Unexpected expression after argument.".to_string()));
        }
        if let ExpressionToken::Expression(_, _, expr) | ExpressionToken::ConstExpression(_, _, expr) = expression_tokens.remove(0) {
            Ok(expr)

        } else {
            unreachable!();
        }
    }

}


// Expression Parsing ---------------------------------------------------------
struct ExpressionParser {
    token: Option<ValueToken>,
    tokens: TokenIterator<ValueToken>,
    last_file_index: usize,
    last_index: usize
}

impl ExpressionParser {

    fn parse_tokens(tokens: Vec<ValueToken>, expression_id: &mut usize) -> Result<Expression, SourceError> {
        ExpressionParser::new(tokens)?.parse_binary(expression_id, 0)
    }

    fn new(tokens: Vec<ValueToken>) -> Result<ExpressionParser, SourceError> {
        let mut tokens = TokenIterator::new(tokens);
        let first = tokens.next();
        let mut parser = Self {
            token: None,
            tokens,
            last_file_index: 0,
            last_index: 0
        };
        parser.update(first);
        Ok(parser)
    }

    fn parse_binary(&mut self, expression_id: &mut usize, prec: usize) -> Result<Expression, SourceError> {

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

    fn next(&mut self) -> Option<ValueToken> {
        let next = self.tokens.next();
        self.update(next)
    }

    fn expect<S: Into<String>>(&mut self, msg: S) -> Result<ValueToken, SourceError> {
        let next =self.tokens.get(msg.into())?;
        Ok(self.update(Some(next)).expect("ExpressionParser::expect failed"))
    }

    fn assert_typ<S: Into<String>>(&mut self, typ: TokenType, msg: S) -> Result<ValueToken, SourceError> {
        let next = self.tokens.next();
        match self.update(next) {
            Some(token) => if token.is(typ) {
                Ok(token)

            } else {
                Err(token.error(msg.into()))
            },
            _ => unreachable!("ExpressionParser::assert_typ failed")
        }
    }

    fn update(&mut self, token: Option<ValueToken>) -> Option<ValueToken> {
        if let Some(token) = &token {
            self.last_file_index = token.inner().file_index;
            self.last_index = token.inner().start_index;
        }
        mem::replace(&mut self.token, token)
    }

    fn parse_unary(&mut self, expression_id: &mut usize) -> Result<Expression, SourceError> {

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
                Some(ValueToken::Name(inner)) => {
                    let name = inner.value.clone();
                    Ok(Expression::Value(ExpressionValue::ConstantValue(inner, name)))
                },
                Some(ValueToken::Offset { inner, value }) => Ok(
                    Expression::Value(ExpressionValue::OffsetAddress(inner, value)),
                ),
                Some(ValueToken::Float { value, .. }) => Ok(
                    Expression::Value(ExpressionValue::Float(value)),
                ),
                Some(ValueToken::Integer { value, .. }) => Ok(
                    Expression::Value(ExpressionValue::Integer(value)),
                ),
                Some(ValueToken::String { value, .. }) => Ok(
                    Expression::Value(ExpressionValue::String(value)),
                ),
                Some(ValueToken::GlobalLabelRef(inner, id)) => Ok(
                    Expression::Value(ExpressionValue::GlobalLabelAddress(inner, id)),
                ),
                Some(ValueToken::LocalLabelRef(inner, id)) => Ok(
                    Expression::Value(ExpressionValue::LocalLabelAddress(inner, id)),
                ),
                Some(ValueToken::BuiltinCall(inner, arguments)) => {
                    // TODO handle TokenGroups and forward tokens to EntryStage so they
                    let mut args = Vec::with_capacity(arguments.len());
                    for tokens in arguments {
                        args.push(ExpressionStage::parse_expression_argument(tokens, expression_id)?);
                    }
                    // TODO need a second typ here so EntryStage can parse the instructions
                    Ok(Expression::BuiltinCall {
                        name: inner.value.clone(),
                        inner,
                        args
                    })
                }
                Some(token) => {
                    Err(token.error(format!("Unexpected \"{}\" token in incomplete expression.", token.value())))
                },
                _ => Err(SourceError::new(self.last_file_index, self.last_index, "Unexpected end of expression.".to_string()))
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
        Self::is_value_token_type(current) || current == TokenType::OpenParen || current == TokenType::Operator
    }

    fn is_follow_up_token(prev: TokenType, next: TokenType) -> bool {
        if prev == TokenType::OpenParen {
            next == TokenType::OpenParen || next == TokenType::Operator || ExpressionParser::is_value_token_type(next)

        } else if prev == TokenType::CloseParen {
            next == TokenType::CloseParen || next == TokenType::Operator

        } else if prev == TokenType::Operator {
            next == TokenType::OpenParen || ExpressionParser::is_value_token_type(next) || next == TokenType::Operator

        } else if ExpressionParser::is_value_token_type(prev) {
            next == TokenType::CloseParen || next == TokenType::Operator

        } else {
            false
        }
    }

    fn is_value_token_type(typ: TokenType) -> bool {
        match typ {
            TokenType::Integer | TokenType::Float | TokenType::String | TokenType::Name |
            TokenType::Offset | TokenType::GlobalLabelRef | TokenType::LocalLabelRef | TokenType::BuiltinCall => {
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
    use crate::lexer::Lexer;
    use crate::expression::{Expression, ExpressionValue, Operator};
    use super::{ExpressionStage, ExpressionToken, InnerToken, Register, Flag};
    use super::super::mocks::value_lex;

    fn expr_lexer<S: Into<String>>(s: S) -> Lexer<ExpressionStage> {
        Lexer::<ExpressionStage>::from_lexer(value_lex(s)).expect("ExpressionLexer failed")
    }

    fn expr_lexer_error<S: Into<String>>(s: S) -> String {
        Lexer::<ExpressionStage>::from_lexer(value_lex(s)).err().unwrap().to_string()
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
    fn test_passthrough_registers() {
        assert_eq!(tfe("hl"), vec![
            ExpressionToken::Register {
                inner: itk!(0, 2, "hl"),
                name: Register::HL
            }
        ]);
    }

    #[test]
    fn test_passthrough_flags() {
        assert_eq!(tfe("nz"), vec![
            ExpressionToken::Flag {
                inner: itk!(0, 2, "nz"),
                typ: Flag::NoZero
            }
        ]);
    }

    #[test]
    fn test_constants() {
        assert_eq!(tfe("foo EQU"), vec![
            ExpressionToken::Constant(itk!(0, 3, "foo")),
            ExpressionToken::Reserved(itk!(4, 7, "EQU"))
        ]);
        assert_eq!(tfe("foo EQUS"), vec![
            ExpressionToken::Constant(itk!(0, 3, "foo")),
            ExpressionToken::Reserved(itk!(4, 8, "EQUS"))
        ]);
    }

    #[test]
    fn test_standalone() {
        assert_eq!(tfe("foo"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 3, "foo"),
                0,
                Expression::Value(ExpressionValue::ConstantValue(itk!(0, 3, "foo"), "foo".to_string()))
            )
        ]);
        assert_eq!(tfe("4"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("4.2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 3, "4.2"),
                0,
                Expression::Value(ExpressionValue::Float(OrderedFloat::from(4.2)))
            )
        ]);
        assert_eq!(tfe("'Foo'"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 5, "Foo"),
                0,
                Expression::Value(ExpressionValue::String("Foo".to_string()))
            )
        ]);
        assert_eq!(tfe("@+4"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "+4"),
                0,
                Expression::Value(ExpressionValue::OffsetAddress(itk!(0, 3, "+4"), 4))
            )
        ]);
        assert_eq!(tfe("@-4"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "-4"),
                0,
                Expression::Value(ExpressionValue::OffsetAddress(itk!(0, 3, "-4"), -4))
            )
        ]);
    }

    #[test]
    fn test_expression_non_constant() {
        assert_eq!(tfe("global_label:\nfoo + global_label"), vec![
            ExpressionToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ExpressionToken::Expression(
                itk!(14, 17, "foo"),
                0,
                Expression::Binary {
                    inner: itk!(18, 19, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::GlobalLabelAddress(
                        itk!(20, 32, "global_label"), 1
                    ))),
                    left: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(14, 17, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_expression_id() {
        assert_eq!(tfe("4\n2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            ),
            ExpressionToken::ConstExpression(
                itk!(2, 3, "2"),
                1,
                Expression::Value(ExpressionValue::Integer(2))
            )
        ]);
    }

    // Builtin Calls ----------------------------------------------------------
    #[test]
    fn test_builtin_call() {
        assert_eq!(tfe("DBG()"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
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
    fn test_builtin_call_with_global_label() {
        assert_eq!(tfe("global_label:\nCEIL(global_label)"), vec![
            ExpressionToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ExpressionToken::Expression(
                itk!(14, 18, "CEIL"),
                1,
                Expression::BuiltinCall {
                    inner: itk!(14, 18, "CEIL"),
                    name: "CEIL".to_string(),
                    args: vec![
                        Expression::Value(ExpressionValue::GlobalLabelAddress(
                            itk!(19, 31, "global_label"), 1
                        ))
                    ]
                }
            )
        ]);
    }

    #[test]
    fn test_builtin_call_with_local_label() {
        assert_eq!(tfe("global_label:\n.local_label:\nCEIL(.local_label)"), vec![
            ExpressionToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ExpressionToken::LocalLabelDef(
                itk!(14, 27, "local_label"),
                2
            ),
            ExpressionToken::Expression(
                itk!(28, 32, "CEIL"),
                1,
                Expression::BuiltinCall {
                    inner: itk!(28, 32, "CEIL"),
                    name: "CEIL".to_string(),
                    args: vec![
                        Expression::Value(ExpressionValue::LocalLabelAddress(
                            itk!(33, 45, "local_label"), 2
                        ))
                    ]
                }
            )
        ]);
    }

    #[test]
    fn test_builtin_call_error_with_invalid_arg_expr() {
        assert_eq!(expr_lexer_error("CEIL((4)(4))"), "In file \"main.gb.s\" on line 1, column 9: Unexpected expression after argument.\n\nCEIL((4)(4))\n        ^--- Here");
    }

    #[test]
    fn test_builtin_call_error_with_invalid_arg_tokens() {
        assert_eq!(expr_lexer_error("CEIL((4) DB)"), "In file \"main.gb.s\" on line 1, column 10: Unexpected \"DB\" token, expected the start of a expression instead.\n\nCEIL((4) DB)\n         ^--- Here");
    }

    // Label Refs -------------------------------------------------------------
    #[test]
    fn test_label_global_ref() {
        assert_eq!(tfe("global_label:\nglobal_label"), vec![
            ExpressionToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ExpressionToken::Expression(
                itk!(14, 26, "global_label"),
                0,
                Expression::Value(ExpressionValue::GlobalLabelAddress (
                    itk!(14, 26, "global_label"), 1
                ))
            )
        ]);
    }

    #[test]
    fn test_label_local_ref() {
        assert_eq!(tfe("global_label:\n.local_label:\n.local_label"), vec![
            ExpressionToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ExpressionToken::LocalLabelDef(
                itk!(14, 27, "local_label"),
                2
            ),
            ExpressionToken::Expression(
                itk!(28, 40, "local_label"),
                0,
                Expression::Value(ExpressionValue::LocalLabelAddress (
                    itk!(28, 40, "local_label"), 2
                ))
            )
        ]);
    }

    // Unary ------------------------------------------------------------------
    #[test]
    fn test_unary_operator_plus() {
        assert_eq!(tfe("+4"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
                itk!(0, 1, "+"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_minus() {
        assert_eq!(tfe("- 2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "-"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }
            )
        ]);
        assert_eq!(tfe("-foo"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "-"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_logical_not() {
        assert_eq!(tfe("!4"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
                itk!(0, 1, "!"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "!"),
                    op: Operator::LogicalNot,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_bit_negate() {
        assert_eq!(tfe("~4"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
                itk!(0, 1, "~"),
                0,
                Expression::Unary {
                    inner: itk!(0, 1, "~"),
                    op: Operator::BitNegate,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), "foo".to_string())))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_string() {
        assert_eq!(tfe("+'Foo'"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
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
        assert_eq!(expr_lexer_error("++"), "In file \"main.gb.s\" on line 1, column 2: Unexpected end of expression after unary operator, expected a right-hand side value.\n\n++\n ^--- Here");
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
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("((4))"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("(((4)))"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_double_expr() {
        assert_eq!(tfe("(4)(4)"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                0,
                Expression::Value(ExpressionValue::Integer(4))
            ),
            ExpressionToken::ConstExpression(
                itk!(3, 4, "("),
                1,
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_wrap_inner_unary() {
        assert_eq!(tfe("+(4)"), vec![
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
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
            ExpressionToken::ConstExpression(
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

    // Binary -----------------------------------------------------------------
    #[test]
    fn test_binary_operator_single() {
        let operators = vec![
            (Operator::LessThan, "<"),
            (Operator::GreaterThan, ">"),
            (Operator::Plus, "+"),
            (Operator::Minus, "-"),
            (Operator::Mul, "*"),
            (Operator::Div, "/"),
            (Operator::Modulo, "%"),
            (Operator::BitAnd, "&"),
            (Operator::BitOr, "|"),
            (Operator::BitXor, "^")
        ];
        for (op, s) in operators {
            assert_eq!(tfe(format!("4 {} 2", s)), vec![
                ExpressionToken::ConstExpression(
                    itk!(0, 1, "4"),
                    0,
                    Expression::Binary {
                        inner: itk!(2, 3, s),
                        op,
                        right: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                        left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                    }
                )
            ]);
        }
    }

    #[test]
    fn test_binary_operator_double() {
        let operators = vec![
            (Operator::ShiftRight, ">>"),
            (Operator::ShiftLeft, "<<"),
            (Operator::LogicalAnd, "&&"),
            (Operator::LogicalOr, "||"),
            (Operator::Equals, "=="),
            (Operator::Unequals, "!="),
            (Operator::GreaterThanEqual, ">="),
            (Operator::LessThanEqual, "<="),
            (Operator::Pow, "**"),
            (Operator::DivInt, "//")
        ];
        for (op, s) in operators {
            assert_eq!(tfe(format!("4 {} 2", s)), vec![
                ExpressionToken::ConstExpression(
                    itk!(0, 1, "4"),
                    0,
                    Expression::Binary {
                        inner: itk!(2, 4, s.chars().next().unwrap().to_string()),
                        op,
                        right: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                        left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                    }
                )
            ]);
        }
    }

    #[test]
    fn test_binary_operator_with_parens() {
        assert_eq!(tfe("((4) + ((2)))"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                0,
                Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
    }

    #[test]
    fn test_binary_unary_takes_precedence() {
        assert_eq!(tfe("4 + ~2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Binary {
                    inner: itk!(2, 3, "+"),
                    op: Operator::Plus,
                    right: Box::new(
                        Expression::Unary {
                            inner: itk!(4, 5, "~"),
                            op: Operator::BitNegate,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                        }
                    ),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
    }

    #[test]
    fn test_binary_precedence() {
        assert_eq!(tfe("4 || 2 && 8"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Binary {
                    inner: itk!(7, 9, "&"),
                    op: Operator::LogicalAnd,
                    left: Box::new(
                        Expression::Binary {
                            inner: itk!(2, 4, "|"),
                            op: Operator::LogicalOr,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                            left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                        }
                    ),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(8)))
                }
            )
        ]);

        assert_eq!(tfe("4 + 2 * 8"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Binary {
                    inner: itk!(2, 3, "+"),
                    op: Operator::Plus,
                    right: Box::new(
                        Expression::Binary {
                            inner: itk!(6, 7, "*"),
                            op: Operator::Mul,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(8))),
                            left: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                        }
                    ),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);

        assert_eq!(tfe("4 * 2 + 8"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Binary {
                    inner: itk!(6, 7, "+"),
                    op: Operator::Plus,
                    left: Box::new(
                        Expression::Binary {
                            inner: itk!(2, 3, "*"),
                            op: Operator::Mul,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                            left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                        }
                    ),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(8)))
                }
            )
        ]);

    }

    #[test]
    fn test_binary_precedence_parens() {
        assert_eq!(tfe("4 * (2 + 8)"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                0,
                Expression::Binary {
                    inner: itk!(2, 3, "*"),
                    op: Operator::Mul,
                    right: Box::new(
                        Expression::Binary {
                            inner: itk!(7, 8, "+"),
                            op: Operator::Plus,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(8))),
                            left: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                        }
                    ),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
    }

}

