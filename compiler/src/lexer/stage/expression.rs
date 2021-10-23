// STD Dependencies -----------------------------------------------------------
use std::mem;


// External Dependencies ------------------------------------------------------
use gb_cpu::{Flag, Register};


// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::{ValueStage, Symbol};
use crate::error::SourceError;
use crate::expression::{Expression, ExpressionValue};
use super::value::{ValueToken, ValueTokenType};
use super::macros::{IfStatementBranch, ForStatement, BlockStatement};
use super::super::{LexerStage, InnerToken, TokenIterator, LexerToken};


// Types ----------------------------------------------------------------------
#[derive(Copy, Clone, PartialEq)]
enum ArgumentType {
    None,
    BuiltinCall,
    ParentLabelCall
}


// Expression Specific Tokens -------------------------------------------------
lexer_token!(ExpressionToken, ExpressionTokenType, (Debug, Eq, PartialEq), {
    // Default, Exported
    Constant((bool, bool)),
    Reserved(()),
    Segment(()),
    Instruction(()),
    MetaInstruction(()),
    BinaryFile((Vec<u8>)),
    Comma(()),
    OpenBracket(()),
    CloseBracket(()),
    Expression((Expression)),
    ConstExpression((Expression)),
    IfStatement((Vec<IfStatementBranch<ExpressionToken>>)),
    ForStatement((ForStatement<ExpressionToken>)),
    BlockStatement((BlockStatement<ExpressionToken>)),
    ParentLabelDef((usize, Option<Vec<Register>>, bool)),
    ChildLabelDef((usize))

}, {
    Register {
        name => Register
    },
    Flag {
        typ => Flag
    }
});

impl ExpressionToken {
    fn try_from(token: ValueToken, linter_enabled: bool) -> Result<ExpressionToken, SourceError> {
        match token {
            ValueToken::Segment(inner) => Ok(ExpressionToken::Segment(inner)),
            ValueToken::Reserved(inner) => Ok(ExpressionToken::Reserved(inner)),
            ValueToken::Instruction(inner) => Ok(ExpressionToken::Instruction(inner)),
            ValueToken::MetaInstruction(inner) => Ok(ExpressionToken::MetaInstruction(inner)),
            ValueToken::BinaryFile(inner, bytes) => Ok(ExpressionToken::BinaryFile(inner, bytes)),
            ValueToken::Comma(inner) => Ok(ExpressionToken::Comma(inner)),
            ValueToken::OpenBracket(inner) => Ok(ExpressionToken::OpenBracket(inner)),
            ValueToken::CloseBracket(inner) => Ok(ExpressionToken::CloseBracket(inner)),
            ValueToken::ParentLabelDef(inner, id, args, is_global) => Ok(ExpressionToken::ParentLabelDef(inner, id, args, is_global)),
            ValueToken::ChildLabelDef(inner, id, _) => Ok(ExpressionToken::ChildLabelDef(inner, id)),
            ValueToken::Register { inner, name } => Ok(ExpressionToken::Register {
                inner,
                name
            }),
            ValueToken::Flag { inner, typ } => Ok(ExpressionToken::Flag {
                inner,
                typ
            }),
            ValueToken::IfStatement(inner, branches) => {
                let mut expr_branches = Vec::with_capacity(branches.len());
                for branch in branches {
                    expr_branches.push(branch.into_other(|tokens| {
                        ExpressionStage::parse_expression(tokens, ArgumentType::None, linter_enabled)
                    })?);
                }
                Ok(ExpressionToken::IfStatement(
                    inner,
                    expr_branches
                ))
            },
            ValueToken::ForStatement(inner, for_statement) => {
                let binding = if let ValueToken::Name(inner) = *for_statement.binding {
                    Box::new(ExpressionToken::Constant(inner, false, false))

                } else {
                    unreachable!();
                };

                Ok(ExpressionToken::ForStatement(inner, ForStatement {
                    binding,
                    from: ExpressionStage::parse_expression(for_statement.from, ArgumentType::None, linter_enabled)?,
                    to: ExpressionStage::parse_expression(for_statement.to, ArgumentType::None, linter_enabled)?,
                    body: ExpressionStage::parse_expression(for_statement.body, ArgumentType::None, linter_enabled)?
                }))
            },
            ValueToken::BlockStatement(inner, block) => {
                Ok(ExpressionToken::BlockStatement(inner, match block {
                    BlockStatement::Using(cmd, body) => BlockStatement::Using(cmd, ExpressionStage::parse_expression(body, ArgumentType::None, linter_enabled)?),
                    BlockStatement::Volatile(body) => BlockStatement::Volatile(ExpressionStage::parse_expression(body, ArgumentType::None, linter_enabled)?)
                }))
            },
            token => Err(token.error(
                format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.symbol())
            ))
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
        _data: &mut Vec<Self::Data>,
        linter_enabled: bool

    ) -> Result<Vec<Self::Output>, SourceError> {
        let parsed_tokens = Self::parse_expression(tokens, ArgumentType::None, linter_enabled)?;
        Ok(parsed_tokens)
    }

}

impl ExpressionStage {

    fn parse_expression(
        tokens: Vec<ValueToken>,
        argument_type: ArgumentType,
        linter_enabled: bool

    ) -> Result<Vec<ExpressionToken>, SourceError> {
        let mut expression_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            if token.is_symbol(Symbol::GLOBAL) {
                let token = tokens.get("Unexpected end of input after GLOBAL keyword, expected either a label or a CONST declaration.")?;
                if let Some(c) = Self::parse_constant_new(&token, &mut tokens, true)? {
                    expression_tokens.push(c);

                } else {
                    return Err(token.error("Incomplete GLOBAL declaration, expected a CONST keyword instead.".to_string()));
                }

            } else if let Some(c) = Self::parse_constant_new(&token, &mut tokens, false)? {
                expression_tokens.push(c);

            } else if token.is(ValueTokenType::GlobalName) {
                return Err(token.error("Incomplete GLOBAL declaration, expected a CONST keyword before name.".to_string()));

            } else {
                expression_tokens.push(Self::parse_expression_tokens(&mut tokens, token, argument_type, linter_enabled)?);
            }
        }
        Ok(expression_tokens)
    }

    fn parse_constant_new(
        token: &ValueToken,
        tokens: &mut TokenIterator<ValueToken>,
        is_exported: bool

    ) -> Result<Option<ExpressionToken>, SourceError> {
        // [GLOBAL] [DEFAULT] CONST XYZ
        if token.is_symbol(Symbol::DEFAULT) {
            tokens.expect(ValueTokenType::Reserved, Some(Symbol::CONST), "while parsing CONST declaration")?;
            let token = tokens.expect(ValueTokenType::Name, None, "while parsing CONST declaration")?;
            Ok(Some(ExpressionToken::Constant(token.inner().clone(), true, !is_exported)))

        } else if token.is_symbol(Symbol::CONST) {
            let token = tokens.expect(ValueTokenType::Name, None, "while parsing CONST declaration")?;
            Ok(Some(ExpressionToken::Constant(token.inner().clone(), false, !is_exported)))

        } else {
            Ok(None)
        }
    }

    fn parse_expression_tokens(
        tokens: &mut TokenIterator<ValueToken>,
        token: ValueToken,
        argument_type: ArgumentType,
        linter_enabled: bool

    ) -> Result<ExpressionToken, SourceError> {
        // Check for start of expression
        let mut current_typ = token.typ();
        if ExpressionParser::is_start_token(current_typ) {

            // Collect all compatible tokens
            let inner = token.inner().clone();
            let mut value_tokens = vec![token];
            let mut paren_depth = 0;

            while let Some(next_typ) = tokens.peek_typ() {

                // Check Parenthesis Depth
                if current_typ == ValueTokenType::OpenParen {
                    paren_depth += 1;

                } else if current_typ == ValueTokenType::CloseParen {
                    paren_depth -= 1;
                }

                if paren_depth == 0 && next_typ == ValueTokenType::CloseParen {
                    break;
                }

                if ExpressionParser::is_follow_up_token(current_typ, next_typ) {
                    value_tokens.push(tokens.next().unwrap());
                    current_typ = next_typ;

                } else {
                    break;
                }
            }

            // Try to build an expression tree from the tokens
            let expr = ExpressionParser::parse_tokens(value_tokens, linter_enabled)?;
            if expr.is_constant() {
                Ok(ExpressionToken::ConstExpression(inner, expr))

            } else {
                Ok(ExpressionToken::Expression(inner, expr))
            }

        // LabelCalls must be standlone and cannot be part of other expressions
        } else if current_typ == ValueTokenType::ParentLabelCall {
            let inner = token.inner().clone();
            let expr = ExpressionParser::parse_tokens(vec![token], linter_enabled)?;
            Ok(ExpressionToken::Expression(inner, expr))

        } else if argument_type == ArgumentType::None {
            // Forward all non-expression tokens
            ExpressionToken::try_from(token, linter_enabled)

        // Registers in LabelCalls
        } else if argument_type == ArgumentType::ParentLabelCall && current_typ == ValueTokenType::Register {
            let inner = token.inner().clone();
            let reg = Register::from(inner.value.as_str());
            Ok(ExpressionToken::Expression(inner.clone(), Expression::RegisterArgument {
                inner,
                reg
            }))

        // Memory Locations in LabelCalls
        } else if argument_type == ArgumentType::ParentLabelCall && current_typ == ValueTokenType::OpenBracket {
            let token = tokens.get("while parsing memory location argument")?;
            let expr = Self::parse_expression_tokens(tokens, token, ArgumentType::None, linter_enabled)?;
            tokens.expect(ValueTokenType::CloseBracket, None, "while parsing memory location argument")?;
            if let ExpressionToken::ConstExpression(inner, expr) | ExpressionToken::Expression(inner, expr) = expr {
                Ok(ExpressionToken::Expression(inner.clone(), Expression::MemoryArgument {
                    inner,
                    value: Box::new(expr)
                }))

            } else {
                unreachable!()
            }

        } else {
            Err(token.error(
                format!("Unexpected \"{}\" token, expected the start of a expression instead.", token.symbol())
            ))
        }
    }

    fn parse_expression_argument(tokens: Vec<ValueToken>, argument_type: ArgumentType, linter_enabled: bool) -> Result<Expression, SourceError> {
        let mut expression_tokens = Self::parse_expression(tokens, argument_type, linter_enabled)?;
        if expression_tokens.len() > 1 {
            return Err(expression_tokens[1].error("Unexpected expression after argument.".to_string()));
        }
        if let ExpressionToken::Expression(_, expr) | ExpressionToken::ConstExpression(_, expr) = expression_tokens.remove(0) {
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
    last_index: usize,
    linter_support: bool
}

impl ExpressionParser {

    fn parse_tokens(tokens: Vec<ValueToken>, linter_support: bool) -> Result<Expression, SourceError> {
        ExpressionParser::new(tokens, linter_support)?.parse_binary(0)
    }

    fn new(tokens: Vec<ValueToken>, linter_support: bool) -> Result<ExpressionParser, SourceError> {
        let mut tokens = TokenIterator::new(tokens);
        let first = tokens.next();
        let mut parser = Self {
            token: None,
            tokens,
            last_file_index: 0,
            last_index: 0,
            linter_support
        };
        parser.update(first);
        Ok(parser)
    }

    fn parse_binary(&mut self, prec: usize) -> Result<Expression, SourceError> {

        // Every potential binary expression starts with one unary
        let mut left = self.parse_unary()?;

        // Now we collect additional binary operators on the right as long as their
        // precedence is higher then the initial one
        while self.is_binary() && self.precedence() > prec {
            let op = self.assert_typ(ValueTokenType::Operator, "Unexpected end of expression after binary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { inner, typ } = op {

                let right = self.parse_binary(typ.precedence() + typ.associativity())?;

                // Combine to a new left-hand side expression
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

    fn assert_typ<S: Into<String>>(&mut self, typ: ValueTokenType, msg: S) -> Result<ValueToken, SourceError> {
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

    fn parse_unary(&mut self) -> Result<Expression, SourceError> {

        // Parse unary operator and combine with it's right hand side value
        if self.is_unary() {
            let op = self.expect("Unexpected end of expression after unary operator, expected a right-hand side value.")?;
            if let ValueToken::Operator { inner, typ } = op {
                let right = self.parse_binary(typ.precedence())?;
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
            let left = self.parse_binary(0)?;
            let _paren = self.assert_typ(ValueTokenType::CloseParen, "Expected a closing parenthesis after end of inner expression.")?;
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
                Some(ValueToken::Integer { inner, value }) => Ok(
                    if self.linter_support {
                        Expression::Value(ExpressionValue::LintInteger(inner, value))

                    } else {
                        Expression::Value(ExpressionValue::Integer(value))
                    }
                ),
                Some(ValueToken::String { value, .. }) => Ok(
                    Expression::Value(ExpressionValue::String(value)),
                ),
                Some(ValueToken::ParentLabelRef(inner, id)) => Ok(
                    Expression::Value(ExpressionValue::ParentLabelAddress(inner, id)),
                ),
                Some(ValueToken::ChildLabelRef(inner, id, _)) => Ok(
                    Expression::Value(ExpressionValue::ChildLabelAddress(inner, id)),
                ),
                Some(ValueToken::BuiltinCall(inner, arguments)) => {
                    let mut args = Vec::with_capacity(arguments.len());
                    for tokens in arguments {
                        args.push(ExpressionStage::parse_expression_argument(tokens, ArgumentType::BuiltinCall, self.linter_support)?);
                    }
                    Ok(Expression::BuiltinCall {
                        name: inner.value.clone(),
                        inner,
                        args
                    })
                },
                Some(ValueToken::ParentLabelCall(inner, id, Some(arguments))) => {
                    let mut args = Vec::with_capacity(arguments.len());
                    for tokens in arguments {
                        args.push(ExpressionStage::parse_expression_argument(tokens, ArgumentType::ParentLabelCall, self.linter_support)?);
                    }
                    Ok(Expression::ParentLabelCall {
                        name: inner.value.clone(),
                        id,
                        inner,
                        args
                    })
                },
                Some(token) => {
                    Err(token.error(format!("Unexpected \"{}\" token in incomplete expression.", token.symbol())))
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

    fn is_start_token(current: ValueTokenType) -> bool {
        Self::is_value_token_type(current) || current == ValueTokenType::OpenParen || current == ValueTokenType::Operator
    }

    fn is_follow_up_token(prev: ValueTokenType, next: ValueTokenType) -> bool {
        if prev == ValueTokenType::OpenParen {
            next == ValueTokenType::OpenParen || next == ValueTokenType::Operator || ExpressionParser::is_value_token_type(next)

        } else if prev == ValueTokenType::CloseParen {
            next == ValueTokenType::CloseParen || next == ValueTokenType::Operator

        } else if prev == ValueTokenType::Operator {
            next == ValueTokenType::OpenParen || ExpressionParser::is_value_token_type(next) || next == ValueTokenType::Operator

        } else if ExpressionParser::is_value_token_type(prev) {
            next == ValueTokenType::CloseParen || next == ValueTokenType::Operator

        } else {
            false
        }
    }

    fn is_value_token_type(typ: ValueTokenType) -> bool {
        match typ {
            ValueTokenType::Integer | ValueTokenType::Float | ValueTokenType::String | ValueTokenType::Name |
            ValueTokenType::Offset | ValueTokenType::ParentLabelRef | ValueTokenType::ChildLabelRef | ValueTokenType::BuiltinCall => {
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
    use crate::lexer::{Lexer, Symbol};
    use crate::mocks::value_lex;
    use crate::expression::{Expression, ExpressionValue, Operator};
    use super::{ExpressionStage, ExpressionToken, InnerToken, Register, Flag, IfStatementBranch, ForStatement, BlockStatement};

    #[track_caller]
    fn expr_lexer<S: Into<String>>(s: S) -> Lexer<ExpressionStage> {
        Lexer::<ExpressionStage>::from_lexer(value_lex(s), false).expect("ExpressionLexer failed")
    }

    #[track_caller]
    fn expr_lexer_error<S: Into<String>>(s: S) -> String {
        colored::control::set_override(false);
        Lexer::<ExpressionStage>::from_lexer(value_lex(s), false).err().unwrap().to_string()
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
        assert_eq!(tfe("CONST foo"), vec![
            ExpressionToken::Constant(itk!(6, 9, "foo"), false, true)
        ]);
        assert_eq!(tfe("GLOBAL CONST foo"), vec![
            ExpressionToken::Constant(itk!(13, 16, "foo"), false, false)
        ]);
    }

    #[test]
    fn test_error_constants_export() {
        assert_eq!(expr_lexer_error("GLOBAL foo"), "In file \"main.gb.s\" on line 1, column 8: Incomplete GLOBAL declaration, expected a CONST keyword before name.\n\nGLOBAL foo\n       ^--- Here");
        assert_eq!(expr_lexer_error("GLOBAL DEFAULT foo"), "In file \"main.gb.s\" on line 1, column 16: Unexpected token \"Name\" while parsing CONST declaration, expected \"CONST\" instead.\n\nGLOBAL DEFAULT foo\n               ^--- Here");
    }

    #[test]
    fn test_constants_defaults() {
        assert_eq!(tfe("DEFAULT CONST foo"), vec![
            ExpressionToken::Constant(itk!(14, 17, "foo"), true, true)
        ]);
        assert_eq!(tfe("GLOBAL DEFAULT CONST foo"), vec![
            ExpressionToken::Constant(itk!(21, 24, "foo"), true, false)
        ]);
    }

    #[test]
    fn test_error_constants_defaults() {
        assert_eq!(expr_lexer_error("DEFAULT foo"), "In file \"main.gb.s\" on line 1, column 9: Unexpected token \"Name\" while parsing CONST declaration, expected \"CONST\" instead.\n\nDEFAULT foo\n        ^--- Here");
    }

    #[test]
    fn test_standalone() {
        assert_eq!(tfe("foo"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 3, "foo"),
                Expression::Value(ExpressionValue::ConstantValue(itk!(0, 3, "foo"), Symbol::from("foo".to_string())))
            )
        ]);
        assert_eq!(tfe("4"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("4.2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 3, "4.2"),
                Expression::Value(ExpressionValue::Float(OrderedFloat::from(4.2)))
            )
        ]);
        assert_eq!(tfe("'Foo'"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 5, "Foo"),
                Expression::Value(ExpressionValue::String("Foo".to_string()))
            )
        ]);
        assert_eq!(tfe("@+4"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "+4"),
                Expression::Value(ExpressionValue::OffsetAddress(itk!(0, 3, "+4"), 4))
            )
        ]);
        assert_eq!(tfe("@-4"), vec![
            ExpressionToken::Expression(
                itk!(0, 3, "-4"),
                Expression::Value(ExpressionValue::OffsetAddress(itk!(0, 3, "-4"), -4))
            )
        ]);
    }

    #[test]
    fn test_expression_non_constant() {
        assert_eq!(tfe("global_label:\nfoo + global_label"), vec![
            ExpressionToken::ParentLabelDef(
                itk!(0, 13, "global_label"),
                1,
                None,
                false
            ),
            ExpressionToken::Expression(
                itk!(14, 17, "foo"),
                Expression::Binary {
                    inner: itk!(18, 19, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::ParentLabelAddress(
                        itk!(20, 32, "global_label"), 1
                    ))),
                    left: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(14, 17, "foo"), Symbol::from("foo".to_string()))))
                }
            )
        ]);
    }

    #[test]
    fn test_expression_id() {
        assert_eq!(tfe("4\n2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                Expression::Value(ExpressionValue::Integer(4))
            ),
            ExpressionToken::ConstExpression(
                itk!(2, 3, "2"),
                Expression::Value(ExpressionValue::Integer(2))
            )
        ]);
    }


    #[test]
    fn test_error_global_standalone() {
        assert_eq!(expr_lexer_error("GLOBAL"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input after GLOBAL keyword, expected either a label or a CONST declaration.\n\nGLOBAL\n^--- Here");
    }

    // Label Calls ------------------------------------------------------------
    #[test]
    fn test_label_call() {
        assert_eq!(tfe("global_label(a):\ncall global_label(1)"), vec![
            ExpressionToken::ParentLabelDef(
                itk!(0, 16, "global_label"),
                1,
                Some(vec![Register::Accumulator]),
                false
            ),
            ExpressionToken::Instruction(itk!(17, 21, "call")),
            ExpressionToken::Expression(
                itk!(22, 34, "global_label"),
                Expression::ParentLabelCall {
                    inner: itk!(22, 34, "global_label"),
                    id: 1,
                    name: Symbol::from("global_label".to_string()),
                    args: vec![Expression::Value(ExpressionValue::Integer(1))]
                }
            )
        ]);
    }

    #[test]
    fn test_label_call_memory() {
        assert_eq!(tfe("global_label(a):\ncall global_label([1])"), vec![
            ExpressionToken::ParentLabelDef(
                itk!(0, 16, "global_label"),
                1,
                Some(vec![Register::Accumulator]),
                false
            ),
            ExpressionToken::Instruction(itk!(17, 21, "call")),
            ExpressionToken::Expression(
                itk!(22, 34, "global_label"),
                Expression::ParentLabelCall {
                    inner: itk!(22, 34, "global_label"),
                    id: 1,
                    name: Symbol::from("global_label".to_string()),
                    args: vec![Expression::MemoryArgument {
                        inner: itk!(36, 37, "1"),
                        value: Box::new(Expression::Value(ExpressionValue::Integer(1)))
                    }]
                }
            )
        ]);
    }

    // Builtin Calls ----------------------------------------------------------
    #[test]
    fn test_builtin_call() {
        assert_eq!(tfe("DBG()"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 3, "DBG"),
                Expression::BuiltinCall {
                    inner: itk!(0, 3, "DBG"),
                    name: Symbol::from("DBG".to_string()),
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
                Expression::BuiltinCall {
                    inner: itk!(0, 3, "MAX"),
                    name: Symbol::from("MAX".to_string()),
                    args: vec![
                        Expression::Value(ExpressionValue::Integer(4)),
                        Expression::BuiltinCall {
                            inner: itk!(7, 10, "MIN"),
                            name: Symbol::from("MIN".to_string()),
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
            ExpressionToken::ParentLabelDef(
                itk!(0, 13, "global_label"),
                1,
                None,
                false
            ),
            ExpressionToken::Expression(
                itk!(14, 18, "CEIL"),
                Expression::BuiltinCall {
                    inner: itk!(14, 18, "CEIL"),
                    name: Symbol::from("CEIL".to_string()),
                    args: vec![
                        Expression::Value(ExpressionValue::ParentLabelAddress(
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
            ExpressionToken::ParentLabelDef(
                itk!(0, 13, "global_label"),
                1,
                None,
                false
            ),
            ExpressionToken::ChildLabelDef(
                itk!(14, 27, "local_label"),
                2
            ),
            ExpressionToken::Expression(
                itk!(28, 32, "CEIL"),
                Expression::BuiltinCall {
                    inner: itk!(28, 32, "CEIL"),
                    name: Symbol::from("CEIL".to_string()),
                    args: vec![
                        Expression::Value(ExpressionValue::ChildLabelAddress(
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
            ExpressionToken::ParentLabelDef(
                itk!(0, 13, "global_label"),
                1,
                None,
                false
            ),
            ExpressionToken::Expression(
                itk!(14, 26, "global_label"),
                Expression::Value(ExpressionValue::ParentLabelAddress (
                    itk!(14, 26, "global_label"), 1
                ))
            )
        ]);
    }

    #[test]
    fn test_label_local_ref() {
        assert_eq!(tfe("global_label:\n.local_label:\n.local_label"), vec![
            ExpressionToken::ParentLabelDef(
                itk!(0, 13, "global_label"),
                1,
                None,
                false
            ),
            ExpressionToken::ChildLabelDef(
                itk!(14, 27, "local_label"),
                2
            ),
            ExpressionToken::Expression(
                itk!(28, 40, "local_label"),
                Expression::Value(ExpressionValue::ChildLabelAddress (
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
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), Symbol::from("foo".to_string()))))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_minus() {
        assert_eq!(tfe("-2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "-"),
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }
            )
        ]);
        assert_eq!(tfe("- 2"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "-"),
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
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), Symbol::from("foo".to_string()))))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_logical_not() {
        assert_eq!(tfe("!4"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "!"),
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
                Expression::Unary {
                    inner: itk!(0, 1, "!"),
                    op: Operator::LogicalNot,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), Symbol::from("foo".to_string()))))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_bit_negate() {
        assert_eq!(tfe("~4"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "~"),
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
                Expression::Unary {
                    inner: itk!(0, 1, "~"),
                    op: Operator::BitNegate,
                    right: Box::new(Expression::Value(ExpressionValue::ConstantValue(itk!(1, 4, "foo"), Symbol::from("foo".to_string()))))
                }
            )
        ]);
    }

    #[test]
    fn test_unary_operator_string() {
        assert_eq!(tfe("+'Foo'"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "+"),
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
                Expression::Unary {
                    inner: itk!(0, 1, "+"),
                    op: Operator::Plus,
                    right: Box::new(
                        Expression::BuiltinCall {
                            inner: itk!(1, 4, "DBG"),
                            name: Symbol::from("DBG".to_string()),
                            args: vec![]
                        }
                    )
                }
            )
        ]);
    }

    #[test]
    fn test_unary_precedence() {
        assert_eq!(tfe("-2 * 6"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "-"),
                Expression::Unary {
                    inner: itk!(0, 1, "-"),
                    op: Operator::Minus,
                    right: Box::new(
                        Expression::Binary {
                            inner: itk!(3, 4, "*"),
                            op: Operator::Mul,
                            right: Box::new(Expression::Value(ExpressionValue::Integer(6))),
                            left: Box::new(Expression::Value(ExpressionValue::Integer(2)))
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
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("((4))"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
        assert_eq!(tfe("(((4)))"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_double_expr() {
        assert_eq!(tfe("(4)(4)"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "("),
                Expression::Value(ExpressionValue::Integer(4))
            ),
            ExpressionToken::ConstExpression(
                itk!(3, 4, "("),
                Expression::Value(ExpressionValue::Integer(4))
            )
        ]);
    }

    #[test]
    fn test_paren_wrap_inner_unary() {
        assert_eq!(tfe("+(4)"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "+"),
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
                Expression::Unary {
                    inner: itk!(1, 2, "+"),
                    op: Operator::Plus,
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                }
            )
        ]);
    }

    #[test]
    fn test_paren_missing_open() {
        assert_eq!(expr_lexer_error("ROUND(2))"), "In file \"main.gb.s\" on line 1, column 9: Unexpected \")\" token, expected the start of a expression instead.\n\nROUND(2))\n        ^--- Here");
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

    #[test]
    fn test_right_associativity_pow() {
        assert_eq!(tfe("4 * 2 ** 8"), vec![
            ExpressionToken::ConstExpression(
                itk!(0, 1, "4"),
                Expression::Binary {
                    inner: itk!(6, 8, "*"),
                    op: Operator::Pow,
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
    fn test_binary_equal_precedence() {
        assert_eq!(tfe("1 + 2 == 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::Equals,
                inner: itk!(6, 8, "="),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(11, 12, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 + 2 != 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::Unequals,
                inner: itk!(6, 8, "!"),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(11, 12, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 + 2 >= 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::GreaterThanEqual,
                inner: itk!(6, 8, ">"),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(11, 12, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 + 2 <= 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::LessThanEqual,
                inner: itk!(6, 8, "<"),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(11, 12, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 + 2 > 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::GreaterThan,
                inner: itk!(6, 7, ">"),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(10, 11, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 + 2 < 3 - 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::LessThan,
                inner: itk!(6, 7, "<"),
                left: Box::new(Expression::Binary {
                    op: Operator::Plus,
                    inner: itk!(2, 3, "+"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::Minus,
                    inner: itk!(10, 11, "-"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 | 2 == 3 & 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::Equals,
                inner: itk!(6, 8, "="),
                left: Box::new(Expression::Binary {
                    op: Operator::BitOr,
                    inner: itk!(2, 3, "|"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                }),
                right: Box::new(Expression::Binary {
                    op: Operator::BitAnd,
                    inner: itk!(11, 12, "&"),
                    left: Box::new(Expression::Value(ExpressionValue::Integer(3))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(4)))
                })
            })
        ]);

        assert_eq!(tfe("1 && 2 == 3 || 4"), vec![
            ExpressionToken::ConstExpression(itk!(0, 1, "1"), Expression::Binary {
                op: Operator::LogicalOr,
                inner: itk!(12, 14, "|"),
                left: Box::new(Expression::Binary {
                    op: Operator::Equals,
                    inner: itk!(7, 9, "="),
                    left: Box::new(Expression::Binary {
                        op: Operator::LogicalAnd,
                        inner: itk!(2, 4, "&"),
                        left: Box::new(Expression::Value(ExpressionValue::Integer(1))),
                        right: Box::new(Expression::Value(ExpressionValue::Integer(2)))
                    }),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                }),
                right: Box::new(Expression::Value(ExpressionValue::Integer(4))),
            })
        ]);

    }

    // If Statements ----------------------------------------------------------
    #[test]
    fn test_if_statment_forwarding() {
        let lexer = expr_lexer("IF 1 THEN IF 0 THEN nop ENDIF ENDIF");
        assert_eq!(lexer.tokens, vec![
            ExpressionToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![
                        ExpressionToken::ConstExpression(itk!(3, 4, "1"), Expression::Value(ExpressionValue::Integer(1)))
                    ]),
                    body: vec![
                        ExpressionToken::IfStatement(itk!(10, 12, "IF"), vec![
                            IfStatementBranch {
                                condition: Some(vec![
                                    ExpressionToken::ConstExpression(itk!(13, 14, "0"), Expression::Value(ExpressionValue::Integer(0)))
                                ]),
                                body: vec![
                                    ExpressionToken::Instruction(itk!(20, 23, "nop"))
                                ]
                            }
                        ])
                    ]
                }
            ])
        ]);
    }

    // FOR Statements ---------------------------------------------------------
    #[test]
    fn test_for_statment_forwarding() {
        let lexer = expr_lexer("FOR x IN 0 TO 10 REPEAT bar ENDFOR");
        assert_eq!(lexer.tokens, vec![
            ExpressionToken::ForStatement(itk!(0, 3, "FOR"), ForStatement {
                binding: Box::new(ExpressionToken::Constant(itk!(4, 5, "x"), false, false)),
                from: vec![
                    ExpressionToken::ConstExpression(itk!(9, 10, "0"), Expression::Value(ExpressionValue::Integer(0)))
                ],
                to: vec![
                    ExpressionToken::ConstExpression(itk!(14, 16, "10"), Expression::Value(ExpressionValue::Integer(10)))
                ],
                body: vec![
                    ExpressionToken::ConstExpression(
                        itk!(24, 27, "bar"),
                        Expression::Value(ExpressionValue::ConstantValue(itk!(24, 27, "bar"), Symbol::from("bar".to_string())))
                    )
                ]
            })
        ]);
    }

    // Blocks -----------------------------------------------------------------
    #[test]
    fn test_block_using_forwarding() {
        let lexer = expr_lexer("BLOCK USING 'cmd' DB 1 ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ExpressionToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Using(
                "cmd".to_string(),
                vec![
                    ExpressionToken::Reserved(itk!(18, 20, "DB")),
                    ExpressionToken::ConstExpression(
                        itk!(21, 22, "1"),
                        Expression::Value(ExpressionValue::Integer(1))
                    )
                ])
            )
        ]);
    }

    #[test]
    fn test_block_volatile_forwarding() {
        let lexer = expr_lexer("BLOCK VOLATILE nop ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ExpressionToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Volatile(
                vec![
                    ExpressionToken::Instruction(itk!(15, 18, "nop"))
                ])
            )
        ]);
    }

}

