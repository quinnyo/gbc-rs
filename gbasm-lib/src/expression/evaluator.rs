// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, LexerError};
use crate::expression::{DataExpression, Expression, ExpressionValue, ExpressionResult, Operator};


// Expression Evaluator -------------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct EvaluatorConstant {
    pub inner: InnerToken,
    pub is_string: bool,
    pub expression: DataExpression,
}

#[derive(Debug, Eq, PartialEq)]
pub struct EvaluatorContext {
    pub constants: HashMap<String, ExpressionResult>,
    pub raw_constants: HashMap<String, EvaluatorConstant>,
    pub label_offsets: HashMap<usize, usize>,
    pub rom_offset: i32
}

impl EvaluatorContext {

    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            raw_constants: HashMap::new(),
            label_offsets: HashMap::new(),
            rom_offset: 0
        }
    }

    pub fn resolve_constants(&mut self) -> Result<(), LexerError> {
        let mut names: Vec<String> = self.raw_constants.keys().cloned().into_iter().collect();
        names.sort_by(|a, b| {
            a.cmp(&b)
        });
        for name in names {
            let constant = self.raw_constants.get(&name).unwrap().clone();
            let stack = vec![constant.expression.0];
            let c = self.resolve_expression(
                &stack,
                constant.expression.1
            )?;
            self.constants.insert(name, c);
        }
        Ok(())
    }

    pub fn resolve_constant_expression(
        &mut self,
        parent: &InnerToken,
        constant_stack: &[usize],
        name: &str

    ) -> Result<ExpressionResult, LexerError> {
        if let Some(result) = self.constants.get(name) {
            Ok(result.clone())

        } else if let Some(EvaluatorConstant { inner, expression, .. }) = self.raw_constants.get(name).map(|c| c.clone()) {
            let (id, value) = expression.clone();
            if constant_stack.contains(&id) {
                Err(parent.error(
                    format!("Recursive declaration of constant \"{}\".", name)

                ).with_reference(&inner, "Initial declaration was"))

            } else {
                let mut child_stack = constant_stack.to_vec();
                child_stack.push(id);
                self.resolve_expression(
                    &child_stack,
                    value
                )
            }

        } else {
            Err(parent.error(format!("Reference to undeclared constant \"{}\".", name)))
        }
    }

    pub fn resolve_expression(
        &mut self,
        constant_stack: &[usize],
        expression: Expression

    ) -> Result<ExpressionResult, LexerError> {
        Ok(match expression {
            Expression::Binary { inner, op, left, right } => {
                let left = self.resolve_expression(
                    constant_stack,
                    *left
                )?;
                let right = self.resolve_expression(
                    constant_stack,
                    *right
                )?;
                Self::apply_binary_operator(&inner, op.clone(), left, right)?
            },
            Expression::Unary { inner, op, right  } => {
                let right = self.resolve_expression(
                    constant_stack,
                    *right
                )?;
                Self::apply_unary_operator(&inner, op.clone(), right)?
            },
            Expression::Value(value) => {
                match value {
                    ExpressionValue::ConstantValue(inner, name) => {
                        let c = self.resolve_constant_expression(
                            &inner,
                            constant_stack,
                            &name
                        )?;
                        self.constants.insert(name.clone(), c.clone());
                        c
                    },
                    ExpressionValue::Integer(i) => ExpressionResult::Integer(i),
                    ExpressionValue::Float(f) => ExpressionResult::Float(f),
                    ExpressionValue::String(s) => ExpressionResult::String(s),
                    ExpressionValue::OffsetAddress(_, offset) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(self.rom_offset + offset)
                    },
                    ExpressionValue::GlobalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(*self.label_offsets.get(&id).expect("Invalid label ID!") as i32)
                    },
                    ExpressionValue::LocalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(*self.label_offsets.get(&id).expect("Invalid label ID!") as i32)
                    }
                }
            },
            Expression::BuiltinCall { inner, name, args } => {
                let mut arguments = Vec::new();
                for arg in args {
                    arguments.push(self.resolve_expression(
                        constant_stack,
                        arg
                    )?);
                }
                Self::execute_builtin_call(&inner, &name, arguments)?
            }
        })
    }

    fn execute_builtin_call(
        inner: &InnerToken,
        name: &str,
        args: Vec<ExpressionResult>

    ) -> Result<ExpressionResult, LexerError> {
        unimplemented!("BuiltinCall Unsupported");
    }

    fn apply_binary_operator(
        inner: &InnerToken,
        op: Operator,
        left: ExpressionResult,
        right: ExpressionResult

    ) -> Result<ExpressionResult, LexerError> {
        match (&op, &left, &right) {
            _ => {
                Err(inner.error(format!("Unsupported binary operation: {} {} {}", left.as_str(), op.as_str(), right.as_str())))
            }
        }
    }

    fn apply_unary_operator(
        inner: &InnerToken,
        op: Operator,
        right: ExpressionResult

    ) -> Result<ExpressionResult, LexerError> {
        match (&op, &right) {
            // Integer
            (Operator::Plus, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(i.abs()))
            },
            (Operator::Minus, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(-i))
            },
            (Operator::BitNegate, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(!i))
            },
            (Operator::LogicalNot, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(if *i == 0 {
                    1

                } else {
                    0
                }))
            },
            // Float
            (Operator::Plus, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Float(OrderedFloat(f.abs())))
            },
            (Operator::Minus, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Float(OrderedFloat(-f.into_inner())))
            },
            (Operator::LogicalNot, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Integer(if *f == OrderedFloat(0.0) {
                    1

                } else {
                    0
                }))
            },
            // String
            (Operator::LogicalNot, ExpressionResult::String(s)) => {
                Ok(ExpressionResult::Integer(if s.is_empty() {
                    1

                } else {
                    0
                }))
            },
            _ => {
                Err(inner.error(format!("Unsupported unary operation: {} {}", op.as_str(), right.as_str())))
            }
        }
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use crate::lexer::stage::mocks::expr_lex;
    use crate::lexer::{ExpressionToken, LexerError};
    use super::{EvaluatorContext, ExpressionResult};

    fn const_expression<S: Into<String>>(s: S) -> ExpressionResult {
        const_expression_result(s).expect("Evaluator failed")
    }

    fn const_expression_error<S: Into<String>>(s: S) -> String {
        const_expression_result(s).err().unwrap().to_string()
    }

    fn const_expression_result<S: Into<String>>(s: S) -> Result<ExpressionResult, LexerError> {
        let token = expr_lex(s).tokens.remove(0);
        if let ExpressionToken::ConstExpression(_, _, expr) = token {
            let mut context = EvaluatorContext::new();
            let stack = Vec::new();
            context.resolve_expression(&stack, expr)

        } else {
            panic!("Not a constant expression");
        }
    }

    // Tests Expressions ------------------------------------------------------
    #[test]
    fn test_plain_expressions() {
        assert_eq!(const_expression("1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.1"), ExpressionResult::Float(OrderedFloat(1.1)));
        assert_eq!(const_expression("'Hello World'"), ExpressionResult::String("Hello World".to_string()));
    }

    // Tests Unary Expressions ------------------------------------------------
    #[test]
    fn test_unary_integer_expressions() {
        // TODO never parse - in include lexer and instead always apply negative unary operator?
        assert_eq!(const_expression("+1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("- 1"), ExpressionResult::Integer(-1));
        assert_eq!(const_expression("~1"), ExpressionResult::Integer(-2));
        assert_eq!(const_expression("!1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("!0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("!!0"), ExpressionResult::Integer(0));
    }

    #[test]
    fn test_unary_float_expressions() {
        assert_eq!(const_expression("+1.1"), ExpressionResult::Float(OrderedFloat(1.1)));
        assert_eq!(const_expression("- 1.1"), ExpressionResult::Float(OrderedFloat(-1.1)));
        assert_eq!(const_expression_error("~1.1"), "Unsupported unary operation: ~ Float");
        assert_eq!(const_expression("!0.1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("!0.0"), ExpressionResult::Integer(1));
    }

    #[test]
    fn test_unary_string_expressions() {
        assert_eq!(const_expression_error("+'Foo'"), "Unsupported unary operation: + String");
        assert_eq!(const_expression_error("-'Foo'"), "Unsupported unary operation: - String");
        assert_eq!(const_expression_error("~'Foo'"), "Unsupported unary operation: ~ String");
        assert_eq!(const_expression("!''"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("!'Foo'"), ExpressionResult::Integer(0));
    }

    // Tests Binary Expressions -----------------------------------------------

    // TODO all possible / valid combinations


    // Tests Builtin Call Expressions -----------------------------------------

    // TODO all Builtin macros

}

