// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, LexerError};
use crate::expression::{DataExpression, Expression, ExpressionValue, ExpressionResult, Operator};


// Expression Evaluator -------------------------------------------------------
pub struct EvaluatorConstant {
    pub inner: InnerToken,
    pub is_string: bool,
    pub value: DataExpression
}

pub struct ExpressionEvaluator;
impl ExpressionEvaluator {

    pub fn resolve_constant_expression(
        constants: &mut HashMap<String, ExpressionResult>,
        linker_constants: &HashMap<String, EvaluatorConstant>,
        labels_offsets: &HashMap<usize, usize>,
        inner: &InnerToken,
        name: &str,
        rom_offset: usize

    ) -> Result<ExpressionResult, LexerError> {
        if let Some(result) = constants.get(name) {
            Ok(result.clone())

        } else if let Some(EvaluatorConstant { inner, value, .. }) = linker_constants.get(name) {
            Self::resolve_expression(
                constants,
                linker_constants,
                labels_offsets,
                inner,
                &value.1,
                rom_offset
            )

        } else {
            unimplemented!("Undefined constant name");
        }
    }

    pub fn resolve_expression(
        constants: &mut HashMap<String, ExpressionResult>,
        linker_constants: &HashMap<String, EvaluatorConstant>,
        labels_offsets: &HashMap<usize, usize>,
        inner: &InnerToken,
        value: &Expression,
        rom_offset: usize

    ) -> Result<ExpressionResult, LexerError> {
        Ok(match value {
            Expression::Binary { inner, op, left, right } => {
                let left = Self::resolve_expression(
                    constants,
                    linker_constants,
                    labels_offsets,
                    &inner,
                    &*left,
                    rom_offset
                )?;
                let right = Self::resolve_expression(
                    constants,
                    linker_constants,
                    labels_offsets,
                    &inner,
                    &*right,
                    rom_offset
                )?;
                Self::apply_binary_operator(inner, op.clone(), left, right)?
            },
            Expression::Unary { inner, op, right  } => {
                let right = Self::resolve_expression(
                    constants,
                    linker_constants,
                    labels_offsets,
                    &inner,
                    &*right,
                    rom_offset
                )?;
                Self::apply_unary_operator(inner, op.clone(), right)?
            },
            Expression::Value(value) => {
                match value {
                    ExpressionValue::ConstantValue(inner, name) => {
                        let c = Self::resolve_constant_expression(
                            constants,
                            linker_constants,
                            labels_offsets,
                            inner,
                            name,
                            rom_offset
                        )?;
                        constants.insert(name.clone(), c.clone());
                        c
                    },
                    ExpressionValue::Integer(i) => ExpressionResult::Integer(*i),
                    ExpressionValue::Float(f) => ExpressionResult::Float(f.clone()),
                    ExpressionValue::String(s) => ExpressionResult::String(s.clone()),
                    ExpressionValue::OffsetAddress(_, offset) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(rom_offset as i32 + offset)
                    },
                    ExpressionValue::GlobalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(*labels_offsets.get(&id).expect("Invalid label ID!") as i32)
                    },
                    ExpressionValue::LocalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        ExpressionResult::Integer(*labels_offsets.get(&id).expect("Invalid label ID!") as i32)
                    }
                }
            },
            Expression::BuiltinCall { inner, args, .. } => {
                let mut arguments = Vec::new();
                for arg in args {
                    arguments.push(Self::resolve_expression(
                        constants,
                        linker_constants,
                        labels_offsets,
                        &inner,
                        arg,
                        rom_offset
                    )?);
                }
                Self::execute_builtin_call(inner, arguments)?
            }
        })
    }

    fn execute_builtin_call(
        inner: &InnerToken,
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
        match (op, left, right) {
            _ => {
                unimplemented!("Unsupported operation");
            }
        }
    }

    fn apply_unary_operator(
        inner: &InnerToken,
        op: Operator,
        right: ExpressionResult

    ) -> Result<ExpressionResult, LexerError> {
        match (op, right) {
            _ => {
                unimplemented!("Unsupported operation");
            }
        }
    }

}

