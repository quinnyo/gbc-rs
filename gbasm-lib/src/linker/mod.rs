// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Lexer, LexerError, EntryStage, EntryToken};
use crate::expression::{DataExpression, OptionalDataExpression, Expression, ExpressionValue, ExpressionResult};
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};


// Types ----------------------------------------------------------------------
struct LinkerSection {
    inner: InnerToken,
    base: usize,
    offset: usize,
    size: usize
}

struct LinkerConstant {
    inner: InnerToken,
    is_string: bool,
    value: DataExpression,
    result: Option<ExpressionResult>
}

enum LinkerEntry {
    Data {
        inner: InnerToken,
        offset: usize,
        size: usize,
        alignment: DataAlignment,
        endianess: DataEndianess,
        storage:  DataStorage
    },
    Label {
        inner: InnerToken,
        id: usize,
        offset: usize,
        size: usize,
    },
    Instruction {
        inner: InnerToken,
        argument: OptionalDataExpression,
        offset: usize,
        size: usize,
        is_debug: bool
    }
}


// Linker Implementation ------------------------------------------------------
pub struct Linker {
    constants: HashMap<String, ExpressionResult>,
    labels_offsets: HashMap<usize, usize>,
    entries: Vec<LinkerEntry>
}

impl Linker {

    pub fn from_lexer(lexer: Lexer<EntryStage>) -> Result<Self, LexerError> {

        // TODO hash map of constants
        let mut raw_constants = HashMap::new();

        // Extract all Constants
        let mut entry_tokens = Vec::new();
        for token in lexer.tokens {
            if let EntryToken::Constant { inner, is_string, value } = token {
                raw_constants.insert(inner.value.clone(), LinkerConstant {
                    inner,
                    is_string,
                    value,
                    result: None
                });

            } else {
                entry_tokens.push(token);
            }
        }

        // Evaluate extracted constants
        let mut constants = HashMap::new();

        // TODO resolve constant expressions

        // TODO resolve all Expression
        // TODO resolve constant sizes for EntryToken::Data's so all offsets (i.e. label addresses) can be calculated in
        // one sweep instead of having to do multiple passes

        // TODO go through remaining stuff and create LinkerEntries
        let mut sections: HashMap<String, Vec<LinkerSection>> = HashMap::new();
        let mut labels_offsets = HashMap::new();
        let mut entries = Vec::new();
        let mut offset = 0;
        for token in entry_tokens {
            // TODO section definitions change the section
                // TODO if no offset is given to a section re-use the last entry with the same name
                    // TODO otherwise push a new entry
                // TODO check if section was already defined and re-use it

            // TODO resolve sizes of data values
            // TODO resolve sizes of instructions
            // TODO record labels with offset
            // TODO update current section offset / size
        }
        Ok(Self {
            constants,
            labels_offsets,
            entries
        })
    }

    fn resolve_arguments(&mut self) {
        // TODO resolve all data values / arguments
        // TODO resolve all instruction arguments
    }

    // TODO compute expressions
    fn resolve_constant_expression(
        constants: &mut HashMap<String, ExpressionResult>,
        labels_offsets: &HashMap<usize, usize>,
        name: String,
        offset: usize,
        expr: DataExpression

    ) -> Result<ExpressionResult, LexerError> {
        // TODO compute expression
        match expr.1 {
            Expression::Binary { .. } => {
                // TODO evalute left
                // TODO evalute right
                // TODO apply operator based on return types
            },
            Expression::Unary { ..  } => {
                // TODO evalute right
                // TODO apply operator based on return type
            },
            Expression::Value(value) => {
                match value {
                    ExpressionValue::ConstantValue(_, _) => {
                        // TODO Self::resolve_constant_expression()
                    },
                    ExpressionValue::Integer(i) => {
                        // TODO ExpressionResult::Integer(i)
                    },
                    ExpressionValue::Float(f) => {
                        // TODO ExpressionResult::Float(f)
                    },
                    ExpressionValue::String(s) => {
                        // TODO ExpressionResult::String(s)
                    },
                    ExpressionValue::OffsetAddress(_, value) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        // TODO ExpressionResult::Integer(offset + value)
                    },
                    ExpressionValue::GlobalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        // TODO ExpressionResult::Integer(labels_offsets.get(id))
                    },
                    ExpressionValue::LocalLabelAddress(_, id) => {
                        // TODO jr Instructions need to convert to a relative value using their own offset
                        // TODO ExpressionResult::Integer(labels_offsets.get(id))
                    }
                }
                // TODO convert into ExpressionResult
            },
            Expression::BuiltinCall { .. } => {

            }
        }
        // TODO set result
        Ok(ExpressionResult::Integer(0))
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::Linker;
}

