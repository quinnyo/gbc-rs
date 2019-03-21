// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Lexer, LexerError, EntryStage, EntryToken};
use crate::expression::{OptionalDataExpression, ExpressionResult};
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};
use crate::expression::evaluator::{EvaluatorConstant, ExpressionEvaluator};


// Types ----------------------------------------------------------------------
struct LinkerSection {
    inner: InnerToken,
    base: usize,
    offset: usize,
    size: usize
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
                raw_constants.insert(inner.value.clone(), EvaluatorConstant {
                    inner,
                    is_string,
                    value
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

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::Linker;
}

