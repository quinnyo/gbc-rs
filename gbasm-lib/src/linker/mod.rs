// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Lexer, LexerError, EntryStage, EntryToken};
use crate::expression::{OptionalDataExpression, ExpressionResult};
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};
use crate::expression::evaluator::{EvaluatorConstant, EvaluatorContext};


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
struct LinkerSection {
    id: usize,
    inner: InnerToken,
    base: usize,
    offset: usize,
    size: usize
}

#[derive(Debug, Eq, PartialEq)]
struct LinkerEntry {
    inner: InnerToken,
    section_id: usize,
    offset: RomOffset,
    size: usize,
    data: LinkerData
}

#[derive(Debug, Eq, PartialEq)]
enum RomOffset {
    /// An offset that may shift around based on optimizations of entries that come before it
    Dynamic(usize),
    /// An offset that will not be shifted around by any optimizations performed on the rom
    Fixed(usize)
}

#[derive(Debug, Eq, PartialEq)]
enum LinkerData {
    Data {
        alignment: DataAlignment,
        endianess: DataEndianess,
        storage:  DataStorage
    },
    Label {
        id: usize,
        // use for label map later on
        name: String
    },
    Instruction {
        argument: OptionalDataExpression,
        value: Option<ExpressionResult>,
        is_debug: bool
    }
}


// Linker Implementation ------------------------------------------------------
pub struct Linker {
    context: EvaluatorContext,
    sections: Vec<LinkerSection>,
    entries: Vec<LinkerEntry>
}

impl Linker {

    pub fn from_lexer(lexer: Lexer<EntryStage>) -> Result<Self, LexerError> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        Self::new(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })
    }

    fn new(tokens: Vec<EntryToken>) -> Result<Self, LexerError> {

        let mut context = EvaluatorContext::new();

        // Extract and evaluate all constants
        let mut entry_tokens = Vec::new();
        for token in tokens {
            if let EntryToken::Constant { inner, is_string, value } = token {
                context.raw_constants.insert(inner.value.clone(), EvaluatorConstant {
                    inner,
                    is_string,
                    expression: value
                });

            } else {
                entry_tokens.push(token);
            }
        }
        context.resolve_constants()?;

        // TODO go through remaining stuff and create LinkerEntries
        let mut sections = Vec::new();
        let mut entries = Vec::new();
        let mut offset = 0;
        for token in entry_tokens {
            // TODO if the current section name != previous section or current section offset != None
                // TODO push a new section definition onto the stack and use it's offset going
                // forward


            // TODO resolve sizes of data values
            // TODO resolve sizes of instructions
            // TODO record labels with offset
            // TODO update current section offset / size
            // TODO resolve all Expression
            // TODO resolve constant sizes for EntryToken::Data's so all offsets (i.e. label addresses) can be calculated in
            // one sweep instead of having to do multiple passes

        }

        // TODO check for overlappings in the section list
        // TODO store section list for later use and reference in entries

        // TODO offsets can still change after this due to optimizations
        Ok(Self {
            context,
            sections,
            entries
        })
    }

    fn resolve_arguments(&mut self) {
        // TODO resolve all data values / arguments
            // TODO check value / argument types

        // TODO resolve all instruction arguments
            // TODO check argument types
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::Linker;
    use ordered_float::OrderedFloat;
    use crate::lexer::stage::mocks::entry_lex;
    use crate::expression::ExpressionResult;

    fn linker<S: Into<String>>(s: S) -> Linker {
        Linker::from_lexer(entry_lex(s.into())).expect("Linker failed")
    }

    fn linker_error<S: Into<String>>(s: S) -> String {
        Linker::from_lexer(entry_lex(s.into())).err().unwrap().to_string()
    }

    fn context_constants(linker: &Linker) -> Vec<(String, ExpressionResult)> {
        let mut constants = Vec::new();
        for (key, value) in &linker.context.constants {
            constants.push((key.clone(), value.clone()));
        }
        constants.sort_by(|a, b| {
            a.0.cmp(&b.0)
        });
        constants
    }

    // Constant Evaluation ----------------------------------------------------
    #[test]
    fn test_constant_eval_plain_values() {
        let l = linker("int EQU 1\nfloat EQU 3.14\nstring EQU 'Hello World'");
        assert_eq!(context_constants(&l), vec![
            ("float".to_string(), ExpressionResult::Float(OrderedFloat(3.14))),
            ("int".to_string(), ExpressionResult::Integer(1)),
            ("string".to_string(), ExpressionResult::String("Hello World".to_string()))
        ]);
    }

    #[test]
    fn test_constant_eval_referenced_values() {
        let l = linker("A EQU B\nB EQU C\nC EQU 2");
        assert_eq!(context_constants(&l), vec![
            ("A".to_string(), ExpressionResult::Integer(2)),
            ("B".to_string(), ExpressionResult::Integer(2)),
            ("C".to_string(), ExpressionResult::Integer(2))
        ]);
    }

    #[test]
    fn test_error_constant_eval_recursive() {
        assert_eq!(linker_error("A EQU B\nB EQU C\nC EQU A"), "In file \"main.gb.s\" on line 3, column 7: Recursive declaration of constant \"A\".\n\nC EQU A\n      ^--- Here\n\nInitial declaration was in file \"main.gb.s\" on line 1, column 1:\n\nA EQU B\n^--- Here");
    }

    #[test]
    fn test_error_constant_eval_undeclared() {
        assert_eq!(linker_error("A EQU B\nB EQU C\nC EQU D"), "In file \"main.gb.s\" on line 3, column 7: Reference to undeclared constant \"D\".\n\nC EQU D\n      ^--- Here");
    }

    // TODO test constant evaluation and errors
    // TODO test the type evaluation etc.
        // TODO move those tests into the Evaluator module?

    // TODO test entry conversion
    // TODO test data size resolution and data value resolution
        // TODO test if expression results fit into data width etc. ?
        // TODO TODO perform this in a second validation step or inline with entry_token
        // conversion?

    // TODO test section list / stack logic
    // TODO test section overlap detection

}

