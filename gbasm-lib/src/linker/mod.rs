// STD Dependencies -----------------------------------------------------------
use std::error::Error;


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
    calculated_size: usize,
    max_size: usize,
    entries: Vec<SectionEntry>
}

impl LinkerSection {
    fn add_entry(&mut self, context: &mut EvaluatorContext, token: EntryToken) -> Result<(), LexerError> {
        let size = match token {
            EntryToken::Instruction(_, _) => {
                // TODO get instr size
                0
            },
            EntryToken::InstructionWithArg(_, _, _) => {
                // TODO get instr size
                0
            },
            EntryToken::DebugInstruction(_, _) => {
                // TODO get instr size
                // TODO handle debug mode
                0
            },
            EntryToken::DebugInstructionWithArg(_, _, _) => {
                // TODO handle debug mode
                // TODO get instr size
                0
            },
            EntryToken::GlobalLabelDef(_, _) => {
                // TODO record labels with offset
                0
            },
            EntryToken::LocalLabelDef(_, _) => {
                // TODO record labels with offset
                0
            },
            EntryToken::Data { .. } => {
                // TODO data arguments are always const and can be resolved here
                // TODO resolve expressions / sizes of data values
                // TODO verify data alignment, argument value size etc.
                0
            },
            _ => unreachable!()
        };
        self.offset += size;
        // TODO check if section exceeds max size
        Ok(())
    }

    fn update_offsets(&mut self)  {
        // TODO go through all entries
        // TODO use Fixed Offsets as starting points
        // TODO update Dynamic Offsets
        // TODO update label offsets in self.context
        // TODO when label is inside bank subtract ROM bank offset
            // TODO e.g. label is in bank 1 at 0, so ROM is 4000 but label offset is still 0
    }

    fn resolve_arguments(&mut self) {
        // TODO resolve all instruction arguments
            // TODO check argument types
    }

    fn optimize(&mut self) {
        // TODO run entries through optimizer
        // TODO update_offsets()
        // TODO resolve_arguments()
    }

}

#[derive(Debug, Eq, PartialEq)]
struct SectionEntry {
    inner: InnerToken,
    section_id: usize,
    offset: usize,
    size: usize,
    data: EntryData
}

#[derive(Debug, Eq, PartialEq)]
enum EntryData {
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
    sections: Vec<LinkerSection>
}

impl Linker {

    pub fn from_lexer(lexer: Lexer<EntryStage>) -> Result<Self, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        Ok(Self::new(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?)
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

        // Map entries to sections
        let mut sections: Vec<LinkerSection> = Vec::new();
        for token in entry_tokens {
            if let EntryToken::SectionDeclaration { .. } = token {
                // TODO if the current section name != previous section or current section offset != None
                    // TODO push a new section definition onto the stack and use it's offset going
                    // forward

            } else if let Some(section) = sections.last_mut() {
                section.add_entry(&mut context, token)?;

            } else {
                // TODO error entry outside of section / before any section
            }
        }

        // TODO check for overlappings in the section list
        Ok(Self {
            context,
            sections
        })
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

    // Section Mapping --------------------------------------------------------
    #[test]
    fn test_error_entry_before_any_section() {

    }

    // TODO handle banks when computing offsets

    #[test]
    fn test_section_initial() {
        // TODO ROM0[$0000]
    }

    #[test]
    fn test_section_initial_with_default_base() {
        // TODO ROM0
    }

    #[test]
    fn test_section_follow_up_append() {
        // TODO ROM0[$0000]
        // TODO ROM0
    }

    #[test]
    fn test_section_follow_up_initial() {
        // TODO ROM0[$0000]
        // TODO ROM0[$2000]
    }

    // TODO test entry conversion
    // TODO test data size resolution and data value resolution
        // TODO test if expression results fit into data width etc. ?
        // TODO TODO perform this in a second validation step or inline with entry_token
        // conversion?

    // TODO test section list / stack logic
    // TODO test section overlap detection

}

