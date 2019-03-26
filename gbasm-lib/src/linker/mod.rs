// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Lexer, LexerError, EntryStage, EntryToken};
use crate::expression::{OptionalDataExpression, ExpressionResult};
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};
use crate::expression::evaluator::{EvaluatorConstant, EvaluatorContext};


// Statics --------------------------------------------------------------------
lazy_static! {
    static ref SECTION_DEFAULTS: HashMap<&'static str, SectionDefault> = {
        let mut map = HashMap::new();
        map.insert("ROM0", SectionDefault {
            base_address: 0x0000,
            size: 0x4000,
            is_rom: true,
            min_bank: None,
            max_bank: None
        });
        map.insert("ROMX", SectionDefault {
            base_address: 0x4000,
            size: 0x4000,
            is_rom: true,
            min_bank: Some(1),
            max_bank: Some(127)
        });
        map.insert("WRAM0", SectionDefault {
            base_address: 0xC000,
            size: 0x1000,
            is_rom: true,
            min_bank: None,
            max_bank: None
        });
        map.insert("WRAMX", SectionDefault {
            base_address: 0xD000,
            size: 0x1000,
            is_rom: true,
            min_bank: Some(1),
            max_bank: Some(1)
        });
        map.insert("HRAM", SectionDefault {
            base_address: 0xFF80,
            size: 0x80,
            is_rom: true,
            min_bank: None,
            max_bank: None
        });
        map.insert("RAM", SectionDefault {
            base_address: 0xA000,
            size: 0x2000,
            is_rom: true,
            min_bank: None,
            max_bank: None
        });
        map.insert("RAMX", SectionDefault {
            base_address: 0xA000,
            size: 0x2000,
            is_rom: true,
            min_bank: Some(0),
            max_bank: Some(7)
        });
        map
    };
}


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
struct SectionDefault {
    base_address: usize,
    size: usize,
    is_rom: bool,
    min_bank: Option<usize>,
    max_bank: Option<usize>
}

#[derive(Debug, Eq, PartialEq)]
struct LinkerSection {
    id: usize,
    hash: String,
    segment: String,
    inner: InnerToken,

    start_address: usize,
    end_address: usize,
    is_rom: bool,
    bank_offset: usize,
    bank: usize,

    entries: Vec<SectionEntry>
}

impl fmt::Display for LinkerSection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{: >2}] {: >5}[{:0>4x}-{:0>4x} +{:0>4x}][{}]", self.id, self.hash, self.start_address, self.end_address, self.bank_offset, self.bank)
    }
}

impl LinkerSection {
    fn add_entry(&mut self, _context: &mut EvaluatorContext, token: EntryToken) -> Result<(), LexerError> {
        match token {
            EntryToken::Instruction(_, _) => {
                // TODO error if ram section
                // TODO get instr size
            },
            EntryToken::InstructionWithArg(_, _, _) => {
                // TODO error if ram section
                // TODO get instr size
            },
            EntryToken::DebugInstruction(_, _) => {
                // TODO error if ram section
                // TODO get instr size
                // TODO handle debug mode
            },
            EntryToken::DebugInstructionWithArg(_, _, _) => {
                // TODO error if ram section
                // TODO handle debug mode
                // TODO get instr size
            },
            EntryToken::GlobalLabelDef(_, _) => {
                // TODO record labels with offset into context
            },
            EntryToken::LocalLabelDef(_, _) => {
                // TODO record labels with offset into context
            },
            EntryToken::Data { .. } => {
                // TODO data arguments are always const and can be resolved here
                // TODO resolve expressions / sizes of data values
                // TODO verify data alignment, argument value size etc.
            },
            _ => unreachable!()
        }
        Ok(())
    }

    fn resolve_addresses(&mut self, _context: &mut EvaluatorContext)  {
        //let mut current_size = 0;
        for e in &mut self.entries {
            // TODO update label offsets in self.context
            // TODO entry offsets here are based on self.start_address

            //current_size += e.size;
            //if current_size > self.size {
                // TODO check if section exceeds max size
            //}
        }
        //self.current_size = current_size;
    }

    fn resolve_instruction_arguments(&mut self, _context: &mut EvaluatorContext) {
        // TODO resolve all instruction arguments
            // TODO check argument types and whether they fit into the data slot size
    }

    fn optimize_instructions(&mut self, _context: &mut EvaluatorContext) -> bool {
        // TODO run instruction entries through optimizer
        false
    }

    fn generate(&self, buffer: &mut [u8]) {
        if self.is_rom {
            let mut offset = self.start_address + self.bank_offset;
            for e in &self.entries {
                 e.generate(&mut buffer[offset..]);
                 offset += e.size;
            }
        }
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

impl SectionEntry {
    fn generate(&self, _buffer: &mut [u8]) {
        // TODO write to buffer using the computed data and instruction values
    }
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
        op: usize,
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
        let mut section_index = 0;
        let mut sections: Vec<LinkerSection> = Vec::new();
        for token in entry_tokens {
            if let EntryToken::SectionDeclaration { inner, name, segment_name, segment_offset, segment_size, bank_index } = token {

                // Parse options
                // TODO test error not string
                let name = opt_string(&inner, context.resolve_optional_expression(name)?)?;
                // TODO test error not positive offset
                let segment_offset = opt_integer(&inner, context.resolve_optional_expression(segment_offset)?)?;
                // TODO test error not positive size
                let segment_size = opt_integer(&inner, context.resolve_optional_expression(segment_size)?)?;
                // TODO test error not positive index
                let bank_index = opt_integer(&inner, context.resolve_optional_expression(bank_index)?)?;

                // Section Hash
                let hash = if let Some(bank) = bank_index {
                    format!("{}-{}", segment_name, bank)

                } else {
                    segment_name.clone()
                };

                // If a offset is specified create a new section
                //let bank = bank_index.map(|i| i).or(defaults.min_bank).unwrap_or(0);
                if let Some(offset) = segment_offset {
                    let id = sections.len();
                    sections.push(Self::create_section(id, segment_name, hash, inner, Some(offset), segment_size, bank_index)?);
                    section_index = id;

                // If no offset is specified, search from the end for a matching NAME-BANK hash
                } else if let Some(id) = sections.iter().rev().find(|s| s.hash == hash).map(|s| s.id) {
                    section_index = id;

                // If no section is found create a new section
                } else {
                    let id = sections.len();
                    sections.push(Self::create_section(id, segment_name, hash, inner, None, segment_size, bank_index)?);
                    section_index = id;
                }

            } else if sections.is_empty() {
                // TODO error entry outside of section / before any section

            } else if let Some(section) = sections.get_mut(section_index) {
                section.add_entry(&mut context, token)?;
            }
        }

        // Sort sections by base address
        sections.sort_by(|a, b| {
            a.start_address.cmp(&b.start_address)
        });

        // Limit end_address of sections to next section start_address - 1
        let section_starts: Vec<(usize, String)> = sections.iter().skip(1).map(|s| {
            (s.start_address, s.segment.clone())

        }).collect();

        // TODO test
        for (section, (next_start, next_segment)) in sections.iter_mut().zip(section_starts.into_iter()) {
            if section.segment == next_segment {
                section.end_address = next_start - 1;
            }
        }

        // Resolve addresses and instruction argument
        for s in &mut sections {
            s.resolve_addresses(&mut context);
        }

        // TODO check for overlappings in the section list must only be done once since later on
        // sections can only get smaller

        for s in &mut sections {
            s.resolve_instruction_arguments(&mut context);
        }

        for s in &sections {
            println!("{}", s);
        }

        Ok(Self {
            context,
            sections
        })
    }

    pub fn optimize_instructions(&mut self) {
        loop {
            // Run passes until no more optimizations were applied
            let mut optimzations_applied = false;
            for s in &mut self.sections {
                optimzations_applied |= s.optimize_instructions(&mut self.context);
            }
            if !optimzations_applied {
                break;
            }
            for s in &mut self.sections {
                s.resolve_addresses(&mut self.context);
            }
            for s in &mut self.sections {
                s.resolve_instruction_arguments(&mut self.context);
            }
        }
    }

    pub fn generate(&self, buffer: &mut [u8]) {
        for s in &self.sections {
            s.generate(buffer);
        }
    }

    fn create_section(
        id: usize,
        segment: String,
        hash: String,
        inner: InnerToken,
        segment_offset: Option<usize>,
        segment_size: Option<usize>,
        segment_bank: Option<usize>

    ) -> Result<LinkerSection, LexerError> {

        let defaults = SECTION_DEFAULTS.get(segment.as_str()).expect("Invalid segment name");

        // Bank
        let bank = if segment_bank.is_none() && defaults.min_bank.is_some() {
            defaults.min_bank.unwrap_or(0)

        } else {
            segment_bank.unwrap_or(0)
        };

        // TODO Check if the segment is banked
        // TODO Check for negative bank indicies
        // TODO Check for max bank

        // Size
        let mut size = segment_size.unwrap_or(defaults.size);
        if size == 0 {
            // Fix old workaround from gbasm-js
            size = defaults.size;
        }

        // For sections with specified offsets we still need to correct for banking
        let (start_address, end_address, bank_offset) = if let Some(offset) = segment_offset {
            // TODO test
            if bank == 0 {
                (
                    offset,
                    defaults.base_address + size,
                    0
                )

            // TODO test
            } else {
                (
                    offset,
                    defaults.base_address + size,
                    (bank - 1) * defaults.size
                )
            }

        // Set default offset if not specified
        } else if bank == 0 {
            // TODO test
            (
                defaults.base_address,
                defaults.base_address + size,
                0
            )

        } else {
            // TODO test
            (
                defaults.base_address,
                defaults.base_address + size,
                (bank - 1) * defaults.size
            )
        };

        // TODO check if start_address is inside base_address - base_address + size range
        Ok(LinkerSection {
            id,
            hash,
            segment,
            inner,
            start_address,
            end_address: end_address - 1,
            is_rom: defaults.is_rom,
            bank_offset,
            bank,
            entries: Vec::new()
        })

    }

}

fn opt_string(
    inner: &InnerToken,
    result: Option<ExpressionResult>

) -> Result<Option<String>, LexerError> {
    match result {
        Some(ExpressionResult::String(s)) => {
            Ok(Some(s))
        },
        None => Ok(None),
        _ => {
            Err(inner.error("Expected a string value instead.".to_string()))
        }
    }
}

fn opt_integer(
    inner: &InnerToken,
    result: Option<ExpressionResult>

) -> Result<Option<usize>, LexerError> {
    match result {
        Some(ExpressionResult::Integer(i)) => {
            if i >= 0 {
                Ok(Some(i as usize))

            } else {
                Err(inner.error("Expected a positive integer value instead.".to_string()))
            }
        },
        None => Ok(None),
        _ => {
            Err(inner.error("Expected a positive integer value instead.".to_string()))
        }
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
        let l = linker("SECTION 'A',ROMX[$4000]");
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

