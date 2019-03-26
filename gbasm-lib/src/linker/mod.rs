// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::fmt;
use std::error::Error;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Lexer, LexerError, LexerToken, EntryStage, EntryToken};
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
    name: String,
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
        write!(f, "[{: >2}][{: >16}] {: >5}[{:0>4x}-{:0>4x} +{:0>4x}][{}]", self.id, self.name, self.segment, self.start_address, self.end_address, self.bank_offset, self.bank)
    }
}

impl LinkerSection {
    fn add_entry(&mut self, _context: &mut EvaluatorContext, token: EntryToken) -> Result<(), LexerError> {
        match token {
            EntryToken::Instruction(_, _) => {
                // TODO error if rom section
                // TODO get instr size
            },
            EntryToken::InstructionWithArg(_, _, _) => {
                // TODO error if rom section
                // TODO get instr size
            },
            EntryToken::DebugInstruction(_, _) => {
                // TODO error if rom section
                // TODO get instr size
                // TODO handle debug mode
            },
            EntryToken::DebugInstructionWithArg(_, _, _) => {
                // TODO error if rom section
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
                // TODO check if rom section
                // TODO data arguments are always const and can be resolved here
                // TODO resolve expressions / sizes of data values
                // TODO verify data alignment, argument value size etc.
            },
            _ => unreachable!()
        }
        Ok(())
    }

    fn add_sub_section(&mut self, inner: InnerToken, name: Option<String>) {
        // TODO push related entry
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
    },
    SubSection {
        name: String
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
                let name = opt_string(&inner, context.resolve_optional_expression(name)?, "Invalid section name")?;
                let segment_offset = opt_integer(&inner, context.resolve_optional_expression(segment_offset)?, "Invalid section offset")?;
                let segment_size = opt_integer(&inner, context.resolve_optional_expression(segment_size)?, "Invalid section size")?;
                let bank_index = opt_integer(&inner, context.resolve_optional_expression(bank_index)?, "Invalid section bank index")?;

                // If a offset is specified create a new section
                if let Some(offset) = segment_offset {
                    let id = sections.len();
                    sections.push(Self::create_section(id, segment_name, name, inner, Some(offset), segment_size, bank_index)?);
                    section_index = id;

                // If no offset is specified, search from the end for a matching NAME-BANK hash
                } else {

                    // Section Hash
                    let defaults = SECTION_DEFAULTS.get(segment_name.as_str()).expect("Invalid segment name");
                    let hash = (&segment_name, bank_index.or(defaults.min_bank).unwrap_or(0));

                    if let Some(id) = sections.iter().rev().find(|s| (&s.segment, s.bank) == hash).map(|s| s.id) {
                        section_index = id;
                        if let Some(section) = sections.get_mut(section_index) {
                            section.add_sub_section(inner, name);
                        }

                    // If no section is found create a new section
                    } else {
                        let id = sections.len();
                        sections.push(Self::create_section(id, segment_name, name, inner, None, segment_size, bank_index)?);
                        section_index = id;
                    }
                }

            } else if sections.is_empty() {
                return Err(token.error("Unexpected ROM entry before any section declaration".to_string()))

            } else if let Some(section) = sections.get_mut(section_index) {
                section.add_entry(&mut context, token)?;
            }
        }

        // Sort sections by base address
        sections.sort_by(|a, b| {
            if a.start_address == b.start_address {
                a.bank.cmp(&b.bank)

            } else {
                a.start_address.cmp(&b.start_address)
            }
        });

        // Limit end_address of sections to next section start_address - 1
        let section_starts: Vec<(usize, String)> = sections.iter().skip(1).map(|s| {
            (s.start_address, s.segment.clone())

        }).collect();

        for (section, (next_start, next_segment)) in sections.iter_mut().zip(section_starts.into_iter()) {
            if section.segment == next_segment {
                if section.end_address >= next_start {
                    section.end_address = next_start - 1;
                }
            }
        }

        // Resolve addresses and instruction argument
        for s in &mut sections {
            s.resolve_addresses(&mut context);
        }

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
        name: Option<String>,
        inner: InnerToken,
        segment_offset: Option<usize>,
        segment_size: Option<usize>,
        segment_bank: Option<usize>

    ) -> Result<LinkerSection, LexerError> {

        let defaults = SECTION_DEFAULTS.get(segment.as_str()).expect("Invalid segment name");

        // Bank
        let bank = if segment_bank.is_none() && defaults.min_bank.is_some() {
            defaults.min_bank.unwrap_or(0)

        } else if let Some(bank) = segment_bank {
            let min = defaults.min_bank.unwrap_or(0);
            let max = defaults.max_bank.unwrap_or(0);
            if defaults.min_bank.is_none() {
                return Err(inner.error(format!("Invalid section bank index of {}, section does not support banking.", bank)))

            } else if bank < min || bank > max {
                return Err(inner.error(format!("Invalid section bank index of {}, must lie between {} and {}.", bank, min ,max)))

            } else {
                bank
            }

        } else {
            0
        };

        // For sections with specified offsets we still need to correct for banking
        let (start, default_end) = (defaults.base_address, defaults.base_address + defaults.size);
        let (start_address, bank_offset) = if let Some(offset) = segment_offset {
            if bank == 0 {
                (offset, 0)

            } else {
                (offset, (bank - 1) * defaults.size)
            }

        // Set default offset if not specified
        } else if bank == 0 {
            (start, 0)

        } else {
            (start, (bank - 1) * defaults.size)
        };

        // Calculate section size
        let end_address = if let Some(size) = segment_size {
            // Workaround incorrect gbasm-js behaviour
            if size == 0  {
                // consume rest of available section
                cmp::min(start_address + defaults.size, default_end)

            } else {
                start_address + size
            }

        } else {
            // consume rest of available section
            cmp::min(start_address + defaults.size, default_end)
        };

        // Validate that start_address is in range of section bounds
        if start_address < start || start_address >= default_end {
            return Err(inner.error(format!("Invalid section offset of ${:0>4x}, must lie between >=${:0>4x} and <=${:0>4x}", start_address, start, default_end - 1)));

        // Validate that section does not exceed size bounds
        } else if end_address > default_end {
            let size = end_address - start_address;
            let exceed = end_address - default_end;
            return Err(inner.error(format!("Invalid section size of ${:0>4x}, may not exceeed upper section bound at ${:0>4x}. Exceeds bound by {} bytes(s).", size, default_end, exceed)));
        }

        Ok(LinkerSection {
            id,
            name: name.unwrap_or_else(|| "".to_string()),
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
    result: Option<ExpressionResult>,
    msg: &'static str

) -> Result<Option<String>, LexerError> {
    match result {
        Some(ExpressionResult::String(s)) => {
            Ok(Some(s))
        },
        None => Ok(None),
        _ => {
            Err(inner.error(format!("{}, expected a string value instead.", msg)))
        }
    }
}

fn opt_integer(
    inner: &InnerToken,
    result: Option<ExpressionResult>,
    msg: &'static str

) -> Result<Option<usize>, LexerError> {
    match result {
        Some(ExpressionResult::Integer(i)) => {
            if i >= 0 {
                Ok(Some(i as usize))

            } else {
                Err(inner.error(format!("{}, expected a positive integer value instead.", msg)))
            }
        },
        None => Ok(None),
        _ => {
            Err(inner.error(format!("{}, expected a positive integer value instead.", msg)))
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

    fn linker_sections(linker: Linker) -> Vec<String> {
        linker.sections.iter().map(|s| {
            format!("{}", s)

        }).collect()
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
        assert_eq!(linker_error("ld a,a"), "In file \"main.gb.s\" on line 1, column 1: Unexpected ROM entry before any section declaration\n\nld a,a\n^--- Here");
    }

    #[test]
    fn test_error_section_declaration() {
        assert_eq!(linker_error("SECTION 4,ROM0"), "In file \"main.gb.s\" on line 1, column 1: Invalid section name, expected a string value instead.\n\nSECTION 4,ROM0\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0['foo']"), "In file \"main.gb.s\" on line 1, column 1: Invalid section offset, expected a positive integer value instead.\n\nSECTION ROM0[\'foo\']\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0[-1]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section offset, expected a positive integer value instead.\n\nSECTION ROM0[-1]\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0[$0000],BANK['Foo']"), "In file \"main.gb.s\" on line 1, column 1: Invalid section bank index, expected a positive integer value instead.\n\nSECTION ROM0[$0000],BANK[\'Foo\']\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0[$0000],BANK[-1]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section bank index, expected a positive integer value instead.\n\nSECTION ROM0[$0000],BANK[-1]\n^--- Here");
    }

    #[test]
    fn test_section_initial() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROM0")), vec![
            "[ 0][               A]  ROM0[0000-3fff +0000][0]".to_string()
        ]);
    }

    #[test]
    fn test_section_initial_with_offset() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROM0[$1000]")), vec![
            "[ 0][               A]  ROM0[1000-3fff +0000][0]".to_string()
        ]);
    }

    #[test]
    fn test_section_initial_banked() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX")), vec![
            "[ 0][               A]  ROMX[4000-7fff +0000][1]".to_string()
        ]);
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX,BANK[1]")), vec![
            "[ 0][               A]  ROMX[4000-7fff +0000][1]".to_string()
        ]);
    }

    #[test]
    fn test_section_initial_banked_with_offset() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX[$5000]")), vec![
            "[ 0][               A]  ROMX[5000-7fff +0000][1]".to_string()
        ]);
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX[$5000],BANK[1]")), vec![
            "[ 0][               A]  ROMX[5000-7fff +0000][1]".to_string()
        ]);
    }

    #[test]
    fn test_section_merge_banked() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX[$5000]\nSECTION 'B',ROMX,BANK[1]\n")), vec![
            "[ 0][               A]  ROMX[5000-7fff +0000][1]".to_string()
        ]);
    }

    #[test]
    fn test_section_append_banked() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX\nSECTION 'B',ROMX,BANK[2]\n")), vec![
            "[ 0][               A]  ROMX[4000-3fff +0000][1]".to_string(),
            "[ 1][               B]  ROMX[4000-7fff +4000][2]".to_string()
        ]);
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX,BANK[1]\nSECTION 'B',ROMX,BANK[2]\n")), vec![
            "[ 0][               A]  ROMX[4000-3fff +0000][1]".to_string(),
            "[ 1][               B]  ROMX[4000-7fff +4000][2]".to_string()
        ]);
    }

    #[test]
    fn test_section_multiple() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROM0[$0100]\nSECTION 'B',ROMX[$4000]\nSECTION 'C',HRAM")), vec![
            "[ 0][               A]  ROM0[0100-3fff +0000][0]".to_string(),
            "[ 1][               B]  ROMX[4000-7fff +0000][1]".to_string(),
            "[ 2][               C]  HRAM[ff80-ffff +0000][0]".to_string()
        ]);
    }

    #[test]
    fn test_section_multiple_cutoff() {
        assert_eq!(linker_sections(linker("SECTION 'A',ROM0[$0100]\nSECTION 'B',ROM0[$0200]\nSECTION 'C',ROM0[$1000]")), vec![
            "[ 0][               A]  ROM0[0100-01ff +0000][0]".to_string(),
            "[ 1][               B]  ROM0[0200-0fff +0000][0]".to_string(),
            "[ 2][               C]  ROM0[1000-3fff +0000][0]".to_string()
        ]);
    }

    #[test]
    fn test_error_section_bank() {
        assert_eq!(linker_error("SECTION ROM0,BANK[1]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section bank index of 1, section does not support banking.\n\nSECTION ROM0,BANK[1]\n^--- Here");
        assert_eq!(linker_error("SECTION RAMX,BANK[8]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section bank index of 8, must lie between 0 and 7.\n\nSECTION RAMX,BANK[8]\n^--- Here");
    }

    #[test]
    fn test_error_section_offset_in_range() {
        linker("SECTION ROM0[$3FFF]");
        assert_eq!(linker_error("SECTION ROM0[$4000]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section offset of $4000, must lie between >=$0000 and <=$3fff\n\nSECTION ROM0[$4000]\n^--- Here");
        linker("SECTION ROMX[$4000]");
        assert_eq!(linker_error("SECTION ROMX[$3FFF]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section offset of $3fff, must lie between >=$4000 and <=$7fff\n\nSECTION ROMX[$3FFF]\n^--- Here");
        linker("SECTION ROMX[$7FFF]");
        assert_eq!(linker_error("SECTION ROMX[$8000]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section offset of $8000, must lie between >=$4000 and <=$7fff\n\nSECTION ROMX[$8000]\n^--- Here");
    }

    #[test]
    fn test_error_section_size_in_range() {
        linker("SECTION ROM0[$0000][$4000]");
        assert_eq!(linker_error("SECTION ROM0[$0000][$4001]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section size of $4001, may not exceeed upper section bound at $4000. Exceeds bound by 1 bytes(s).\n\nSECTION ROM0[$0000][$4001]\n^--- Here");
        linker("SECTION ROMX[$4000][$4000]");
        assert_eq!(linker_error("SECTION ROMX[$6000][$2001]"), "In file \"main.gb.s\" on line 1, column 1: Invalid section size of $2001, may not exceeed upper section bound at $8000. Exceeds bound by 1 bytes(s).\n\nSECTION ROMX[$6000][$2001]\n^--- Here");
    }

}

