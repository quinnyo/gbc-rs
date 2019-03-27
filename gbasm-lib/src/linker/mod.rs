// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Modules --------------------------------------------------------------------
mod section;
mod util;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{Lexer, LexerError, LexerToken, EntryStage, EntryToken};
use crate::expression::evaluator::{EvaluatorConstant, EvaluatorContext};
use self::section::{Section, SectionList};


// Linker Implementation ------------------------------------------------------
pub struct Linker {
    context: EvaluatorContext,
    sections: Vec<Section>
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
        let mut sections: Vec<Section> = Vec::new();
        for token in entry_tokens {
            if let EntryToken::SectionDeclaration { inner, name, segment_name, segment_offset, segment_size, bank_index } = token {

                // Parse options
                let name = util::opt_string(&inner, context.resolve_optional_expression(name)?, "Invalid section name")?;
                let segment_offset = util::opt_integer(&inner, context.resolve_optional_expression(segment_offset)?, "Invalid section offset")?;
                let segment_size = util::opt_integer(&inner, context.resolve_optional_expression(segment_size)?, "Invalid section size")?;
                let bank_index = util::opt_integer(&inner, context.resolve_optional_expression(bank_index)?, "Invalid section bank index")?;

                // If a offset is specified create a new section
                if let Some(offset) = segment_offset {
                    let id = sections.len();
                    sections.push(Section::new(id, segment_name, name, inner, Some(offset), segment_size, bank_index)?);
                    section_index = id;

                } else {
                    // If no offset is specified, search from the end for a matching hash
                    let hash = Section::default_hash(&segment_name, bank_index);
                    if let Some(id) = sections.iter().rev().find(|s| s.hash() == hash).map(|s| s.id) {
                        section_index = id;

                        // Insert a marker within the section
                        if let Some(section) = sections.get_mut(section_index) {
                            section.add_marker(inner, name);
                        }

                    // If no section is found create a new section
                    } else {
                        let id = sections.len();
                        sections.push(Section::new(id, segment_name, name, inner, None, segment_size, bank_index)?);
                        section_index = id;
                    }
                }

            } else if sections.is_empty() {
                return Err(token.error("Unexpected ROM entry before any section declaration".to_string()))

            } else if let Some(section) = sections.get_mut(section_index) {
                section.add_entry(&mut context, token)?;
            }
        }

        SectionList::initialize(&mut sections);
        SectionList::resolve(&mut sections, &mut context)?;

        // for s in &sections {
        //     println!("{}", s);
        // }

        Ok(Self {
            context,
            sections
        })
    }

    pub fn optimize_instructions(&mut self) -> Result<(), LexerError> {
        // Run passes until no more optimizations were applied
        while SectionList::optimize_instructions(&mut self.sections, &mut self.context) {
            SectionList::resolve(&mut self.sections, &mut self.context)?;
        }
        Ok(())
    }

    pub fn generate(&self, buffer: &mut [u8]) {
        for s in &self.sections {
            s.generate(buffer);
        }
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::Linker;
    use super::section::EntryData;
    use ordered_float::OrderedFloat;
    use crate::lexer::stage::mocks::{entry_lex, entry_lex_binary};
    use crate::expression::{Expression, ExpressionResult, ExpressionValue};
    use crate::expression::data::{DataAlignment, DataEndianess};

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

    fn linker_section_entries(linker: Linker) -> Vec<Vec<(usize, EntryData)>> {
        linker.sections.into_iter().map(|s| {
            s.entries.into_iter().map(|e| {
                (e.size, e.data)

            }).collect()

        }).collect()
    }

    fn linker_section_offsets(linker: Linker) -> Vec<Vec<(usize, usize)>> {
        linker.sections.into_iter().map(|s| {
            s.entries.into_iter().map(|e| {
                (e.offset, e.size)

            }).collect()

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
            "[ 0][               A]  ROMX[4000-7fff +0000][1]".to_string(),
            "[ 1][               B]  ROMX[4000-7fff +4000][2]".to_string()
        ]);
        assert_eq!(linker_sections(linker("SECTION 'A',ROMX,BANK[1]\nSECTION 'B',ROMX,BANK[2]\n")), vec![
            "[ 0][               A]  ROMX[4000-7fff +0000][1]".to_string(),
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
    fn test_section_bank_no_cutoff() {
        assert_eq!(linker_sections(linker("SECTION 'A',RAMX[$A000]\nSECTION 'B',RAMX[$A000],BANK[1]\nSECTION 'C',RAMX[$A000],BANK[2]")), vec![
            "[ 0][               A]  RAMX[a000-bfff +0000][0]".to_string(),
            "[ 1][               B]  RAMX[a000-bfff +2000][1]".to_string(),
            "[ 2][               C]  RAMX[a000-bfff +4000][2]".to_string()
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

    #[test]
    fn test_error_entry_outside_rom_segment() {
        assert_eq!(linker_error("SECTION WRAM0\nld a,a"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Instruction outside of ROM segment.\n\nld a,a\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nld a,[$0000]"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Instruction outside of ROM segment.\n\nld a,[$0000]\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nbrk"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Instruction outside of ROM segment.\n\nbrk\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nmsg 'foo'"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Instruction outside of ROM segment.\n\nmsg \'foo\'\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nDB 0"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Data Declaration outside of ROM segment.\n\nDB 0\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nDW 0"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Data Declaration outside of ROM segment.\n\nDW 0\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nDS 0 'Hello World'"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Data Declaration outside of ROM segment.\n\nDS 0 \'Hello World\'\n^--- Here");
        assert_eq!(linker_error("SECTION WRAM0\nDS 'Hello World'"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Data Declaration outside of ROM segment.\n\nDS \'Hello World\'\n^--- Here");
    }

    #[test]
    fn test_error_entry_outside_ram_segment() {
        assert_eq!(linker_error("SECTION ROM0\nDB"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Byte Variable outside of RAM segment.\n\nDB\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nDW"), "In file \"main.gb.s\" on line 2, column 1: Unexpected Word Variable outside of RAM segment.\n\nDW\n^--- Here");
    }

    // Labels -----------------------------------------------------------------
    #[test]
    fn test_section_entry_labels() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nglobal_label:\n.local_label:")), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global_label".to_string()
                }),
                (0, EntryData::Label {
                    id: 2,
                    name: "local_label".to_string()
                })
            ]
        ]);
    }

    // ROM Data Entries -------------------------------------------------------
    #[test]
    fn test_section_entry_data_rom_db() {
        // TODO value range errors
        // TODO test actual expression evaluation
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDB $10,$20")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (0, Expression::Value(ExpressionValue::Integer(16))),
                        (1, Expression::Value(ExpressionValue::Integer(32)))
                    ]),
                    bytes: None
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDB -1,-128")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (0, Expression::Value(ExpressionValue::Integer(-1))),
                        (1, Expression::Value(ExpressionValue::Integer(-128)))
                    ]),
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_rom_dw() {
        // TODO value range errors
        // TODO test actual expression evaluation
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDW $1000,$2000")), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (0, Expression::Value(ExpressionValue::Integer(4096))),
                        (1, Expression::Value(ExpressionValue::Integer(8192)))
                    ]),
                    bytes: None
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDW -1")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (0, Expression::Value(ExpressionValue::Integer(-1)))
                    ]),
                    bytes: None
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nBW $1000,$2000")), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Big,
                    expressions: Some(vec![
                        (0, Expression::Value(ExpressionValue::Integer(4096))),
                        (1, Expression::Value(ExpressionValue::Integer(8192)))
                    ]),
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_rom_incbin() {
        let linker = Linker::from_lexer(entry_lex_binary("SECTION ROM0\nINCBIN 'child.bin'", vec![1, 2, 3, 4])).expect("Linker failed");
        assert_eq!(linker_section_entries(linker), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1, 2, 3, 4])
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_rom_ds() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 5")), vec![
            vec![
                (5, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0, 0, 0, 0, 0])
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 'FOO'")), vec![
            vec![
                (3, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![70, 79, 79])
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 5 'FOO'")), vec![
            vec![
                (5, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![70, 79, 79, 0, 0])
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_entry_data_rom_ds() {
        assert_eq!(linker_error("SECTION ROM0\nDS -1"), "In file \"main.gb.s\" on line 2, column 1: Invalid storage capacity, expected a positive integer value instead.\n\nDS -1\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nDS -1 'FOO'"), "In file \"main.gb.s\" on line 2, column 1: Invalid storage capacity, expected a positive integer value instead.\n\nDS -1 \'FOO\'\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nDS 2 'FOO'"), "In file \"main.gb.s\" on line 2, column 1: Invalid storage capacity, specified capacity must be >= length of stored string data.\n\nDS 2 \'FOO\'\n^--- Here");
    }

    // RAM Data Entries -------------------------------------------------------
    #[test]
    fn test_section_entry_data_ram_db() {
        assert_eq!(linker_section_entries(linker("SECTION WRAM0\nDB")), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_ram_dw() {
        assert_eq!(linker_section_entries(linker("SECTION WRAM0\nDW")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_ram_ds() {
        assert_eq!(linker_section_entries(linker("SECTION WRAM0\nDS 8")), vec![
            vec![
                (8, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_ram_ds8() {
        assert_eq!(linker_section_entries(linker("SECTION WRAM0\nDS8 8")), vec![
            vec![
                (8, EntryData::Data {
                    alignment: DataAlignment::WithinWord,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_ram_ds16() {
        assert_eq!(linker_section_entries(linker("SECTION WRAM0\nDS16 8")), vec![
            vec![
                (8, EntryData::Data {
                    alignment: DataAlignment::Word,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: None
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_entry_data_ds16_alignment() {
        linker("SECTION ROM0[$0000]\n DS16 1");
        linker("SECTION ROM0[$0100]\n DS16 1");
        linker("SECTION ROM0[$1000]\n DS16 1");
        assert_eq!(linker_error("SECTION ROM0[$0001]\nDS16 1"), "In file \"main.gb.s\" on line 2, column 1: Invalid alignment of Data Declaration, \"DS16\" is required to start a low byte value of $00.\n\nDS16 1\n^--- Here");
    }

    #[test]
    fn test_error_section_entry_data_ds8_alignment() {
        linker("SECTION ROM0[$0000]\n DS8 256");
        linker("SECTION ROM0[$0100]\n DS8 256");
        linker("SECTION ROM0[$0080]\n DS8 128");
        assert_eq!(linker_error("SECTION ROM0[$0000]\nDS8 257"), "In file \"main.gb.s\" on line 2, column 1: Invalid alignment of Data Declaration, \"DS8\" is required to start and end within the same low byte.\n\nDS8 257\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0[$0100]\nDS8 257"), "In file \"main.gb.s\" on line 2, column 1: Invalid alignment of Data Declaration, \"DS8\" is required to start and end within the same low byte.\n\nDS8 257\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0[$0080]\nDS8 129"), "In file \"main.gb.s\" on line 2, column 1: Invalid alignment of Data Declaration, \"DS8\" is required to start and end within the same low byte.\n\nDS8 129\n^--- Here");
    }

    #[test]
    fn test_error_section_entry_data_ram_ds() {
        assert_eq!(linker_error("SECTION WRAM0\nDS -1"), "In file \"main.gb.s\" on line 2, column 1: Invalid storage capacity, expected a positive integer value instead.\n\nDS -1\n^--- Here");
    }

    // Address Evaluation -----------------------------------------------------

    // TODO test correct evaluation of label addresses
    // TODO test correct evaluation of label addresses across sections

    // Offsets ----------------------------------------------------------------

    #[test]
    fn test_section_entry_offsets() {
        assert_eq!(linker_section_offsets(linker("SECTION ROM0\nDS16 5\nDS8 3\nDS 4\nDS 10\nglobal_label:")), vec![
            vec![
                (0, 5),
                (5, 3),
                (8, 4),
                (12, 10),
                (22, 0)
            ]
        ]);
        assert_eq!(linker_section_offsets(linker("SECTION ROM0[$2000]\nDS16 5\nDS8 3\nDS 4\nDS 10\nglobal_label:")), vec![
            vec![
                (8192 + 0, 5),
                (8192 + 5, 3),
                (8192 + 8, 4),
                (8192 + 12, 10),
                (8192 + 22, 0)
            ]
        ]);
    }

    // TODO test further offset calculation


    // Instructions -----------------------------------------------------------

    // TODO

}

