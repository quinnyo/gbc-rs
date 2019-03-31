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

    pub fn from_lexer(
        lexer: Lexer<EntryStage>,
        strip_debug: bool,
        optimize: bool

    ) -> Result<Self, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        Ok(Self::new(lexer.tokens, strip_debug, optimize).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?)
    }

    fn new(
        tokens: Vec<EntryToken>,
        strip_debug: bool,
        optimize: bool

    ) -> Result<Self, LexerError> {

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

        if strip_debug {
            SectionList::strip_debug(&mut sections, &mut context)?;
        }

        if optimize {
            // Run passes until no more optimizations were applied
            while SectionList::optimize_instructions(&mut sections, &mut context) {
                SectionList::resolve(&mut sections, &mut context)?;
            }
        }

        SectionList::verify(&sections)?;

        Ok(Self {
            context,
            sections
        })
    }

    pub fn generate(&self, buffer: &mut [u8]) {
        SectionList::generate(&self.sections, buffer)
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::Linker;
    use super::section::EntryData;
    use ordered_float::OrderedFloat;
    use crate::lexer::InnerToken;
    use crate::lexer::stage::mocks::{entry_lex, entry_lex_binary};
    use crate::expression::{Expression, ExpressionResult, ExpressionValue, TEMPORARY_EXPRESSION_ID};
    use crate::expression::data::{DataAlignment, DataEndianess};

    fn linker<S: Into<String>>(s: S) -> Linker {
        Linker::from_lexer(entry_lex(s.into()), false, false).expect("Linker failed")
    }

    fn linker_error<S: Into<String>>(s: S) -> String {
        Linker::from_lexer(entry_lex(s.into()), false, false).err().unwrap().to_string()
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

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    macro_rules! mtk {
        ($start:expr, $end:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $parsed);
                t.set_macro_call_id($id);
                t
            }
        }
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
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDB $10,$20")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (0, Expression::Value(ExpressionValue::Integer(16)))),
                        (1, (1, Expression::Value(ExpressionValue::Integer(32))))
                    ]),
                    bytes: Some(vec![16, 32]),
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDB -1,-128")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (0, Expression::Value(ExpressionValue::Integer(-1)))),
                        (1, (1, Expression::Value(ExpressionValue::Integer(-128))))
                    ]),
                    bytes: Some(vec![255, 128]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_entry_data_rom_db() {
        assert_eq!(linker_error("SECTION ROM0\nDB 256"), "In file \"main.gb.s\" on line 2, column 1: Invalid byte data, expected a byte value in the range of -128 to 255 instead.\n\nDB 256\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nDB -129"), "In file \"main.gb.s\" on line 2, column 1: Invalid byte data, expected a byte value in the range of -128 to 255 instead.\n\nDB -129\n^--- Here");
    }

    #[test]
    fn test_section_entry_data_rom_dw_bw() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDW $1000,$2000")), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (2, (0, Expression::Value(ExpressionValue::Integer(4096)))),
                        (2, (1, Expression::Value(ExpressionValue::Integer(8192))))
                    ]),
                    bytes: Some(vec![0, 16, 0, 32]),
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDW -1")), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (2, (0, Expression::Value(ExpressionValue::Integer(-1))))
                    ]),
                    bytes: Some(vec![255, 255]),
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nBW $1000,$2000")), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Big,
                    expressions: Some(vec![
                        (2, (0, Expression::Value(ExpressionValue::Integer(4096)))),
                        (2, (1, Expression::Value(ExpressionValue::Integer(8192))))
                    ]),
                    bytes: Some(vec![16, 0, 32, 0]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_entry_data_rom_dw_bw() {
        assert_eq!(linker_error("SECTION ROM0\nDW 65536"), "In file \"main.gb.s\" on line 2, column 1: Invalid word data, expected a word value in the range of -32768 to 65535 instead.\n\nDW 65536\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nDW -32769"), "In file \"main.gb.s\" on line 2, column 1: Invalid word data, expected a word value in the range of -32768 to 65535 instead.\n\nDW -32769\n^--- Here");
    }

    #[test]
    fn test_section_entry_data_rom_incbin() {
        let linker = Linker::from_lexer(entry_lex_binary("SECTION ROM0\nINCBIN 'child.bin'", vec![1, 2, 3, 4]), false, false).expect("Linker failed");
        assert_eq!(linker_section_entries(linker), vec![
            vec![
                (4, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1, 2, 3, 4]),
                    debug_only: false
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
                    bytes: Some(vec![0, 0, 0, 0, 0]),
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 'FOO'")), vec![
            vec![
                (3, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![70, 79, 79]),
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 5 'FOO'")), vec![
            vec![
                (5, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![70, 79, 79, 0, 0]),
                    debug_only: false
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
                    bytes: None,
                    debug_only: false
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
                    bytes: None,
                    debug_only: false
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
                    bytes: None,
                    debug_only: false
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
                    bytes: None,
                    debug_only: false
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
                    bytes: None,
                    debug_only: false
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

    #[test]
    fn test_section_entry_data_label_evaluation() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 5\nglobal:\nDW global\nDB global")), vec![
            vec![
                (5, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0, 0, 0, 0, 0]),
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                }),
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (2, (1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(29, 35, "global"), 1))))
                    ]),
                    bytes: Some(vec![5, 0]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (2, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(39, 45, "global"), 1))))
                    ]),
                    bytes: Some(vec![5]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_data_label_evaluation_cross_section() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nDS 5\nglobal:\nDW global\nSECTION ROM0[$2000]\nDB global")), vec![
            vec![
                (5, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0, 0, 0, 0, 0]),
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                }),
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (2, (1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(29, 35, "global"), 1))))
                    ]),
                    bytes: Some(vec![5, 0]),
                    debug_only: false
                })
            ],
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (3, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(59, 65, "global"), 1))))
                    ]),
                    bytes: Some(vec![5]),
                    debug_only: false
                })
            ]
        ]);

    }

    // Macro Constant Evaluation ----------------------------------------------
    #[test]
    fn test_section_macro_constant_evaluation() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nFOO(C)\nC EQU 2\nMACRO FOO(@a) DB @a ENDMACRO")), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (0, Expression::Value(ExpressionValue::ConstantValue(mtk!(17, 18, "C", 0), "C".to_string()))))
                    ]),
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_section_macro_label_address_evaluation() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nFOO(global)\nglobal:\nMACRO FOO(@a) DB @a ENDMACRO")), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (0, Expression::Value(ExpressionValue::GlobalLabelAddress(mtk!(17, 23, "global", 0), 1))))
                    ]),
                    bytes: Some(vec![1]),
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_macro_defined_constant_evaluation() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nFOO(C)\nMACRO FOO(@a) @a EQU 2 ENDMACRO\n DB C")), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![
                        (1, (1, Expression::Value(ExpressionValue::ConstantValue(itk!(56, 57, "C"), "C".to_string()))))
                    ]),
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    // Offsets ----------------------------------------------------------------

    #[test]
    fn test_section_entry_offsets_data() {
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

    #[test]
    fn test_section_entry_instruction_offsets() {
        assert_eq!(linker_section_offsets(linker("SECTION ROM0\nld a,a\nld hl,$4000\nsrl a")), vec![
            vec![
                (0, 1),
                (1, 3),
                (4, 2)
            ]
        ]);
        assert_eq!(linker_section_offsets(linker("SECTION ROM0[$2000]\nld a,a\nld hl,$4000\nsrl a")), vec![
            vec![
                (8192, 1),
                (8193, 3),
                (8196, 2)
            ]
        ]);
    }

    #[test]
    fn test_section_entry_instruction_offsets_debug() {
        assert_eq!(linker_section_offsets(linker("SECTION ROM0\nmsg 'Hello World'\nbrk")), vec![
            vec![
                (0, 1),  // ld d,d
                (1, 2),  // jr
                (3, 1),  // 0x64
                (4, 1),  // 0x64
                (5, 1),  // 0x00
                (6, 1),  // 0x00
                (7, 11),  // String Bytes
                (18, 1)   // ld b,b,
            ]
        ]);
    }

    // Instructions -----------------------------------------------------------

    #[test]
    fn test_section_instructions() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nld a,a\nadd hl,de\nsrl a")), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 25,
                    expression: None,
                    bytes: vec![25],
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 319,
                    expression: None,
                    bytes: vec![203, 63],
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_section_instructions_with_arg() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nld a,$20\nld hl,$4000")), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 62,
                    expression: Some((0, Expression::Value(ExpressionValue::Integer(32)))),
                    bytes: vec![62, 32],
                    debug_only: false
                }),
                (3, EntryData::Instruction {
                    op_code: 33,
                    expression: Some((1, Expression::Value(ExpressionValue::Integer(16384)))),
                    bytes: vec![33, 0, 64],
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_instructions_with_arg() {
        assert_eq!(linker_error("SECTION ROM0\nld hl,$10000"), "In file \"main.gb.s\" on line 2, column 1: Invalid word argument, expected a word value in the range of -32768 to 65535 instead.\n\nld hl,$10000\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nld hl,-$8001"), "In file \"main.gb.s\" on line 2, column 1: Invalid word argument, expected a word value in the range of -32768 to 65535 instead.\n\nld hl,-$8001\n^--- Here");
    }

    #[test]
    fn test_section_instructions_with_arg_constants() {
        for (bit, op) in vec![(0, 71), (1, 79), (2, 87), (3, 95), (4, 103), (5, 111), (6, 119), (7, 127)] {
            assert_eq!(linker_section_entries(linker(format!("SECTION ROM0\nbit {},a", bit))), vec![
                vec![
                    (2, EntryData::Instruction {
                        op_code: 327,
                        expression: Some((0, Expression::Value(ExpressionValue::Integer(bit)))),
                        bytes: vec![203, op],
                        debug_only: false
                    })
                ]
            ]);
        }

        for (bit, op) in vec![(0, 135), (1, 143), (2, 151), (3, 159), (4, 167), (5, 175), (6, 183), (7, 191)] {
            assert_eq!(linker_section_entries(linker(format!("SECTION ROM0\nres {},a", bit))), vec![
                vec![
                    (2, EntryData::Instruction {
                        op_code: 391,
                        expression: Some((0, Expression::Value(ExpressionValue::Integer(bit)))),
                        bytes: vec![203, op],
                        debug_only: false
                    })
                ]
            ]);
        }

        for (bit, op) in vec![(0, 199), (1, 207), (2, 215), (3, 223), (4, 231), (5, 239), (6, 247), (7, 255)] {
            assert_eq!(linker_section_entries(linker(format!("SECTION ROM0\nset {},a", bit))), vec![
                vec![
                    (2, EntryData::Instruction {
                        op_code: 455,
                        expression: Some((0, Expression::Value(ExpressionValue::Integer(bit)))),
                        bytes: vec![203, op],
                        debug_only: false
                    })
                ]
            ]);
        }

        for (rst, op) in vec![(0, 199), (8, 207), (16, 215), (24, 223), (32, 231), (40, 239), (48, 247), (56, 255)] {
            assert_eq!(linker_section_entries(linker(format!("SECTION ROM0\nrst {}", rst))), vec![
                vec![
                    (1, EntryData::Instruction {
                        op_code: 199,
                        expression: Some((0, Expression::Value(ExpressionValue::Integer(rst)))),
                        bytes: vec![op],
                        debug_only: false
                    })
                ]
            ]);
        }
    }

    #[test]
    fn test_error_section_instructions_with_arg_constants() {
        assert_eq!(linker_error("SECTION ROM0\nbit -1,a"), "In file \"main.gb.s\" on line 2, column 1: Invalid constant value -1, one of the following values is required: 0, 1, 2, 3, 4, 5, 6, 7\n\nbit -1,a\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nbit 8,a"), "In file \"main.gb.s\" on line 2, column 1: Invalid constant value 8, one of the following values is required: 0, 1, 2, 3, 4, 5, 6, 7\n\nbit 8,a\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nrst 2"), "In file \"main.gb.s\" on line 2, column 1: Invalid constant value 2, one of the following values is required: 0, 8, 16, 24, 32, 40, 48, 56\n\nrst 2\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nrst 41"), "In file \"main.gb.s\" on line 2, column 1: Invalid constant value 41, one of the following values is required: 0, 8, 16, 24, 32, 40, 48, 56\n\nrst 41\n^--- Here");
    }

    #[test]
    fn test_section_instructions_jr() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nglobal:\njr z,global\njr @+4\njr @-1")), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                }),
                (2, EntryData::Instruction {
                    op_code: 40,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(26, 32, "global"), 1)))),
                    bytes: vec![40, 254],
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((1, Expression::Value(ExpressionValue::OffsetAddress(itk!(36, 39, "+4"), 4)))),
                    bytes: vec![24, 4],
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((2, Expression::Value(ExpressionValue::OffsetAddress(itk!(43, 46, "-1"), -1)))),
                    bytes: vec![24, 255],
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nvsync")), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 250,
                    expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(65345)))),
                    bytes: vec![250, 65, 255],
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 230,
                    expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(2)))),
                    bytes: vec![230, 2],
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 32,
                    expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::OffsetAddress(itk!(13, 18, "vsync"), -4)))),
                    bytes: vec![32, 252],
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nldsp hl,-3")), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 248,
                    expression: Some((0, Expression::Value(ExpressionValue::Integer(-3)))),
                    bytes: vec![248, 253],
                    debug_only: false
                })
            ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\njr foo\nfoo:\njr bar\nld a,a\nbar:")), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(16, 19, "foo"), 1)))),
                    bytes: vec![24, 0],
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "foo".to_string()
                }),
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(28, 31, "bar"), 2)))),
                    bytes: vec![24, 1],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 2,
                    name: "bar".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_error_section_instructions_jr_range() {
        linker("SECTION ROM0\njr global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_error("SECTION ROM0\njr global\nSECTION ROM0[130]\nglobal:"), "In file \"main.gb.s\" on line 2, column 1: Relative jump offset of 128 is out of range, expected a signed byte value in the range of -128 to 127 instead.\n\njr global\n^--- Here");
        linker("SECTION ROM0\nglobal:\nSECTION ROM0[126]\njr global");
        assert_eq!(linker_error("SECTION ROM0\nglobal:\nSECTION ROM0[127]\njr global"), "In file \"main.gb.s\" on line 4, column 1: Relative jump offset of -129 is out of range, expected a signed byte value in the range of -128 to 127 instead.\n\njr global\n^--- Here");
    }

    #[test]
    fn test_section_instructions_jp() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nglobal:\njp foo\njp global\nfoo:")), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                }),
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(24, 27, "foo"), 2)))),
                    bytes: vec![195, 6, 0],
                    debug_only: false }),

                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(31, 37, "global"), 1)))),
                    bytes: vec![195, 0, 0],
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 2,
                    name: "foo".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_instructions_debug_brk() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nbrk")), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 64,
                    expression: None,
                    bytes: vec![64],
                    debug_only: true
                })
            ]
        ]);
    }

    #[test]
    fn test_section_instructions_debug_msg() {
        assert_eq!(linker_section_offsets(linker("SECTION ROM0\nmsg 'Hello World'\nglobal:")), vec![
           vec![
                (0, 1),
                (1, 2),
                (3, 1),
                (4, 1),
                (5, 1),
                (6, 1),
                (7, 11),
                (18, 0)
           ]
        ]);
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nmsg 'Hello World'")), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 82,
                    expression: None,
                    bytes: vec![82],
                    debug_only: true
                }),
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((
                        TEMPORARY_EXPRESSION_ID,
                        Expression::Value(ExpressionValue::OffsetAddress(itk!(13, 16, "msg"), 15))
                    )),
                    bytes: vec![24, 15],
                    debug_only: true
                }),
                (1, EntryData::Instruction {
                    op_code: 100,
                    expression: None,
                    bytes: vec![100],
                    debug_only: true
                }),
                (1, EntryData::Instruction {
                    op_code: 100,
                    expression: None,
                    bytes: vec![100],
                    debug_only: true
                }),
                (1, EntryData::Instruction {
                    op_code: 0,
                    expression: None,
                    bytes: vec![0],
                    debug_only: true
                }),
                (1, EntryData::Instruction {
                    op_code: 0,
                    expression: None,
                    bytes: vec![0],
                    debug_only: true
                }),
                (11, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]),
                    debug_only: true
                })
            ]
        ]);
    }

    // Section Validation -----------------------------------------------------
    #[test]
    fn test_error_section_size_bounds() {
        linker("SECTION ROM0[$0000][2]\nld a,a\nld a,a");
        assert_eq!(linker_error("SECTION ROM0[$0000][2]\nld a,a\nld a,a\nld a,a"), "In file \"main.gb.s\" on line 1, column 1: Section contents exceeds allocated area $0000-$0001 by 1 byte(s)\n\nSECTION ROM0[$0000][2]\n^--- Here");
    }

    // Debug Stripping --------------------------------------------------------
    #[test]
    fn test_section_debug_strip_entries() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njr global\nmsg 'Hello World'\nglobal:\nld a,a"), true, false).expect("Debug stripping failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(16, 22, "global"), 1)))),
                    bytes: vec![24, 0],
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_section_debug_strip_offsets() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njr global\nmsg 'Hello World'\nglobal:\nld a,a"), true, false).expect("Debug stripping failed");
        assert_eq!(linker_section_offsets(l), vec![
            vec![
                (0, 2),
                (2, 0),
                (2, 1)
            ]
        ]);
    }

    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_section_optimize_lda0_to_xora() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld a,0"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_cp0_to_ora() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\ncp 0"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_ldaffxx_to_ldhaxx() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld a,[$FF05]"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_ldffxxa_to_ldhxxa() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld [$FF05],a"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_callret_to_jp() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nglobal:\ncall global\nret\nld a,a"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_jp_to_jr() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njp global\nSECTION ROM0[129]\nglobal:"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njp c,global\nSECTION ROM0[129]\nglobal:"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njp nc,global\nSECTION ROM0[129]\nglobal:"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njp z,global\nSECTION ROM0[129]\nglobal:"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\njp nz,global\nSECTION ROM0[129]\nglobal:"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_ldbldc_to_ldbc() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld b,1\nld c,2\nld a,a"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_lddlde_to_ldde() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld d,1\nld e,2\nld a,a"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_ldhldl_to_ldhl() {
        let l = Linker::from_lexer(entry_lex("SECTION ROM0\nld h,1\nld l,2\nld a,a"), true, true).expect("Optimization failed");
        assert_eq!(linker_section_entries(l), vec![
            vec![
            ]
        ]);
    }

}

