// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::collections::VecDeque;


// Internal Dependencies ------------------------------------------------------
use crate::expression::{OptionalDataExpression, Expression, ExpressionValue};
use super::section::entry::{EntryData, SectionEntry};
use super::util::instruction;


// Low Level Instruction Optimizer --------------------------------------------
pub fn optimize_section_entries(entries: &mut Vec<SectionEntry>) -> bool {

    fn get_instruction(entries: &[SectionEntry], i: usize) -> Option<(u16, i32, &OptionalDataExpression, &[u8])> {
        if let Some(entry) = entries.get(i) {
            if let EntryData::Instruction { ref op_code, ref bytes, ref expression, volatile, .. } = entry.data {
                if volatile {
                    None

                } else {
                    Some((*op_code, (entry.offset + entry.size) as i32, &expression, bytes))
                }

            } else {
                None
            }

        } else {
            None
        }
    }

    // Do a dry run to figure out of there is anything to optimize at all
    // But keep the optimized entries so we don't have to generate them twice
    let mut i = 0;
    let mut optimized_entries = VecDeque::new();
    let mut optimized_length = 0;
    while i < entries.len() {
        if let Some((op_code, offset, expression, bytes)) = get_instruction(&entries, i) {
            let b = get_instruction(&entries, i + 1);
            let c = get_instruction(&entries, i + 2);
            if let Some((remove_count, new_entries)) = optimize_instructions(
                op_code,
                offset,
                expression,
                bytes,
                b,
                c
            ) {
                optimized_length += new_entries.len();
                optimized_entries.push_back((i, new_entries, remove_count));
                i += remove_count;

            } else {
                optimized_length += 1;
            }

        } else {
            optimized_length += 1;
        }
        i += 1;
    }

    // Perform actual optimizations
    if !optimized_entries.is_empty() {
        let mut i = 0;
        let mut entries_with_optimizations: Vec<SectionEntry> = Vec::with_capacity(optimized_length);
        while i < entries.len() {

            // Check if we optimized any entries at this location
            let is_optimized = if let Some((index, _,_)) = optimized_entries.front() {
                *index == i

            } else {
                false
            };

            if is_optimized {
                // Get the previously optimized entries
                let (_, new_entries, remove_count) = optimized_entries.pop_front().unwrap();

                // Use the initial entry as the basis for the new ones
                let base_entry = &entries[i];

                // Insert new instructions
                for e in new_entries {
                    if let EntryData::Instruction { op_code, expression, bytes, .. } = e {
                        entries_with_optimizations.push(SectionEntry::new_with_size(
                            base_entry.section_id,
                            base_entry.inner.clone(),
                            instruction::size(op_code),
                            EntryData::Instruction {
                                op_code,
                                expression,
                                bytes,
                                volatile: false,
                                debug_only: false
                            }
                        ))

                    } else {
                        unreachable!();
                    }
                }

                i += remove_count;

            } else {
                entries_with_optimizations.push(entries[i].clone());
            }
            i += 1;
        }

        debug_assert_eq!(entries_with_optimizations.len(), optimized_length);

        // We replace the existing entries inline so we don't need to
        // pop from the old vector which would be much slower
        mem::replace(entries, entries_with_optimizations);
        true

    } else {
        false
    }

}

fn optimize_instructions(
    op_code: u16,
    end_of_instruction: i32,
    expression: &OptionalDataExpression,
    bytes: &[u8],
    b: Option<(u16, i32, &OptionalDataExpression, &[u8])>,
    c: Option<(u16, i32, &OptionalDataExpression, &[u8])>

) -> Option<(usize, Vec<EntryData>)> {
    match (op_code, b, c) {
        /*
        // and a,X
        // cp 0
        // jr/jp z/nz
        (0xA0..=0xA7, Some((0xFE, _, _ , cp_bytes)), Some((jo, _, expr, _))) | (0xE6, Some((0xFE, _, _ , cp_bytes)), Some((jo, _, expr, _))) => {
            // Check for jr z,nz or jp z,nz
            if cp_bytes[1] == 0x00 && (jo == 0x28 || jo == 0x20 || jo == 0xC2 || jo == 0xCA) {
                // TODO remove cp 0
                println!("potential and X / cp 0 / jump found {:?}", expr);
                None

            } else {
                None
            }
        },

        // or a,X
        // cp 0
        // jr/jp z/nz
        (0xB0..=0xB7, Some((0xFE, _, _ , cp_bytes)), Some((jo, _, expr, _))) | (0xF6, Some((0xFE, _, _ , cp_bytes)), Some((jo, _, expr, _))) => {
            // Check for jr z,nz or jp z,nz
            if cp_bytes[1] == 0x00 && (jo == 0x28 || jo == 0x20 || jo == 0xC2 || jo == 0xCA) {
                // TODO remove cp 0
                println!("potential or X / cp 0 / jump found {:?}", expr);
                None

            } else {
                None
            }
        },*/

        // ld a,0 -> xor a
        //
        // -> save 1 byte and 3 T-states
        (0x3E, _, _) if bytes[1] == 0x00 => {
            Some((0, vec![EntryData::Instruction {
                op_code: 175,
                expression: None,
                bytes: instruction::bytes(175),
                volatile: false,
                debug_only: false
            }]))
        },

        // cp 0 -> or a
        //
        // save 1 byte and 3 T-states
        (0xFE , _, _) if bytes[1] == 0x00 => {
            Some((0, vec![EntryData::Instruction {
                op_code: 183,
                expression: None,
                bytes: instruction::bytes(183),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld a,[someLabel] -> ldh a,$XX
        (0xFA, _, _) if bytes[2] == 0xFF => {
            Some((0, vec![EntryData::Instruction {
                op_code: 0xF0,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(bytes[1])))),
                bytes: instruction::bytes(0xF0),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld [someLabel],a -> ldh $XX,a
        (0xEA, _, _) if bytes[2] == 0xFF => {
            Some((0, vec![EntryData::Instruction {
                op_code: 0xE0,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(bytes[1])))),
                bytes: instruction::bytes(0xE0),
                volatile: false,
                debug_only: false
            }]))
        },

        // jp c,label  -> jr c,label
        // jp nc,label -> jr nc,label
        // jp z,label  -> jr z,label
        // jp nz,label -> jr nz,label
        (0xDA, _, _) |
        (0xD2, _, _) |
        (0xCA, _, _) |
        (0xC2, _, _) |

        // jp label    -> jr label
        // We only want jp's without that are not followed by a nop to
        // avoid messing with potential jump tables
        (0xC3, None, _) |
        (0xC3, Some((0x01..=512, _, _, _)), _) => {
            let address = i32::from(bytes[1]) | (i32::from(bytes[2]) << 8);
            let relative = address - end_of_instruction;

            // Since the resulting instruction shrinks in size we might now be able
            // to reach the full jump range when the target is at a fixed distance
            // due to a rom section boundray
            if relative > -128 && relative < 127 {
                // Without flags
                if op_code == 0xC3 {
                    Some((0, vec![EntryData::Instruction {
                        op_code: 0x18,
                        expression: expression.clone(),
                        bytes: instruction::bytes(0x18),
                        volatile: false,
                        debug_only: false
                    }]))

                // With flags
                } else {
                    Some((0, vec![EntryData::Instruction {
                        op_code: op_code - 0xA2,
                        expression: expression.clone(),
                        bytes: instruction::bytes(op_code - 0xA2),
                        volatile: false,
                        debug_only: false
                    }]))
                }
            } else {
                None
            }
        },

        // Remove any relative jumps which jump directly behind themselves
        //
        // jr label
        // label:
        (0x18, _, _) => {
            if bytes[1] == 0 {
                Some((0, vec![]))

            } else {
                None
            }
        },

        // call LABEL
        // ret
        // ->
        // jp   LABEL
        //
        // save 1 byte and 17 T-states
        (0xCD, Some((0xC9, _, _, _)), _) => {
            Some((1, vec![EntryData::Instruction {
                op_code: 0xC3,
                expression: expression.clone(),
                bytes: instruction::bytes(0xC3),
                volatile: false,
                debug_only: false
            }]))
        },

        // srl a
        // srl a
        // srl a
        // ->
        // rrca
        // rrca
        // rrca
        // and %00011111
        //
        // save 1 byte and 5 T-states
        (319, Some((319, _, _, _)), Some((319, _, _, _))) => {
            Some((2, vec![
                EntryData::Instruction {
                    op_code: 0x0F,
                    expression: None,
                    bytes: instruction::bytes(0x0F),
                    volatile: false,
                    debug_only: false
                },
                EntryData::Instruction {
                    op_code: 0x0F,
                    expression: None,
                    bytes: instruction::bytes(0x0F),
                    volatile: false,
                    debug_only: false
                },
                EntryData::Instruction {
                    op_code: 0x0F,
                    expression: None,
                    bytes: instruction::bytes(0x0F),
                    volatile: false,
                    debug_only: false
                },
                EntryData::Instruction {
                    op_code: 0xE6,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x1F))),
                    bytes: instruction::bytes(0xE6),
                    volatile: false,
                    debug_only: false
                }
            ]))
        },
        _ => None
    }
}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::super::test::{
        linker_optimize,
        linker_section_entries
    };
    use super::EntryData;
    use crate::lexer::InnerToken;
    use crate::expression::{Expression, ExpressionValue};

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }


    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_no_optimization_across_labels() {
        let l = linker_optimize("SECTION ROM0\ncall global\nfoo:\nret\nld a,a\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 205,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(18, 24, "global"), 2))),
                    bytes: vec![205, 130, 0],
                    volatile: false,
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "foo".to_string()
                }),
                (1, EntryData::Instruction {
                    op_code: 201,
                    expression: None,
                    bytes: vec![201],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 2,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
    }


    #[test]
    fn test_optimize_lda0_to_xora() {
        let l = linker_optimize("SECTION ROM0\nld a,0\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 175,
                    expression: None,
                    bytes: vec![175],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_cp0_to_ora() {
        let l = linker_optimize("SECTION ROM0\ncp 0\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 183,
                    expression: None,
                    bytes: vec![183],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_ldaffxx_to_ldhaxx() {
        let l = linker_optimize("SECTION ROM0\nld a,[$FF05]\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 240,
                    expression: Some(Expression::Value(ExpressionValue::Integer(5))),
                    bytes: vec![240, 5],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_ldffxxa_to_ldhxxa() {
        let l = linker_optimize("SECTION ROM0\nld [$FF05],a\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 224,
                    expression: Some(Expression::Value(ExpressionValue::Integer(5))),
                    bytes: vec![224, 5],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_callret_to_jp() {
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,a\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(18, 24, "global"), 1))),
                    bytes: vec![195, 130, 0],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_callret_to_jp_to_jr() {
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,a\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(18, 24, "global"), 1))),
                    bytes: vec![24, 127],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_jp_to_jr() {
        let l = linker_optimize("SECTION ROM0\njp global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(16, 22, "global"), 1))),
                    bytes: vec![24, 127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nglobal:\nSECTION ROM0[124]\njp global");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ],
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(42, 48, "global"), 1))),
                    bytes: vec![24, 130],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);

        let l = linker_optimize("SECTION ROM0\njp c,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 56,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(18, 24, "global"), 1))),
                    bytes: vec![56, 127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp nc,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 48,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(19, 25, "global"), 1))),
                    bytes: vec![48, 127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp z,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 40,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(18, 24, "global"), 1))),
                    bytes: vec![40, 127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp nz,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 32,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(19, 25, "global"), 1))),
                    bytes: vec![32, 127],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_no_optimization_jp_when_followed_by_nop() {
        let l = linker_optimize("SECTION ROM0\njp global\nnop\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(16, 22, "global"), 1))),
                    bytes: vec![195, 129, 0],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 0,
                    expression: None,
                    bytes: vec![0],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_no_optimization_jp_when_out_of_range() {
        let l = linker_optimize("SECTION ROM0\njp global\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(16, 22, "global"), 1))),
                    bytes: vec![195, 130, 0],
                    volatile: false,
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nglobal:\nSECTION ROM0[125]\njp global");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global".to_string()
                })
            ],
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(42, 48, "global"), 1))),
                    bytes: vec![195, 0, 0],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_srla() {
        let l = linker_optimize("SECTION ROM0\nsrl a\nsrl a\nsrl a\nsrl a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 15,
                    expression: None,
                    bytes: vec![15],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 15,
                    expression: None,
                    bytes: vec![15],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 15,
                    expression: None,
                    bytes: vec![15],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 230,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x1F))),
                    bytes: vec![230, 31],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 319,
                    expression: None,
                    bytes: vec![203, 63],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_relative_jump_with_0_offer() {
        let l = linker_optimize("SECTION ROM0\njr foo\nfoo:\njp bar\nbar:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "foo".to_string()
                }),
                (0, EntryData::Label {
                    id: 2,
                    is_local: false,
                    name: "bar".to_string()
                })
            ]
        ]);
    }

    // Volatile Blocks --------------------------------------------------------
    #[test]
    fn test_volatile_block_no_optimize() {
        let l = linker_optimize("SECTION ROM0\nBLOCK VOLATILE\nld a,0\ncp 0\nENDBLOCK");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 62,
                    expression: None,
                    bytes: vec![62, 0],
                    volatile: true,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 254,
                    expression: None,
                    bytes: vec![254, 0],
                    volatile: true,
                    debug_only: false
                })
            ]
        ]);
    }

}

