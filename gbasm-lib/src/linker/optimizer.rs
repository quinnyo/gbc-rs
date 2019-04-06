// Internal Dependencies ------------------------------------------------------
use crate::expression::{OptionalDataExpression, Expression, ExpressionValue, TEMPORARY_EXPRESSION_ID};
use super::section::entry::{EntryData, SectionEntry};
use super::util::instruction;


// Low Level Instruction Optimizer --------------------------------------------
pub fn optimize_section_entries(entries: &mut Vec<SectionEntry>) -> bool {

    fn get_instruction(entries: &[SectionEntry], i: usize) -> Option<(usize, i32, &OptionalDataExpression, &[u8])> {
        if let Some(entry) = entries.get(i) {
            if let EntryData::Instruction { ref op_code, ref bytes, ref expression, .. } = entry.data {
                Some((*op_code, (entry.offset + entry.size) as i32, &expression, bytes))

            } else {
                None
            }

        } else {
            None
        }
    }

    let mut i = 0;
    let mut any_optimizations = false;
    while i < entries.len() {
        if let Some((op_code, offset, expression, bytes)) = get_instruction(&entries, i) {
            let b = get_instruction(&entries, i + 1);
            let c = get_instruction(&entries, i + 2);
            if let Some((remove_count, EntryData::Instruction { op_code, expression, bytes, .. })) = optimize_instructions(
                op_code,
                offset,
                expression,
                bytes,
                b,
                c
            ) {
                // Remove additional instruction
                for _ in 0..remove_count {
                    entries.remove(i + 1);
                }

                // Modify the current instruction
                let entry = &mut entries[i];
                entry.size = instruction::size(op_code);
                entry.data = EntryData::Instruction {
                    op_code,
                    expression,
                    bytes,
                    debug_only: false
                };
                any_optimizations = true;
            }
        }
        i += 1;
    }
    any_optimizations
}

fn optimize_instructions(
    op_code: usize,
    end_of_instruction: i32,
    expression: &OptionalDataExpression,
    bytes: &[u8],
    b: Option<(usize, i32, &OptionalDataExpression, &[u8])>,
    c: Option<(usize, i32, &OptionalDataExpression, &[u8])>

) -> Option<(usize, EntryData)> {
    match (op_code, b, c) {
        // ld a,0 -> xor a
        //
        // -> save 1 byte and 3 T-states
        (0x3E, _, _) if bytes[1] == 0x00 => {
            Some((0, EntryData::Instruction {
                op_code: 175,
                expression: None,
                bytes: instruction::bytes(175),
                debug_only: false
            }))
        },

        // cp 0 -> or a
        //
        // save 1 byte and 3 T-states
        (0xFE , _, _) if bytes[1] == 0x00 => {
            Some((0, EntryData::Instruction {
                op_code: 183,
                expression: None,
                bytes: instruction::bytes(183),
                debug_only: false
            }))
        },

        // ld a,[someLabel] -> ldh a,$XX
        (0xFA, _, _) if bytes[2] == 0xFF => {
            Some((0, EntryData::Instruction {
                op_code: 0xF0,
                expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(bytes[1] as i32)))),
                bytes: instruction::bytes(0xF0),
                debug_only: false
            }))
        },

        // ld [someLabel],a -> ldh $XX,a
        (0xEA, _, _) if bytes[2] == 0xFF => {
            Some((0, EntryData::Instruction {
                op_code: 0xE0,
                expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(bytes[1] as i32)))),
                bytes: instruction::bytes(0xE0),
                debug_only: false
            }))
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
        // We only want jp's without that are not followed by a nop
        (0xC3, None, _) |
        (0xC3, Some((0x01..=512, _, _, _)), _) => {
            let address = bytes[1] as i32 | ((bytes[2] as i32) << 8);
            let relative = address - end_of_instruction;

            // Since the resulting instruction shrinks in size we might now be able
            // to reach the full jump range when the target is at a fixed distance
            // due to a rom section boundray
            if relative > -128 && relative < 127 {
                // Without flags
                if op_code == 0xC3 {
                    Some((0, EntryData::Instruction {
                        op_code: 0x18,
                        expression: expression.clone(),
                        bytes: instruction::bytes(0x18),
                        debug_only: false
                    }))

                // With flags
                } else {
                    Some((0, EntryData::Instruction {
                        op_code: op_code - 0xA2,
                        expression: expression.clone(),
                        bytes: instruction::bytes(op_code - 0xA2),
                        debug_only: false
                    }))
                }
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
            Some((1, EntryData::Instruction {
                op_code: 0xC3,
                expression: expression.clone(),
                bytes: instruction::bytes(0xC3),
                debug_only: false
            }))
        },
        // TODO combine adjancent b/c c/b d/e e/d h/l l/h loads into a single ld bc etc.
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
    use crate::expression::{Expression, ExpressionValue, TEMPORARY_EXPRESSION_ID};

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }


    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_section_no_optimization_across_labels() {
        let l = linker_optimize("SECTION ROM0\ncall global\nfoo:\nret\nld a,a\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 205,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(18, 24, "global"), 2)))),
                    bytes: vec![205, 130, 0],
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 1,
                    name: "foo".to_string()
                }),
                (1, EntryData::Instruction {
                    op_code: 201,
                    expression: None,
                    bytes: vec![201],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 2,
                    name: "global".to_string()
                })
            ]
        ]);
    }


    #[test]
    fn test_section_optimize_lda0_to_xora() {
        let l = linker_optimize("SECTION ROM0\nld a,0\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 175,
                    expression: None,
                    bytes: vec![175],
                    debug_only: false
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
    fn test_section_optimize_cp0_to_ora() {
        let l = linker_optimize("SECTION ROM0\ncp 0\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 183,
                    expression: None,
                    bytes: vec![183],
                    debug_only: false
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
    fn test_section_optimize_ldaffxx_to_ldhaxx() {
        let l = linker_optimize("SECTION ROM0\nld a,[$FF05]\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 240,
                    expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(5)))),
                    bytes: vec![240, 5],
                    debug_only: false
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
    fn test_section_optimize_ldffxxa_to_ldhxxa() {
        let l = linker_optimize("SECTION ROM0\nld [$FF05],a\nld a,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 224,
                    expression: Some((TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(5)))),
                    bytes: vec![224, 5],
                    debug_only: false
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
    fn test_section_optimize_callret_to_jp() {
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,a\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(18, 24, "global"), 1)))),
                    bytes: vec![195, 130, 0],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_callret_to_jp_to_jr() {
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,a\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(18, 24, "global"), 1)))),
                    bytes: vec![24, 127],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_optimize_jp_to_jr() {
        let l = linker_optimize("SECTION ROM0\njp global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(16, 22, "global"), 1)))),
                    bytes: vec![24, 127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nglobal:\nSECTION ROM0[124]\njp global");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ],
            vec![
                (2, EntryData::Instruction {
                    op_code: 24,
                    expression: Some((1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(42, 48, "global"), 1)))),
                    bytes: vec![24, 130],
                    debug_only: false
                })
            ]
        ]);

        let l = linker_optimize("SECTION ROM0\njp c,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 56,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(18, 24, "global"), 1)))),
                    bytes: vec![56, 127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp nc,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 48,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(19, 25, "global"), 1)))),
                    bytes: vec![48, 127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp z,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 40,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(18, 24, "global"), 1)))),
                    bytes: vec![40, 127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\njp nz,global\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 32,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(19, 25, "global"), 1)))),
                    bytes: vec![32, 127],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_no_optimization_jp_when_followed_by_nop() {
        let l = linker_optimize("SECTION ROM0\njp global\nnop\nSECTION ROM0[129]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(16, 22, "global"), 1)))),
                    bytes: vec![195, 129, 0],
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 0,
                    expression: None,
                    bytes: vec![0],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
    }

    #[test]
    fn test_section_no_optimization_jp_when_out_of_range() {
        let l = linker_optimize("SECTION ROM0\njp global\nSECTION ROM0[130]\nglobal:");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((0, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(16, 22, "global"), 1)))),
                    bytes: vec![195, 130, 0],
                    debug_only: false
                })
            ],
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nglobal:\nSECTION ROM0[125]\njp global");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    name: "global".to_string()
                })
            ],
            vec![
                (3, EntryData::Instruction {
                    op_code: 195,
                    expression: Some((1, Expression::Value(ExpressionValue::GlobalLabelAddress(itk!(42, 48, "global"), 1)))),
                    bytes: vec![195, 0, 0],
                    debug_only: false
                })
            ]
        ]);
    }

}

