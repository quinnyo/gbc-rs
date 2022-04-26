// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::collections::VecDeque;


// External Dependencies ------------------------------------------------------
use gb_cpu::Instruction;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;
use super::section::entry::{EntryData, SectionEntry};
use crate::expression::{OptionalDataExpression, Expression, ExpressionValue};
use super::util::instruction;


// Types ----------------------------------------------------------------------
pub type OptimizerInfo = (InnerToken, String);

struct OptEntry<'a> {
    op_code: u16,
    end_of_instruction: i32,
    expression: &'a OptionalDataExpression,
    bytes: &'a [u8],
    inner: &'a InnerToken,
}


// Low Level Instruction Optimizer --------------------------------------------
pub fn optimize_section_entries(entries: &mut Vec<SectionEntry>, notes: &mut Vec<OptimizerInfo>, strip_debug: bool) -> bool {

    // Do a dry run to figure out of there is anything to optimize at all
    // But keep the optimized entries so we don't have to generate them twice
    let mut i = 0;
    let mut optimized_entries = VecDeque::with_capacity(128);
    let mut optimized_length = 0;
    while i < entries.len() {
        if let Some(a) = get_instruction(entries, i) {
            let b = get_instruction(entries, i + 1);
            let c = get_instruction(entries, i + 2);
            if let Some((remove_count, new_entries)) = optimize_instructions(
                notes,
                a,
                b,
                c,
                strip_debug
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
    let optimized = if !optimized_entries.is_empty() {
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
                    if let EntryData::Instruction { op_code, expression, bytes, volatile, debug_only } = e {
                        entries_with_optimizations.push(SectionEntry::new_with_size(
                            base_entry.section_id,
                            base_entry.inner.clone(),
                            instruction::size(op_code),
                            EntryData::Instruction {
                                op_code,
                                expression,
                                bytes,
                                volatile,
                                debug_only
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
        let _ = mem::replace(entries, entries_with_optimizations);
        true

    } else {
        false
    };
    optimized_accumulator_loads(entries, notes) || optimized
}

fn optimized_accumulator_loads(entries: &mut Vec<SectionEntry>, notes: &mut Vec<OptimizerInfo>) -> bool {
    let mut i = 0;
    let mut optimized = false;
    while i < entries.len() {
        // Search for initial Accumulator immediate load
        if let Some(a @ OptEntry { op_code: 0x3E | 0xAF, .. }) = get_instruction(entries, i) {
            let mut prev_value = a.bytes.get(1).cloned().unwrap_or(0);
            i += 1;
            loop {
                // Skip over any instructions that cannot modify the Accumulator
                while let Some(b) = get_instruction(entries, i) {
                    if Instruction::does_not_modify_accumulator(b.op_code) {
                        i += 1;

                    } else {
                        break;
                    }
                }

                // Check if there is a chained load
                if let Some(c @ OptEntry { op_code: 0x3E | 0xAF, .. }) = get_instruction(entries, i) {
                    // Check if we can replace the loaded value with a faster / short alternative
                    let next_value = c.bytes.get(1).cloned().unwrap_or(0);
                    if next_value == prev_value {
                        notes.push((c.inner.clone(), "removed".to_string()));
                        entries.remove(i);
                        optimized = true;

                    } else if next_value == prev_value.wrapping_add(1) {
                        notes.push((c.inner.clone(), "replaced".to_string()));
                        let e = entries.get_mut(i).expect("missing entry");
                        e.data = EntryData::Instruction {
                            op_code: 0x3C,
                            expression: None,
                            bytes: instruction::bytes(0x3C),
                            volatile: false,
                            debug_only: false
                        };
                        e.size = 1;
                        optimized = true;
                        i += 1;

                    } else if next_value == prev_value.wrapping_sub(1) {
                        notes.push((c.inner.clone(), "replaced".to_string()));
                        let e = entries.get_mut(i).expect("missing entry");
                        e.data = EntryData::Instruction {
                            op_code: 0x3D,
                            expression: None,
                            bytes: instruction::bytes(0x3D),
                            volatile: false,
                            debug_only: false
                        };
                        e.size = 1;
                        optimized = true;
                        i += 1;

                    // TODO handle multiply by 2 via `add a` instruction

                    } else {
                        i += 1;
                        break;
                    }
                    prev_value = next_value;

                } else {
                    break;
                }
            }

        } else if let Some(a @ OptEntry { op_code: 0xF0 | 0xFA, .. }) = get_instruction(entries, i) {
            let mut prev_addr = if a.bytes.len() == 3 {
                ((a.bytes[2] as u16) << 8) | (a.bytes[1] as u16)

            } else {
                0xFF00 | a.bytes[1] as u16
            };
            i += 1;
            loop {
                // Skip over any instructions that cannot modify the Accumulator
                while let Some(b) = get_instruction(entries, i) {
                    if Instruction::does_not_modify_accumulator(b.op_code) {
                        i += 1;

                    } else {
                        break;
                    }
                }

                // Check if there is a chained load
                if let Some(c @ OptEntry { op_code: 0xF0 | 0xFA, .. }) = get_instruction(entries, i) {
                    let next_addr = if c.bytes.len() == 3 {
                        ((c.bytes[2] as u16) << 8) | (c.bytes[1] as u16)

                    } else {
                        0xFF00 | c.bytes[1] as u16
                    };

                    // Need to avoid messing with IO (0xFF00..0xFF80) polling
                    if next_addr == prev_addr && !is_volatile_address(prev_addr) {
                        notes.push((c.inner.clone(), "removed".to_string()));
                        entries.remove(i);
                        optimized = true;

                    } else {
                        i += 1;
                        break;
                    }
                    prev_addr = next_addr;

                } else {
                    break;
                }
            }

        } else {
            i += 1;
        }
    }
    optimized
}

// TODO keep track of how many bytes / cycles got optimized
fn optimize_instructions(
    notes: &mut Vec<OptimizerInfo>,
    a: OptEntry,
    b: Option<OptEntry>,
    c: Option<OptEntry>,
    strip_debug: bool

) -> Option<(usize, Vec<EntryData>)> {
    match (a.op_code, b, c) {
        // ld a,a
        // ld c,c
        // ld d,d
        // ld e,e
        // ld h,h
        // ld l,l
        //
        // save 1 byte 4 cycles
        (0x7F, _, _) | (0x49, _, _) | (0x52, _, _) | (0x5B, _, _) | (0x64, _, _)  | (0x6D, _, _) => {
            notes.push((a.inner.clone(), "removed".to_string()));
            Some((0, vec![]))
        },

        // ld b,b
        //
        // save 1 byte 4 cycles
        (0x40, _, _) if strip_debug => {
            notes.push((a.inner.clone(), "stripped".to_string()));
            Some((0, vec![]))
        },

        // ld a,d8
        // ld [hl],a
        // ->
        // ld [hl],d8
        //
        // save 1 byte 4 cycles
        (0x3E, Some(OptEntry { op_code: 0x77 , .. }), _) => {
            Some((1, vec![EntryData::Instruction {
                op_code: 0x36,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(a.bytes[1])))),
                bytes: a.bytes.to_vec(),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld b,X
        // ld c,Y
        // ->
        // ld bc,XY
        //
        // save 1 byte and 4 cycles
        (0x06, Some(b @ OptEntry { op_code: 0x0E, .. }), _) => {
            let value = ((a.bytes[1] as u16) << 8) | (b.bytes[1] as u16);
            Some((1, vec![EntryData::Instruction {
                op_code: 0x01,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(value)))),
                bytes: instruction::bytes(0x01),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld d,X
        // ld e,Y
        // ->
        // ld de,XY
        //
        // save 1 byte and 4 cycles
        (0x16, Some(b @ OptEntry { op_code: 0x1E, .. }), _) => {
            let value = ((a.bytes[1] as u16) << 8) | (b.bytes[1] as u16);
            Some((1, vec![EntryData::Instruction {
                op_code: 0x11,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(value)))),
                bytes: instruction::bytes(0x11),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld h,X
        // ld l,Y
        // ->
        // ld hl,XY
        //
        // save 1 byte and 4 cycles
        (0x26, Some(b @ OptEntry { op_code: 0x2E, .. }), _) => {
            let value = ((a.bytes[1] as u16) << 8) | (b.bytes[1] as u16);
            Some((1, vec![EntryData::Instruction {
                op_code: 0x21,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(value)))),
                bytes: instruction::bytes(0x21),
                volatile: false,
                debug_only: false
            }]))
        },

        /*
        // and a,X
        // cp 0
        // jr/jp z/nz
        (0xA0..=0xA7, Some((0xFE, _, _ , cp_bytes, _)), Some((jo, _, expr, _, _))) | (0xE6, Some((0xFE, _, _ , cp_bytes, _)), Some((jo, _, expr, _, _))) => {
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
        (0xB0..=0xB7, Some((0xFE, _, _ , cp_bytes, _)), Some((jo, _, expr, _, _))) | (0xF6, Some((0xFE, _, _ , cp_bytes, _)), Some((jo, _, expr, _, _))) => {
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
        // -> save 1 byte and 4 cycles
        (0x3E, _, _) if a.bytes[1] == 0x00 => {
            notes.push((a.inner.clone(), "xor a".to_string()));
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
        // save 1 byte and 4 cycles
        (0xFE , _, _) if a.bytes[1] == 0x00 => {
            notes.push((a.inner.clone(), "or a".to_string()));
            Some((0, vec![EntryData::Instruction {
                op_code: 183,
                expression: None,
                bytes: instruction::bytes(183),
                volatile: false,
                debug_only: false
            }]))
        },

        // ld a,[someLabel] -> ldh a,$XX
        //
        // save 1 byte and 4 cycles
        (0xFA, _, _) if a.bytes[2] == 0xFF => {
            Some((0, vec![EntryData::Instruction {
                op_code: 0xF0,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(a.bytes[1])))),
                bytes: vec![0xF0, a.bytes[1]],
                volatile: false,
                debug_only: false
            }]))
        },

        // ld [someLabel],a -> ldh $XX,a
        //
        // save 1 byte and 4 cycles
        (0xEA, _, _) if a.bytes[2] == 0xFF => {
            Some((0, vec![EntryData::Instruction {
                op_code: 0xE0,
                expression: Some(Expression::Value(ExpressionValue::Integer(i32::from(a.bytes[1])))),
                bytes: vec![0xE0, a.bytes[1]],
                volatile: false,
                debug_only: false
            }]))
        },

        // ld [someLabel],a
        // ld a,[someLabel]
        //
        // save 6 byte and 32 cycles
        (0xEA, Some(b @ OptEntry { op_code: 0xFA, .. }), _) => {
            let addr = ((a.bytes[2] as u16) << 8) | (a.bytes[1] as u16);
            if a.bytes[1] == b.bytes[1] && a.bytes[2] == b.bytes[2] && !is_volatile_address(addr) {
                notes.push((a.inner.clone(), "removed".to_string()));
                notes.push((b.inner.clone(), "removed".to_string()));
                Some((1, vec![]))

            } else {
                None
            }
        },

        // ldh [someLabel],a
        // ldh a,[someLabel]
        //
        // save 4 byte and 24 cycles
        (0xE0, Some(b @ OptEntry { op_code: 0xF0, .. }), _) => {
            if a.bytes[1] == b.bytes[1] && !is_volatile_address(0xFF00 | a.bytes[1] as u16) {
                notes.push((a.inner.clone(), "removed".to_string()));
                notes.push((b.inner.clone(), "removed".to_string()));
                Some((1, vec![]))

            } else {
                None
            }
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
        // We only want jp's that are not followed by a nop to
        // avoid messing with a potential jump table
        (0xC3, None, _) |
        (0xC3, Some(OptEntry { op_code: 0x01..=512, .. }), _) => {
            let address = i32::from(a.bytes[1]) | (i32::from(a.bytes[2]) << 8);
            let relative = address - a.end_of_instruction;

            // Since the resulting instruction shrinks in size we might now be able
            // to reach the full jump range when the target is at a fixed distance
            // due to a rom section boundary
            if relative > -128 && relative < 127 {
                notes.push((a.inner.clone(), "jr".to_string()));

                // Without flags
                if a.op_code == 0xC3 {
                    Some((0, vec![EntryData::Instruction {
                        op_code: 0x18,
                        expression: a.expression.clone(),
                        bytes: instruction::bytes(0x18),
                        volatile: false,
                        debug_only: false
                    }]))

                // With flags
                } else {
                    Some((0, vec![EntryData::Instruction {
                        op_code: a.op_code - 0xA2,
                        expression: a.expression.clone(),
                        bytes: instruction::bytes(a.op_code - 0xA2),
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
            if a.bytes[1] == 0 {
                notes.push((a.inner.clone(), "removed".to_string()));
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
        // save 1 byte and 24 cycles
        (0xCD, Some(b @ OptEntry { op_code: 0xC9, .. }), _) => {
            notes.push((a.inner.clone(), "jp".to_string()));
            notes.push((b.inner.clone(), "removed".to_string()));
            Some((1, vec![EntryData::Instruction {
                op_code: 0xC3,
                expression: a.expression.clone(),
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
        // and %0001_1111
        //
        // save 1 byte and 4 cycles
        (0x13F, Some(OptEntry { op_code: 0x13F, .. }), Some(OptEntry { op_code: 0x13F, .. })) => {
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

fn is_volatile_address(addr: u16) -> bool {
    // IO registers and mapper registers
    matches!(addr, 0xFF00..=0xFF7F | 0x0000..=0x7FFF)
}

fn get_instruction(entries: &[SectionEntry], i: usize) -> Option<OptEntry> {
    if let Some(entry) = entries.get(i) {
        if let EntryData::Instruction { ref op_code, ref bytes, ref expression, volatile, .. } = entry.data {
            if volatile {
                None

            } else {
                Some(OptEntry {
                    op_code: *op_code,
                    end_of_instruction: (entry.offset + entry.size) as i32,
                    expression,
                    bytes,
                    inner: &entry.inner
                })
            }

        } else {
            None
        }

    } else {
        None
    }
}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::super::test::{
        linker_optimize,
        linker_optimize_keep_debug,
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
        let l = linker_optimize("SECTION ROM0\ncall global\nfoo:\nret\nld a,b\nSECTION ROM0[130]\nglobal:");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
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
    fn test_optimize_lda_d8_ldhl_a_to_ldhl_d8() {
        let l = linker_optimize("SECTION ROM0\nld a,5\nld [hl],a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 54,
                    expression: Some(Expression::Value(ExpressionValue::Integer(5))),
                    bytes: vec![54, 5],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_lda0_to_xora() {
        let l = linker_optimize("SECTION ROM0\nld a,0\nld a,b");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_cp0_to_ora() {
        let l = linker_optimize("SECTION ROM0\ncp 0\nld a,b");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_ldaffxx_to_ldhaxx() {
        let l = linker_optimize("SECTION ROM0\nld a,[$FF05]\nld a,b");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_ldffxxa_to_ldhxxa() {
        let l = linker_optimize("SECTION ROM0\nld [$FF05],a\nld a,b");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_optimize_callret_to_jp() {
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,b\nSECTION ROM0[130]\nglobal:");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
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
        let l = linker_optimize("SECTION ROM0\ncall global\nret\nld a,b\nSECTION ROM0[129]\nglobal:");
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
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
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

    // Redundant address store/load -------------------------------------------
    #[test]
    fn test_remove_redundant_address_store_load() {
        let l = linker_optimize("SECTION ROM0\nld [$8000],a\n ld a,[$8000]");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker_optimize("SECTION ROM0\nld [$FF00],a\n ld a,[$FF00]");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 224,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0))),
                    bytes: vec![224, 0],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 240,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0))),
                    bytes: vec![240, 0],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    // Combine individual loads -----------------------------------------------
    #[test]
    fn test_combine_ld_bc_de_hl() {
        let l = linker_optimize("SECTION ROM0\nld b,$80\nld c,$40");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 0x01,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x8040))),
                    bytes: vec![0x01, 64, 128],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nld d,$80\nld e,$40");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 0x11,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x8040))),
                    bytes: vec![0x11, 64, 128],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nld h,$80\nld l,$40");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 0x21,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x8040))),
                    bytes: vec![0x21, 64, 128],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    // LD X,X -----------------------------------------------------------------
    #[test]
    fn test_remove_register_self_loads() {
        let l = linker_optimize("SECTION ROM0\nld a,a\nld b,b\nld c,c\nld d,d\nld e,e\nld h,h\nld l,l");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker_optimize_keep_debug("SECTION ROM0\nld a,a\nld b,b\nld c,c\nld d,d\nld e,e\nld h,h\nld l,l");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 64,
                    expression: None,
                    bytes: vec![64],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    // jc a > b ---------------------------------------------------------------
    #[test]
    fn test_correct_handling_jc_jump_over_jp() {
        let l = linker_optimize("SECTION ROM0\nglobal_label:\njc a > b,global_label");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "global_label".to_string()
                }),
                (1, EntryData::Instruction {
                    op_code: 0xB8,
                    expression: None,
                    bytes: vec![0xB8],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 0x28,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(27, 29, "jc"), 512_000_001))),
                    bytes: vec![0x28, 2],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 0x30,
                    expression: Some(Expression::Value(ExpressionValue::ParentLabelAddress(itk!(36, 48, "global_label"), 1))),
                    bytes: vec![0x30, 251],
                    volatile: false,
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 512_000_001,
                    is_local: false,
                    name: "jc".to_string()
                })
            ]
        ]);
    }

    // Redundant Accumulator Loads --------------------------------------------
    #[test]
    fn test_remove_redundant_acummulator_immediate_load() {
        let l = linker_optimize("SECTION ROM0\nld a,$FF\nld b,a\nld a,$FF\nld c,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 0x3E,
                    expression: None,
                    bytes: vec![0x3E, 0xFF],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 71,
                    expression: None,
                    bytes: vec![71],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 79,
                    expression: None,
                    bytes: vec![79],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nld a,$FF\nld [$4000],a\nld a,$FF\nld [$8000],a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 0x3E,
                    expression: None,
                    bytes: vec![0x3E, 0xFF],
                    volatile: false,
                    debug_only: false
                }),
                (3, EntryData::Instruction {
                    op_code: 0xEA,
                    expression: None,
                    bytes: vec![0xEA, 0, 0x40],
                    volatile: false,
                    debug_only: false
                }),
                (3, EntryData::Instruction {
                    op_code: 0xEA,
                    expression: None,
                    bytes: vec![0xEA, 0, 0x80],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nxor a\ninc hl\nld a,1\nld b,a\nld h,b\nld a,2\ninc sp\nld a,3\nld d,a\nld a,5");
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
                    op_code: 35,
                    expression: None,
                    bytes: vec![35],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 60,
                    expression: None,
                    bytes: vec![60],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 71,
                    expression: None,
                    bytes: vec![71],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 96,
                    expression: None,
                    bytes: vec![96],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 60,
                    expression: None,
                    bytes: vec![60],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 51,
                    expression: None,
                    bytes: vec![51],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 60,
                    expression: None,
                    bytes: vec![60],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 87,
                    expression: None,
                    bytes: vec![87],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 62,
                    expression: None,
                    bytes: vec![62, 5],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_remove_redundant_acummulator_address_load() {
        let l = linker_optimize("SECTION ROM0\nld a,[$CD00]\nld b,a\nld a,[$CD00]\nld c,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 0xFA,
                    expression: None,
                    bytes: vec![0xFA, 0, 205],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 71,
                    expression: None,
                    bytes: vec![71],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 79,
                    expression: None,
                    bytes: vec![79],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nlabel:\nld a,[$CD00]\nld b,a\n.child:\nld a,[$CD00]\nld c,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (0, EntryData::Label {
                    id: 1,
                    is_local: false,
                    name: "label".to_string()
                }),
                (3, EntryData::Instruction {
                    op_code: 0xFA,
                    expression: None,
                    bytes: vec![0xFA, 0, 205],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 71,
                    expression: None,
                    bytes: vec![71],
                    volatile: false,
                    debug_only: false
                }),
                (0, EntryData::Label {
                    id: 2,
                    is_local: true,
                    name: "child".to_string()
                }),
                (3, EntryData::Instruction {
                    op_code: 0xFA,
                    expression: None,
                    bytes: vec![0xFA, 0, 205],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 79,
                    expression: None,
                    bytes: vec![79],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nld a,[$CD00]\nld a,b\nld a,[$CD00]\nld c,a");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (3, EntryData::Instruction {
                    op_code: 0xFA,
                    expression: None,
                    bytes: vec![0xFA, 0, 205],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 120,
                    expression: None,
                    bytes: vec![120],
                    volatile: false,
                    debug_only: false
                }),
                (3, EntryData::Instruction {
                    op_code: 0xFA,
                    expression: None,
                    bytes: vec![0xFA, 0, 205],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 79,
                    expression: None,
                    bytes: vec![79],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
        let l = linker_optimize("SECTION ROM0\nld a,[$FF41]\nld b,a\nld a,[$FF41]");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Instruction {
                    op_code: 0xF0,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x41))),
                    bytes: vec![0xF0, 0x41],
                    volatile: false,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 71,
                    expression: None,
                    bytes: vec![71],
                    volatile: false,
                    debug_only: false
                }),
                (2, EntryData::Instruction {
                    op_code: 0xF0,
                    expression: Some(Expression::Value(ExpressionValue::Integer(0x41))),
                    bytes: vec![0xF0, 0x41],
                    volatile: false,
                    debug_only: false
                })
            ]
        ]);
    }
}

