// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::fmt;
use std::iter;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;
use gbasm_cpu::{self, Argument, Instruction};


// Modules --------------------------------------------------------------------
pub mod entry;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use crate::lexer::{InnerToken, EntryToken};
use crate::expression::ExpressionResult;
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};
use crate::expression::evaluator::EvaluatorContext;
use self::entry::{EntryData, SectionEntry};
use super::util::{self, instruction};


// Statics --------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
struct SectionDefault {
    base_address: usize,
    size: usize,
    is_rom: bool,
    min_bank: Option<usize>,
    max_bank: Option<usize>
}

lazy_static! {
    static ref INSTRUCTIONS: Vec<Instruction> = gbasm_cpu::instruction_list();

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
            is_rom: false,
            min_bank: None,
            max_bank: None
        });
        map.insert("WRAMX", SectionDefault {
            base_address: 0xD000,
            size: 0x1000,
            is_rom: false,
            min_bank: Some(1),
            max_bank: Some(1)
        });
        map.insert("HRAM", SectionDefault {
            base_address: 0xFF80,
            size: 0x80,
            is_rom: false,
            min_bank: None,
            max_bank: None
        });
        map.insert("RAM", SectionDefault {
            base_address: 0xA000,
            size: 0x2000,
            is_rom: false,
            min_bank: None,
            max_bank: None
        });
        map.insert("RAMX", SectionDefault {
            base_address: 0xA000,
            size: 0x2000,
            is_rom: false,
            min_bank: Some(0),
            max_bank: Some(7)
        });
        map
    };
}

#[derive(Debug, Eq, PartialEq)]
pub struct Section {
    pub id: usize,
    name: String,
    pub segment: String,
    inner: InnerToken,

    pub start_address: usize,
    pub end_address: usize,
    pub is_rom: bool,
    pub bank_offset: usize,
    pub bank: usize,

    pub entries: Vec<SectionEntry>,
    bytes_in_use: usize
}

impl fmt::Display for Section {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{: >2}][{: >16}] {: >5}[{:0>4x}-{:0>4x} +{:0>4x}][{}]", self.id, self.name, self.segment, self.start_address, self.end_address, self.bank_offset, self.bank)
    }
}

impl Section {

    pub fn default_hash(name: &str, bank_index: Option<usize>) -> (&str, usize) {
        let defaults = SECTION_DEFAULTS.get(name).expect("Invalid segment name");
        (name, bank_index.or(defaults.min_bank).unwrap_or(0))
    }

    pub fn new(
        id: usize,
        segment: String,
        name: Option<String>,
        inner: InnerToken,
        segment_offset: Option<usize>,
        segment_size: Option<usize>,
        segment_bank: Option<usize>

    ) -> Result<Self, SourceError> {

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

        // Calculate section start and bank offset
        let (start, offset, default_end) = (
            defaults.base_address,
            segment_offset.unwrap_or(defaults.base_address),
            defaults.base_address + defaults.size
        );

        let (start_address, bank_offset) = if bank == 0 {
            (offset, 0)

        } else {
            (offset, (bank - defaults.min_bank.unwrap_or(0)) * defaults.size)
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

        debug_assert!(start_address < end_address);

        Ok(Self {
            id,
            name: name.unwrap_or_else(|| "".to_string()),
            segment,
            inner,

            start_address,
            end_address: end_address - 1,
            is_rom: defaults.is_rom,
            bank_offset,
            bank,

            entries: Vec::with_capacity(256),
            bytes_in_use: 0
        })

    }

    pub fn hash(&self) -> (&str, usize) {
        (&self.segment, self.bank)
    }

    pub fn add_entry(&mut self, context: &mut EvaluatorContext, token: EntryToken) -> Result<(), SourceError> {
        match token {
            EntryToken::Instruction(inner, op_code) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction::size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: None,
                        bytes: instruction::bytes(op_code),
                        debug_only: false
                    }
                ))
            },
            EntryToken::InstructionWithArg(inner, op_code, expr) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction::size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: Some(expr),
                        bytes: instruction::bytes(op_code),
                        debug_only: false
                    }
                ))
            },
            EntryToken::DebugInstruction(inner, op_code) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction::size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: None,
                        bytes: instruction::bytes(op_code),
                        debug_only: true
                    }
                ))
            },
            EntryToken::DebugInstructionWithArg(inner, op_code, expr) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction::size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: Some(expr),
                        bytes: instruction::bytes(op_code),
                        debug_only: true
                    }
                ))
            },
            EntryToken::GlobalLabelDef(inner, id) | EntryToken::LocalLabelDef(inner, id) => {
                let name = inner.value.clone();
                self.entries.push(SectionEntry::new_unsized(self.id, inner, EntryData::Label {
                    name,
                    id
                }));
            },
            EntryToken::Data { inner, endianess, storage, alignment, debug_only } => {

                // Storage only consists of const expressions and can be evaluated here
                let (size, bytes, expressions) = match storage {
                    // RAM Only
                    DataStorage::Byte => {
                        self.check_ram(&inner, "Byte Variable")?;
                        (1, None, None)
                    },
                    DataStorage::Word => {
                        self.check_ram(&inner, "Word Variable")?;
                        (2, None, None)
                    },

                    // ROM Only
                    DataStorage::Array(bytes) => {
                        self.check_rom(&inner, "Data Include")?;
                        (bytes.len(), Some(bytes), None)
                    },
                    DataStorage::Bytes(exprs) => {
                        self.check_rom(&inner, "Data Declaration")?;
                        (exprs.len(), None, Some(exprs.into_iter().map(|e| (1, e)).collect()))
                    },
                    DataStorage::Words(exprs) => {
                        self.check_rom(&inner, "Data Declaration")?;
                        (exprs.len() * 2, None, Some(exprs.into_iter().map(|e| (2, e)).collect()))
                    },
                    DataStorage::Buffer(length, fill) => {
                        let length_or_string = context.resolve_expression(length)?;
                        let fill = context.resolve_optional_expression(fill)?;
                        match (length_or_string, fill) {
                            // DS 15 "FOO"
                            (ExpressionResult::Integer(size), Some(ExpressionResult::String(s))) => {
                                self.check_rom(&inner, "Data Declaration")?;
                                let size = util::positive_integer(&inner, size, "Invalid storage capacity")?;
                                let mut bytes = s.into_bytes();
                                if bytes.len() > size {
                                    return Err(inner.error("Invalid storage capacity, specified capacity must be >= length of stored string data.".to_string()));
                                }
                                let mut padding = iter::repeat(0u8).take(size - bytes.len()).collect();
                                bytes.append(&mut padding);
                                (size, Some(bytes), None)
                            },
                            // DS "Foo"
                            (ExpressionResult::String(s), None) => {
                                self.check_rom(&inner, "Data Declaration")?;
                                let bytes = s.into_bytes();
                                (bytes.len(), Some(bytes), None)
                            },
                            // DS 15
                            (ExpressionResult::Integer(size), None) => {
                                let size = util::positive_integer(&inner, size, "Invalid storage capacity")?;
                                if self.is_rom {
                                    let buffer = iter::repeat(0u8).take(size).collect();
                                    (size, Some(buffer), None)

                                } else {
                                    (size, None, None)
                                }
                            },
                            _ => {
                                return Err(inner.error("invalid Data Declaration format.".to_string()))
                            }
                        }
                    }
                };

                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    size,
                    EntryData::Data {
                        alignment,
                        endianess,
                        expressions,
                        bytes,
                        debug_only
                    },
                ));

            },
            _ => unreachable!()
        }
        Ok(())
    }

    pub fn add_marker(&mut self, inner: InnerToken, name: Option<String>) {
        self.entries.push(SectionEntry::new_unsized(self.id, inner, EntryData::Marker {
            name
        }));
    }

    pub fn resolve_addresses(&mut self, context: &mut EvaluatorContext) -> Result<(), SourceError> {
        let mut offset = 0;
        for e in &mut self.entries {

            // Update offset of entry
            e.offset = self.start_address + offset;

            // Update label addresses
            if let EntryData::Label { ref id, ..} = e.data {
                context.label_addresses.insert(*id, e.offset);

            // Verify alignment requirements
            } else if let EntryData::Data { ref alignment, .. } = e.data {
                if *alignment == DataAlignment::Word && e.offset & 0xFF != 0 {
                    return Err(e.inner.error(
                        "Invalid alignment of Data Declaration, \"DS16\" is required to start a low byte value of $00.".to_string()
                    ));

                } else if *alignment == DataAlignment::WithinWord && (e.offset & 0xFF) + e.size > 256 {
                    return Err(e.inner.error(
                        "Invalid alignment of Data Declaration, \"DS8\" is required to start and end within the same low byte.".to_string()
                    ));
                }
            }

            // Advance section offset by entry size
            offset += e.size

        }
        self.bytes_in_use = offset;
        Ok(())
    }

    pub fn resolve_arguments(&mut self, context: &mut EvaluatorContext) -> Result<(), SourceError> {
        for entry in &mut self.entries {

            // Set context rom_offset for relative offset calculation
            let end_of_instruction = (entry.offset + entry.size) as i32;
            context.rom_offset = end_of_instruction;

            if let EntryData::Data { ref mut bytes, ref expressions, ref endianess, .. } = entry.data {

                // Resolve Data Argument Expressions
                if let Some(expressions) = expressions {
                    let mut data_bytes = Vec::new();
                    for (width, expr) in expressions {
                        if *width == 1 {
                            data_bytes.push(
                                util::byte_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid byte data")?
                            );

                        } else {
                            let word = util::word_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid word data")?;
                            if *endianess == DataEndianess::Little {
                                data_bytes.push(word as u8);
                                data_bytes.push((word >> 8) as u8);

                            } else {
                                data_bytes.push((word >> 8) as u8);
                                data_bytes.push(word as u8);
                            }
                        }

                    }
                    *bytes = Some(data_bytes);
                }
                debug_assert_eq!(self.is_rom, bytes.is_some());

            } else if let EntryData::Instruction { ref op_code, ref mut bytes, ref expression, .. } = entry.data {
                if let Some(expr) = expression {
                    if let Some(argument) = instruction::argument(*op_code) {

                        // Handle constant/offset -> op code mapping
                        if let Some(offsets) = instruction::offsets(*op_code) {
                            let value = util::constant_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid constant argument")?;
                            let mut mapped_op_code = None;
                            for (constant_value, constant_op_code) in offsets {
                                if value == *constant_value as i32 {
                                    mapped_op_code = Some(*constant_op_code);
                                    break;
                                }
                            }

                            // Rewrite instruction to matched op code
                            if let Some(mapped_op_code) = mapped_op_code {
                                *bytes = instruction::bytes(mapped_op_code);

                            } else {
                                let valid_values: Vec<String> = offsets.iter().map(|(v, _)| v.to_string()).collect();
                                return Err(entry.inner.error(
                                    format!("Invalid constant value {}, one of the following values is required: {}", value, valid_values.join(", "))
                                ));
                            }

                        } else {
                            let mut instr_bytes = instruction::bytes(*op_code);
                            let mut arg_bytes: Vec<u8> = match argument {
                                Argument::MemoryLookupByteValue | Argument::ByteValue => {
                                    vec![
                                        util::byte_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid byte argument")?
                                    ]
                                },
                                Argument::MemoryLookupWordValue | Argument::WordValue => {
                                    let word = util::word_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid word argument")?;
                                    vec![
                                        word as u8,
                                        (word >> 8) as u8
                                    ]
                                },
                                Argument::SignedByteValue => {
                                    // ldsp hl,X
                                    if *op_code == 248 {
                                        vec![
                                            util::byte_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid signed byte argument")?
                                        ]

                                    // jr
                                    } else {
                                        let address = util::address_word_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid address")?;
                                        let target = address as i32 - end_of_instruction;
                                        vec![
                                            util::signed_byte_value(&entry.inner, target, "").map_err(|mut e| {
                                                e.message = format!("Relative jump offset of {} is out of range{}", target, e.message);
                                                e
                                            })?
                                        ]
                                    }
                                },
                                _ => unreachable!("Invalid argument type for instruction with expression")
                            };

                            instr_bytes.append(&mut arg_bytes);
                            *bytes = instr_bytes;
                        }

                    } else {
                        unreachable!("Instruction has expression but does not expect any argument: {}", op_code);
                    }
                }
            }
        }
        Ok(())
    }

    pub fn strip_debug(&mut self) {
        self.entries.retain(|entry| {
            match entry.data {
                EntryData::Instruction { debug_only, .. } => {
                    !debug_only
                },
                EntryData::Data { debug_only, .. } => {
                    !debug_only
                },
                _ => true
            }
        });
    }

    pub fn validate_jump_targets(&self, sections: &[Section]) -> Result<(), SourceError> {
        for entry in &self.entries {
            if let EntryData::Instruction { ref bytes, .. } = entry.data {
                if let Some(address) = instruction::jump_address(entry.offset + entry.size, bytes) {

                    for section in sections {
                        for entry in &section.entries {
                            // Either the start of an entry or the potential next entry after it
                            // are considered valid
                            if entry.offset == address || entry.offset + entry.size == address {
                                return Ok(());
                            }
                        }
                    }

                    return Err(entry.inner.error(
                        format!("Jump instruction does not target a valid address, ${:0>4x} is neither the start nor end of any section entry.", address)
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn validate_bounds(&self) -> Result<(), SourceError> {
        if self.start_address + self.bytes_in_use > self.end_address + 1 {
            Err(self.inner.error(format!(
                "Section contents exceeds allocated area ${:0>4x}-${:0>4x} by {} byte(s)",
                self.start_address,
                self.end_address,
                (self.start_address + self.bytes_in_use) - (self.end_address + 1)
            )))

        } else {
            Ok(())
        }
    }

    pub fn write_to_rom_buffer(&self, buffer: &mut [u8]) {
        if self.is_rom {
            for e in &self.entries {
                let offset = self.bank_offset + e.offset;
                e.write_to_rom_buffer(&mut buffer[offset..]);
            }
        }
    }

    fn check_rom(&self, inner: &InnerToken, msg: &str) -> Result<(), SourceError> {
        if self.is_rom {
            Ok(())

        } else {
            Err(inner.error(format!("Unexpected {} outside of ROM segment.", msg)))
        }
    }

    fn check_ram(&self, inner: &InnerToken, msg: &str) -> Result<(), SourceError> {
        if self.is_rom {
            Err(inner.error(format!("Unexpected {} outside of RAM segment.", msg)))

        } else {
            Ok(())
        }
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::super::test::{
        linker,
        linker_binary,
        linker_strip_debug,
        linker_error,
        linker_section_entries,
        linker_section_offsets
    };
    use super::EntryData;
    use crate::lexer::InnerToken;
    use crate::expression::{Expression, ExpressionValue, TEMPORARY_EXPRESSION_ID};
    use crate::expression::data::{DataAlignment, DataEndianess};

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

    // Markers ----------------------------------------------------------------
    #[test]
    fn test_section_markers() {
        assert_eq!(linker_section_entries(linker("SECTION ROM0\nSECTION 'B',ROM0\nSECTION 'C',ROM0\nSECTION ROM0\nSECTION WRAM0\nSECTION 'R',WRAM0")), vec![
            vec![
                (0, EntryData::Marker {
                    name: Some("B".to_string())
                }),
                (0, EntryData::Marker {
                    name: Some("C".to_string())
                }),
                (0, EntryData::Marker {
                    name: None
                })
            ],
            vec![
                (0, EntryData::Marker {
                    name: Some("R".to_string())
                })
            ]
        ]);
    }

    // Constant Evaluation ----------------------------------------------------
    #[test]
    fn test_section_entry_constant_eval() {
        let l = linker("int EQU 1\nfloat EQU 3.14\nstring EQU 'Hello World'\nSECTION ROM0\nDB int\nDB FLOOR(float)\nDS string");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![(1, (3, Expression::Value(ExpressionValue::ConstantValue(itk!(66, 69, "int"), "int".to_string()))))]),
                    bytes: Some(vec![1]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![(1, (5, Expression::BuiltinCall {
                        inner: itk!(73, 78, "FLOOR"),
                        name: "FLOOR".to_string(),
                        args: vec![Expression::Value(ExpressionValue::ConstantValue(itk!(79, 84, "float"), "float".to_string()))]

                    }))]),
                    bytes: Some(vec![3]),
                    debug_only: false
                }),
                (11, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_section_entry_constant_eval_referenced() {
        let l = linker("A EQU B\nB EQU C\nC EQU 2\nSECTION ROM0\nDB A\nDB B\nDB C");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![(1, (3, Expression::Value(ExpressionValue::ConstantValue(itk!(40, 41, "A"), "A".to_string()))))]),
                    bytes: Some(vec![2]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![(1, (4, Expression::Value(ExpressionValue::ConstantValue(itk!(45, 46, "B"), "B".to_string()))))]),
                    bytes: Some(vec![2]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: Some(vec![(1, (5, Expression::Value(ExpressionValue::ConstantValue(itk!(50, 51, "C"), "C".to_string()))))]),
                    bytes: Some(vec![2]),
                    debug_only: false
                }),
            ]
        ]);
    }


    // Labels Entries ---------------------------------------------------------
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
        let linker = linker_binary("SECTION ROM0\nINCBIN 'child.bin'", vec![1, 2, 3, 4]);
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
        let l = linker_strip_debug("SECTION ROM0\njr global\nmsg 'Hello World'\nglobal:\nld a,a");
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
        let l = linker_strip_debug("SECTION ROM0\njr global\nmsg 'Hello World'\nglobal:\nld a,a");
        assert_eq!(linker_section_offsets(l), vec![
            vec![
                (0, 2),
                (2, 0),
                (2, 1)
            ]
        ]);
    }

    // Jump Target Validation -------------------------------------------------
    #[test]
    fn test_section_jump_target_validation() {
        linker("SECTION ROM0\nld a,a\njr global\nSECTION ROM0[129]\nglobal:");
        linker("SECTION ROM0\nld a,a\nglobal:\nSECTION ROM0[127]\njr global");
        linker("SECTION ROM0\nld a,a\njp global\nSECTION ROM0[$2000]\nglobal:");
        linker("SECTION ROM0[$100]\nglobal:\nSECTION ROM0[$2000]\njp global");
        linker("SECTION ROM0\nld a,a\njp @-4");
        linker("SECTION ROM0\nld a,a\njr @-3");

        assert_eq!(linker_error("SECTION ROM0\nld a,a\njr @-1"), "In file \"main.gb.s\" on line 3, column 1: Jump instruction does not target a valid address, $0002 is neither the start nor end of any section entry.\n\njr @-1\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nld a,a\njp @+1"), "In file \"main.gb.s\" on line 3, column 1: Jump instruction does not target a valid address, $0005 is neither the start nor end of any section entry.\n\njp @+1\n^--- Here");
        assert_eq!(linker_error("SECTION ROM0\nld a,a\njp $2000"), "In file \"main.gb.s\" on line 3, column 1: Jump instruction does not target a valid address, $2000 is neither the start nor end of any section entry.\n\njp $2000\n^--- Here");
    }

}

