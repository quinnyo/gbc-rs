// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::fmt;
use std::iter;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;
use gbasm_cpu::{self, Argument, Instruction};


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, LexerError, EntryToken};
use crate::expression::{OptionalDataExpression, DataExpression, ExpressionResult};
use crate::expression::data::{DataAlignment, DataEndianess, DataStorage};
use crate::expression::evaluator::EvaluatorContext;
use super::util;


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

// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum EntryData {
    Marker {
        name: String
    },
    Label {
        id: usize,
        // use for label map later on
        name: String
    },
    Data {
        alignment: DataAlignment,
        endianess: DataEndianess,
        expressions: Option<Vec<(usize, DataExpression)>>,
        bytes: Option<Vec<u8>>,
        debug_only: bool
    },
    Instruction {
        op_code: usize,
        expression: OptionalDataExpression,
        bytes: Vec<u8>,
        debug_only: bool
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SectionEntry {
    inner: InnerToken,
    section_id: usize,
    pub offset: usize,
    pub size: usize,
    pub data: EntryData
}

impl SectionEntry {

    fn new_label(section_id: usize, inner: InnerToken, id: usize) -> Self {
        let name = inner.value.clone();
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size: 0,
            data: EntryData::Label {
                name,
                id
            }
        }
    }

    fn new_with_size(section_id: usize, inner: InnerToken, size: usize, data: EntryData) -> Self {
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size,
            data
        }
    }

    fn generate(&self, _buffer: &mut [u8]) {
        // TODO write to buffer using the computed data and instruction values
    }

}


// Section Abstraction --------------------------------------------------------
pub struct SectionList;
impl SectionList {

    pub fn initialize(sections: &mut Vec<Section>) {
        // Sort sections by base address
        sections.sort_by(|a, b| {
            if a.start_address == b.start_address {
                a.bank.cmp(&b.bank)

            } else {
                a.start_address.cmp(&b.start_address)
            }
        });

        // Limit end_address of sections to next section start_address - 1
        let section_starts: Vec<(usize, usize, String)> = sections.iter().skip(1).map(|s| {
            (s.start_address, s.bank, s.segment.clone())

        }).collect();

        for (section, (next_start, next_bank, next_segment)) in sections.iter_mut().zip(section_starts.into_iter()) {
            if section.segment == next_segment && section.bank == next_bank && section.end_address >= next_start {
                section.end_address = next_start - 1;
            }
        }
    }

    pub fn resolve(sections: &mut Vec<Section>, context: &mut EvaluatorContext) -> Result<(), LexerError> {
        for s in sections.iter_mut() {
            s.resolve_addresses(context)?;
        }
        for s in sections.iter_mut() {
            s.resolve_arguments(context)?;
        }
        // TODO validate JR target across all sections
        // for s in &sections {
        //     s.validate_relative_jumps(&sections)?;
        // }
        Ok(())
    }

    pub fn strip_debug(sections: &mut Vec<Section>, context: &mut EvaluatorContext) -> Result<(), LexerError> {
        for s in sections.iter_mut() {
            s.strip_debug();
        }
        Self::resolve(sections, context)
    }

    pub fn optimize_instructions(sections: &mut Vec<Section>, context: &mut EvaluatorContext) -> bool {
        let mut optimzations_applied = false;
        for s in sections.iter_mut() {
            optimzations_applied |= s.optimize_instructions(context);
        }
        optimzations_applied
    }

}

#[derive(Debug, Eq, PartialEq)]
pub struct Section {
    pub id: usize,
    name: String,
    segment: String,
    inner: InnerToken,

    start_address: usize,
    end_address: usize,
    is_rom: bool,
    bank_offset: usize,
    bank: usize,

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

    ) -> Result<Self, LexerError> {

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

            entries: Vec::new(),
            bytes_in_use: 0
        })

    }

    pub fn hash(&self) -> (&str, usize) {
        (&self.segment, self.bank)
    }

    pub fn add_entry(&mut self, context: &mut EvaluatorContext, token: EntryToken) -> Result<(), LexerError> {
        match token {
            EntryToken::Instruction(inner, op_code) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction_size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: None,
                        bytes: instruction_bytes(op_code),
                        debug_only: false
                    }
                ))
            },
            EntryToken::InstructionWithArg(inner, op_code, expr) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction_size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: Some(expr),
                        bytes: instruction_bytes(op_code),
                        debug_only: false
                    }
                ))
            },
            EntryToken::DebugInstruction(inner, op_code) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction_size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: None,
                        bytes: instruction_bytes(op_code),
                        debug_only: true
                    }
                ))
            },
            EntryToken::DebugInstructionWithArg(inner, op_code, expr) => {
                self.check_rom(&inner, "Instruction")?;
                self.entries.push(SectionEntry::new_with_size(
                    self.id,
                    inner,
                    instruction_size(op_code),
                    EntryData::Instruction {
                        op_code,
                        expression: Some(expr),
                        bytes: instruction_bytes(op_code),
                        debug_only: true
                    }
                ))
            },
            EntryToken::GlobalLabelDef(inner, id) | EntryToken::LocalLabelDef(inner, id) => {
                self.entries.push(SectionEntry::new_label(self.id, inner, id));
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

    pub fn add_marker(&mut self, _inner: InnerToken, _name: Option<String>) {
        // TODO push marker entry with size 0
    }

    pub fn resolve_addresses(&mut self, context: &mut EvaluatorContext) -> Result<(), LexerError> {
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

    pub fn resolve_arguments(&mut self, context: &mut EvaluatorContext) -> Result<(), LexerError> {
        for entry in &mut self.entries {

            // Set context rom_offset for relative offset calculation
            context.rom_offset = (entry.offset + entry.size) as i32;

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
                    if let Some(argument) = instruction_argument(*op_code) {

                        // Handle constant/offset -> op code mapping
                        if let Some(offsets) = instruction_offsets(*op_code) {
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
                                *bytes = instruction_bytes(mapped_op_code);

                            } else {
                                let valid_values: Vec<String> = offsets.iter().map(|(v, _)| v.to_string()).collect();
                                return Err(entry.inner.error(
                                    format!("Invalid constant value {}, one of the following values is required: {}", value, valid_values.join(", "))
                                ));
                            }

                        } else {
                            let mut instr_bytes = instruction_bytes(*op_code);
                            let mut arg_bytes: Vec<u8> = match argument {
                                Argument::MemoryLookupByteValue | Argument::ByteValue => {
                                    vec![
                                        util::byte_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid byte argument")?
                                    ]
                                },
                                Argument::MemoryLookupWordValue | Argument::WordValue => {
                                    let word = util::positive_word_value(&entry.inner, context.resolve_expression(expr.clone())?, "Invalid word argument")?;
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
                                        vec![
                                            // TODO validate that address points at a valid section entry by running another pass after the optimzation phase
                                            // Value is calculated to end of JR instruction
                                            util::signed_byte_value(&entry.inner, address as i32 - entry.offset as i32, "Invalid signed byte argument")?
                                        ]
                                    }
                                },
                                _ => unreachable!("Invalid argument type for instruction with expression")
                            };

                            instr_bytes.append(&mut arg_bytes);
                            *bytes = instr_bytes;
                        }

                    } else {
                        unreachable!("Instruction has expression but does not expect any argument!");
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
        // TODO remove debug entries
    }

    pub fn optimize_instructions(&mut self, _context: &mut EvaluatorContext) -> bool {
        // TODO run consecutive instruction entries through optimizer
        false
    }

    pub fn generate(&self, buffer: &mut [u8]) {
        if self.is_rom {
            for e in &self.entries {
                let offset =self.bank_offset + e.offset;
                e.generate(&mut buffer[offset..]);
            }
        }
    }

    fn check_rom(&self, inner: &InnerToken, msg: &str) -> Result<(), LexerError> {
        if self.is_rom {
            Ok(())

        } else {
            Err(inner.error(format!("Unexpected {} outside of ROM segment.", msg)))
        }
    }

    fn check_ram(&self, inner: &InnerToken, msg: &str) -> Result<(), LexerError> {
        if self.is_rom {
            Err(inner.error(format!("Unexpected {} outside of RAM segment.", msg)))

        } else {
            Ok(())
        }
    }

}

// Helpers --------------------------------------------------------------------
fn instruction_size(op_code: usize) -> usize {
    INSTRUCTIONS[op_code].size
}

fn instruction_bytes(op_code: usize) -> Vec<u8> {
    INSTRUCTIONS[op_code].to_bytes()
}

fn instruction_offsets(op_code: usize) -> Option<&'static Vec<(usize, usize)>> {
    INSTRUCTIONS[op_code].offsets.as_ref()
}

fn instruction_argument(op_code: usize) -> Option<&'static Argument> {
    INSTRUCTIONS[op_code].argument.as_ref()
}

