// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::fmt;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


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
        expressions: Option<Vec<DataExpression>>,
        bytes: Option<Vec<usize>>
    },
    Instruction {
        op: usize,
        argument: OptionalDataExpression,
        value: Option<ExpressionResult>,
        is_debug: bool
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SectionEntry {
    inner: InnerToken,
    section_id: usize,
    offset: usize,
    size: usize,
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
        // TODO if data entry has no bytes it needs to insert a padding of 0 bytes with it's size
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
    }

    pub fn resolve(sections: &mut Vec<Section>, context: &mut EvaluatorContext) {
        for s in sections.iter_mut() {
            s.resolve_addresses(context);
        }

        for s in sections.iter_mut() {
            s.resolve_arguments(context);
        }
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
            EntryToken::Instruction(inner, _) => {
                self.check_rom(&inner, "Instruction")?;
                // TODO get instr size
            },
            EntryToken::InstructionWithArg(inner, _, _) => {
                self.check_rom(&inner, "Instruction")?;
                // TODO get instr size
            },
            EntryToken::DebugInstruction(inner, _) => {
                self.check_rom(&inner, "Instruction")?;
                // TODO get instr size
                // TODO handle debug mode
            },
            EntryToken::DebugInstructionWithArg(inner, _, _) => {
                self.check_rom(&inner, "Instruction")?;
                // TODO handle debug mode
                // TODO get instr size
            },
            EntryToken::GlobalLabelDef(inner, id) | EntryToken::LocalLabelDef(inner, id) => {
                self.entries.push(SectionEntry::new_label(self.id, inner, id));
            },
            EntryToken::Data { inner, endianess, storage, alignment } => {

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
                        (exprs.len(), None, Some(exprs))
                    },
                    DataStorage::Words(exprs) => {
                        self.check_rom(&inner, "Data Declaration")?;
                        (exprs.len() * 2, None, Some(exprs))
                    },
                    DataStorage::Buffer(length, fill) => {
                        let length_or_string = context.resolve_expression(length)?;
                        let fill = context.resolve_optional_expression(fill)?;
                        match (length_or_string, fill) {
                            // DS 15 "FOO"
                            (ExpressionResult::Integer(size), Some(ExpressionResult::String(s))) => {
                                self.check_rom(&inner, "Data Declaration")?;
                                // TODO test negative
                                let size = util::positive_integer(&inner, size, "Invalid storage capacity")?;
                                let bytes = s.into_bytes();
                                // TODO validate string length to be < size and test
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
                                // TODO test negative
                                let size = util::positive_integer(&inner, size, "Invalid storage capacity")?;
                                (size, None, None)
                            },
                            _ => {
                                return Err(inner.error("invalid Data Declaration format.".to_string()))
                            }
                        }
                    }
                };

                // TODO create entry
                println!("{:?}", bytes)

            },
            _ => unreachable!()
        }
        Ok(())
    }

    pub fn add_marker(&mut self, _inner: InnerToken, _name: Option<String>) {
        // TODO push marker entry with size 0
    }

    pub fn resolve_addresses(&mut self, context: &mut EvaluatorContext)  {
        let mut offset = 0;
        for e in &mut self.entries {

            // Update label addresses
            if let EntryData::Label { ref id, ..} = e.data {
                context.label_addresses.insert(*id, offset);
            }

            // Update offset of entry
            e.offset = offset;

            // Advance section offset by entry size
            offset += e.size

        }
        self.bytes_in_use = offset;
    }

    pub fn resolve_arguments(&mut self, _context: &mut EvaluatorContext) {
        for e in &mut self.entries {
            if let EntryData::Data { .. } = e.data {
                // TODO resolve all data arguments

                /*
                let mut bytes = Vec::new();
                for expr in exprs {
                    bytes.push(
                        util::byte_value(&inner, context.resolve_expression(expr)?, "Invalid byte data")?
                    );
                }*/

                /*
                let mut bytes = Vec::new();
                for expr in exprs {
                    let word = util::word_value(&inner, context.resolve_expression(expr)?, "Invalid word data")?;
                    if endianess == DataEndianess::Little {
                        bytes.push(word as u8);
                        bytes.push((word >> 8) as u8);

                    } else {
                        bytes.push((word >> 8) as u8);
                        bytes.push(word as u8);
                    }
                }*/

            } else if let EntryData::Instruction { .. } = e.data {
                // TODO resolve all instruction arguments
                    // TODO check argument types and whether they fit into the data slot size
                // TODO jr Instructions need to convert to a relative value using their own offset
            }
        }
    }

    pub fn optimize_instructions(&mut self, _context: &mut EvaluatorContext) -> bool {
        // TODO run consecutive instruction entries through optimizer
        false
    }

    pub fn generate(&self, buffer: &mut [u8]) {
        if self.is_rom {
            let mut offset = self.start_address + self.bank_offset;
            for e in &self.entries {
                 e.generate(&mut buffer[offset..]);
                 offset += e.size;
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

