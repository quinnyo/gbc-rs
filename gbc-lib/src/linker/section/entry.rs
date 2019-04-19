// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;
use crate::expression::{DataExpression, OptionalDataExpression};
use crate::expression::data::{DataAlignment, DataEndianess};


// Types ----------------------------------------------------------------------
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EntryData {
    Marker {
        name: String
    },
    Label {
        id: usize,
        is_local: bool,
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

impl EntryData {
    pub fn into_bytes(self) -> Vec<u8> {
        match self {
            EntryData::Marker { .. }| EntryData::Label { .. }=> Vec::new(),
            EntryData::Data { bytes, .. } => bytes.unwrap_or_else(Vec::new),
            EntryData::Instruction { bytes, .. } => bytes
        }
    }

}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SectionEntry {
    pub inner: InnerToken,
    pub section_id: usize,
    pub offset: usize,
    pub size: usize,
    pub data: EntryData,
    pub compress: bool
}

impl SectionEntry {

    pub fn new_unsized(section_id: usize, inner: InnerToken, data: EntryData) -> Self {
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size: 0,
            data,
            compress: false
        }
    }

    pub fn new_with_size(section_id: usize, inner: InnerToken, size: usize, data: EntryData, compress: bool) -> Self {
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size,
            data,
            compress
        }
    }

    pub fn write_to_rom_buffer(&self, buffer: &mut [u8]) {
        match &self.data {
            EntryData::Instruction { bytes, .. } => {
                for (index, b) in bytes.iter().enumerate() {
                    buffer[index] = *b;
                }
            }
            EntryData::Data { bytes, .. } => if let Some(bytes) = bytes {
                for (index, b) in bytes.iter().enumerate() {
                    buffer[index] = *b;
                }
            },
            _ => {}
        }
    }

}
