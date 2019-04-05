// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;
use crate::expression::{DataExpression, OptionalDataExpression};
use crate::expression::data::{DataAlignment, DataEndianess};


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
    pub inner: InnerToken,
    pub section_id: usize,
    pub offset: usize,
    pub size: usize,
    pub data: EntryData
}

impl SectionEntry {

    pub fn new_label(section_id: usize, inner: InnerToken, id: usize) -> Self {
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

    pub fn new_with_size(section_id: usize, inner: InnerToken, size: usize, data: EntryData) -> Self {
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size,
            data
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
