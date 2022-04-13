// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;
use crate::expression::{DataExpression, OptionalDataExpression};
use crate::expression::data::{DataAlignment, DataEndianess};


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum EntryDataTyp {
    Marker,
    Label,
    Data,
    Instruction,
    Block
}


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
        op_code: u16,
        expression: OptionalDataExpression,
        bytes: Vec<u8>,
        volatile: bool,
        debug_only: bool
    },
    Block {
        command: String,
        bytes: Vec<u8>
    }
}

impl EntryData {
    fn typ(&self) -> EntryDataTyp {
        match self {
            Self::Marker { .. } => EntryDataTyp::Marker,
            Self::Label { .. } => EntryDataTyp::Label,
            Self::Data { .. } => EntryDataTyp::Data,
            Self::Instruction { .. } => EntryDataTyp::Instruction,
            Self::Block { .. } => EntryDataTyp::Block
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
}

impl SectionEntry {
    pub fn new_unsized(section_id: usize, inner: InnerToken, data: EntryData) -> Self {
        SectionEntry {
            inner,
            section_id,
            offset: 0,
            size: 0,
            data
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

    pub fn typ(&self) -> EntryDataTyp {
        self.data.typ()
    }

    pub fn rom_bytes(&self) -> Option<&[u8]> {
        match &self.data {
            EntryData::Instruction { bytes, .. } => Some(bytes),
            EntryData::Data { bytes: Some(bytes), .. } => Some(bytes),
            _ => None
        }
    }

    pub fn is_rom(&self) -> bool {
        matches!(&self.data, EntryData::Instruction { .. } | EntryData::Data { bytes: Some(_), .. })
    }

    pub fn write_to_rom_buffer(&self, buffer: &mut [u8]) {
        match &self.data {
            EntryData::Instruction { bytes, .. } => {
                for (index, b) in bytes.iter().enumerate() {
                    buffer[index] = *b;
                }
            }
            EntryData::Data { bytes: Some(bytes), .. } => {
                for (index, b) in bytes.iter().enumerate() {
                    buffer[index] = *b;
                }
            },
            _ => {}
        }
    }

}
