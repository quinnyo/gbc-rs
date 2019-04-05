// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::error::Error;


// Modules --------------------------------------------------------------------
mod optimizer;
mod section;
mod util;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{Lexer, LexerError, LexerToken, EntryStage, EntryToken};
use crate::expression::evaluator::{EvaluatorConstant, EvaluatorContext};
use self::section::Section;


// Linker Implementation ------------------------------------------------------
pub struct Linker {
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
            while SectionList::optimize_instructions(&mut sections) {
                SectionList::resolve(&mut sections, &mut context)?;
            }
        }

        SectionList::verify(&sections)?;

        Ok(Self {
            sections
        })
    }

    pub fn to_section_layout(&self) {
        // TODO return section layout and usage data
    }

    pub fn into_rom_buffer(self) -> Vec<u8> {
        let required_rom_size = SectionList::required_rom_size(&self.sections);
        let mut buffer: Vec<u8> = std::iter::repeat(0u8).take(required_rom_size).collect();
        SectionList::write_to_rom_buffer(&self.sections, &mut buffer);
        buffer
    }

}


// Section List Abstraction ---------------------------------------------------
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

    pub fn resolve(sections: &mut [Section], context: &mut EvaluatorContext) -> Result<(), LexerError> {
        for s in sections.iter_mut() {
            s.resolve_addresses(context)?;
        }
        for s in sections.iter_mut() {
            s.resolve_arguments(context)?;
        }
        Ok(())
    }

    pub fn strip_debug(sections: &mut [Section], context: &mut EvaluatorContext) -> Result<(), LexerError> {
        for s in sections.iter_mut() {
            s.strip_debug();
        }
        Self::resolve(sections, context)
    }

    pub fn optimize_instructions(sections: &mut Vec<Section>) -> bool {
        let mut optimzations_applied = false;
        for s in sections.iter_mut() {
            optimzations_applied |= optimizer::optimize_section_entries(&mut s.entries);
        }
        optimzations_applied
    }

    pub fn verify(sections: &[Section]) -> Result<(), LexerError> {
        for s in sections.iter() {
            s.validate_jump_targets(&sections)?;
            s.validate_bounds()?;
        }
        Ok(())
    }

    pub fn required_rom_size(sections: &[Section]) -> usize {

        let mut max_start_address = 0;
        for s in sections.iter() {
            if s.is_rom {
                max_start_address = cmp::max(max_start_address, s.start_address + s.bank_offset);
            }
        }

        let mut v = cmp::max(max_start_address, 0x4000) / 0x4000;

        // Next power of two
        v |= v >> 1;
        v |= v >> 2;
        v |= v >> 4;
        v |= v >> 8;
        v |= v >> 16;

        // Minimum of 32kb
        v += 1;

        // 32kb, 64kb, 128kb, 256kb etc.
        v * 0x4000

    }

    pub fn write_to_rom_buffer(sections: &[Section], buffer: &mut [u8]) {
        for s in sections.iter() {
            s.write_to_rom_buffer(&mut buffer[..]);
        }
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {

    use super::Linker;
    use super::section::entry::EntryData;
    use crate::lexer::stage::mocks::{entry_lex, entry_lex_binary};

    pub fn linker<S: Into<String>>(s: S) -> Linker {
        Linker::from_lexer(entry_lex(s.into()), false, false).expect("Linker failed")
    }

    pub fn linker_error<S: Into<String>>(s: S) -> String {
        Linker::from_lexer(entry_lex(s.into()), false, false).err().unwrap().to_string()
    }

    pub fn linker_strip_debug<S: Into<String>>(s: S) -> Linker {
        Linker::from_lexer(entry_lex(s.into()), true, false).expect("Debug stripping failed")
    }

    pub fn linker_optimize<S: Into<String>>(s: S) -> Linker {
        Linker::from_lexer(entry_lex(s.into()), true, true).expect("Instruction optimization failed")
    }

    pub fn linker_binary<S: Into<String>>(s: S, d: Vec<u8>) -> Linker {
        Linker::from_lexer(entry_lex_binary(s.into(), d), false, false).expect("Binary Linker failed")
    }

    pub fn linker_section_entries(linker: Linker) -> Vec<Vec<(usize, EntryData)>> {
        linker.sections.into_iter().map(|s| {
            s.entries.into_iter().map(|e| {
                (e.size, e.data)

            }).collect()

        }).collect()
    }

    pub fn linker_section_offsets(linker: Linker) -> Vec<Vec<(usize, usize)>> {
        linker.sections.into_iter().map(|s| {
            s.entries.into_iter().map(|e| {
                (e.offset, e.size)

            }).collect()

        }).collect()
    }

    fn linker_sections(linker: Linker) -> Vec<String> {
        linker.sections.iter().map(|s| {
            format!("{}", s)

        }).collect()
    }

    // ROM Sizing -------------------------------------------------------------
    #[test]
    fn test_rom_buffer_sizing() {
        assert_eq!(linker("").into_rom_buffer().len(), 0x8000);
        assert_eq!(linker("SECTION ROM0[$2000]").into_rom_buffer().len(), 0x8000);
        assert_eq!(linker("SECTION ROMX[$4000]").into_rom_buffer().len(), 0x8000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[1]").into_rom_buffer().len(), 0x8000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[2]").into_rom_buffer().len(), 0x10000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[3]").into_rom_buffer().len(), 0x10000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[4]").into_rom_buffer().len(), 0x20000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[5]").into_rom_buffer().len(), 0x20000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[6]").into_rom_buffer().len(), 0x20000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[7]").into_rom_buffer().len(), 0x20000);
        assert_eq!(linker("SECTION ROMX[$4000],BANK[8]").into_rom_buffer().len(), 0x40000);
    }

    // Constant Evaluation ----------------------------------------------------
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

    // ROM Buffer -------------------------------------------------------------
    #[test]
    fn test_rom_buffer_write() {
        let b = linker("SECTION ROM0\nDB $1, $2, $3, $4").into_rom_buffer();
        assert_eq!(b[0..4].to_vec(), vec![1u8, 2, 3, 4]);
        let b = linker("SECTION ROM0\nDW $2000").into_rom_buffer();
        assert_eq!(b[0..2].to_vec(), vec![0, 32]);
        let b = linker("SECTION ROM0\nBW $2000").into_rom_buffer();
        assert_eq!(b[0..2].to_vec(), vec![32, 0]);
        let b = linker("SECTION ROM0\nDS 'Hello World'").into_rom_buffer();
        assert_eq!(b[0..11].to_vec(), vec![72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]);
        let b = linker("SECTION ROM0\nstop\nld a,a\nld hl,$1000\n").into_rom_buffer();
        assert_eq!(b[0..10].to_vec(), vec![16, 0, 127, 33, 0, 16, 0, 0, 0, 0]);
    }

}

