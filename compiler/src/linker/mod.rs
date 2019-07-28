// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::mem;


// Modules --------------------------------------------------------------------
mod optimizer;
mod section;
mod util;


// External Dependencies ------------------------------------------------------
use file_io::FileReader;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use crate::compiler::Lint;
use crate::lexer::{Lexer, LexerToken, LexerFile, EntryStage, EntryToken, Symbol};
use crate::expression::{ExpressionResult, ExpressionValue};
use crate::expression::evaluator::{EvaluatorContext, UsageInformation};
use self::section::Section;


// Structs --------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq, Default)]
pub struct SegmentUsage {
    pub name: String,
    bank: usize,
    pub start_address: usize,
    pub end_address: usize,
    pub bytes_in_use: usize,
    pub ranges: Vec<(bool, Option<String>, usize, usize)>
}


// Linker Implementation ------------------------------------------------------
pub struct Linker {
    sections: Vec<Section>,
    context: EvaluatorContext,
    usage: UsageInformation,
    files: Vec<LexerFile>
}

impl Linker {

    pub fn from_lexer<R: FileReader>(
        file_reader: &R,
        lexer: Lexer<EntryStage>,
        strip_debug: bool,
        optimize: bool,
        linter_enabled: bool

    ) -> Result<Self, SourceError> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let mut linker = Self::new(file_reader, lexer.tokens, strip_debug, optimize, linter_enabled).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        linker.files = files;
        Ok(linker)
    }

    fn new<R: FileReader>(
        file_reader: &R,
        tokens: Vec<EntryToken>,
        strip_debug: bool,
        optimize: bool,
        linter_enabled: bool

    ) -> Result<Self, SourceError> {

        let mut context = EvaluatorContext::new(linter_enabled);
        let mut usage = UsageInformation::new();
        let mut entry_tokens = Vec::with_capacity(tokens.len());
        Self::parse_entries(&mut context, &mut usage, tokens, true, false, &mut entry_tokens, false)?;

        // Make sure that all constants are always evaluated even when they're not used by any
        // other expressions
        context.resolve_all_constants()?;

        // Map entries to sections
        let mut section_index = 0;
        let mut sections: Vec<Section> = Vec::new();
        for (volatile, token) in entry_tokens {
            if let EntryToken::SectionDeclaration { inner, name, segment_name, segment_offset, segment_size, bank_index } = token {

                // Parse options
                let name = util::opt_string(&inner, context.resolve_opt_const_expression(&name, &mut usage, inner.file_index)?, "Invalid section name")?;
                let segment_offset = util::opt_integer(&inner, context.resolve_opt_const_expression(&segment_offset, &mut usage, inner.file_index)?, "Invalid section offset")?;
                let segment_size = util::opt_integer(&inner, context.resolve_opt_const_expression(&segment_size, &mut usage, inner.file_index)?, "Invalid section size")?;
                let bank_index = util::opt_integer(&inner, context.resolve_opt_const_expression(&bank_index, &mut usage, inner.file_index)?, "Invalid section bank index")?;

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
                            if let Some(name) = name {
                                section.add_marker(inner, name);
                            }
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
                section.add_entry(&context, &mut usage, token, volatile)?;
            }
        }

        Self::setup_sections(&mut sections)?;

        // Initialize
        for s in sections.iter_mut() {
            if strip_debug {
                s.strip_debug();
            }
            s.initialize_using_blocks(file_reader)?;
        }

        Self::resolve_sections(&mut sections, &mut context, &mut usage)?;

        if optimize {
            // Run passes until no more optimizations were applied
            while Self::optimize_instructions(&mut sections) {
                Self::resolve_sections(&mut sections, &mut context, &mut usage)?;
            }
        }

        // Verify
        for s in sections.iter() {
            s.validate_jump_targets(&sections)?;
            s.validate_bounds()?;
        }

        Ok(Self {
            sections,
            context,
            usage,
            files: Vec::new()
        })
    }

    pub fn to_lint_list(&self) -> Vec<Lint> {
        let mut lints = Vec::new();
        let mut unused = self.context.find_unused(&self.usage);
        unused.append(&mut self.context.find_magic_numbers(&self.usage));
        unused.sort_by(|a, b| {
            (a.0, &a.1).cmp(&(b.0, &b.1))
        });
        for (_, _, error) in unused {
            lints.push(Lint::new(
                error.extend_with_basic_location(&self.files)
            ));
        }
        lints
    }

    pub fn to_symbol_list(&self) -> Vec<(usize, usize, String)> {
        let mut symbols = Vec::new();
        for section in &self.sections {
            symbols.append(&mut section.symbol_list());
        }
        symbols
    }

    pub fn to_usage_list(&self) -> Vec<SegmentUsage> {
        let mut segments = Vec::new();
        let mut current_segment = SegmentUsage::default();
        for section in &self.sections {
            if (current_segment.name.as_str(), current_segment.bank) != (section.segment.as_str(), section.bank) {

                // Append existing segment
                if !current_segment.name.is_empty() {
                    let mut segment = mem::replace(&mut current_segment, SegmentUsage::default());
                    segment.name = if segment.bank > 0 {
                        format!("{}[{}]", segment.name, segment.bank)

                    } else {
                        segment.name
                    };
                    segments.push(segment);
                }

                // Setup next segment
                current_segment.name = section.segment.to_string();
                current_segment.bank = section.bank;
                current_segment.start_address = section.start_address;
            }

            current_segment.ranges.append(&mut section.usage_list());
            current_segment.end_address = section.end_address + 1;
            current_segment.bytes_in_use += section.bytes_in_use;
        }

        // Append last segment
        if !current_segment.name.is_empty() {
            segments.push(current_segment);
        }
        segments
    }

    pub fn into_rom_buffer(self) -> Vec<u8> {
        let required_rom_size = Self::required_rom_size(&self.sections);
        let mut buffer: Vec<u8> = std::iter::repeat(0u8).take(required_rom_size).collect();
        for s in self.sections {
            s.write_to_rom_buffer(&mut buffer[..]);
        }
        buffer
    }

    fn parse_entries(
        context: &mut EvaluatorContext,
        usage: &mut UsageInformation,
        tokens: Vec<EntryToken>,
        allow_constant_declaration: bool,
        volatile_instructions: bool,
        remaining_tokens: &mut Vec<(bool, EntryToken)>,
        inactive_labels_only: bool

    ) -> Result<(), SourceError> {
        for token in tokens {
            // Record constants
            if let EntryToken::Constant { inner, is_default, is_private, value } = token {
                if !inactive_labels_only {
                    if allow_constant_declaration {
                        context.declare_constant(inner, is_default, is_private, value);

                    } else {
                        return Err(inner.error(
                            "Constant declaration is not allowed inside of a FOR statement body.".to_string()
                        ));
                    }
                }

            // Note label definitions for error messages
            } else if let EntryToken::ParentLabelDef(ref inner, index) = token {
                if inactive_labels_only {
                    context.declare_label(inner, index, false);

                } else {
                    context.declare_label(inner, index, true);
                    remaining_tokens.push((volatile_instructions, token));
                }

            // Evaluate if conditions and insert the corresponding branch into
            // the output tokens
            } else if let EntryToken::IfStatement(inner, branches) = token {
                let mut first_taken = false;
                for branch in branches {
                    let result = if let Some(condition) = branch.condition {
                        context.resolve_const_expression(&condition, usage, inner.file_index)?

                    } else {
                        ExpressionResult::Integer(1)
                    };

                    // Only parse tokens in first branch that is taken
                    if result.is_truthy() && first_taken == false {
                        first_taken = true;
                        Self::parse_entries(context, usage, branch.body, true, false, remaining_tokens, inactive_labels_only)?;

                    } else {
                        Self::parse_entries(context, usage, branch.body, true, false, remaining_tokens, true)?;
                    }
                }

            // Evaluate for ranges and insert the corresponding number of bodies
            // into the output tokens
            } else if let EntryToken::ForStatement(inner, for_statement) = token {

                let binding = for_statement.binding;
                let from = util::integer_value(&inner, context.resolve_const_expression(&for_statement.from, usage, inner.file_index)?, "Invalid for range argument")?;
                let to = util::integer_value(&inner, context.resolve_const_expression(&for_statement.to, usage, inner.file_index)?, "Invalid for range argument")?;

                let iterations = to - from;
                if iterations > 2048 {
                    return Err(inner.error(format!(
                        "FOR statement with {} iterations exceeds the maximum of 2048 allowed iterations.",
                        iterations
                    )));

                } else {
                    for i in from..to {
                        // Replace all occurrences of the index binding with the current index value
                        let value = ExpressionValue::Integer(i);
                        let body_tokens: Vec<EntryToken> = for_statement.body.iter().map(|token| {
                            let mut token = token.clone();
                            token.replace_constant(
                                &binding,
                                &value
                            );
                            token

                        }).collect();
                        Self::parse_entries(context, usage, body_tokens, false, false, remaining_tokens, inactive_labels_only)?;
                    }
                }

            // Expand and verify tokens inside using statements
            } else if let EntryToken::UsingStatement(inner, cmd, tokens) = token {

                let mut data_tokens = Vec::new();
                Self::parse_entries(context, usage, tokens, true, false, &mut data_tokens, inactive_labels_only)?;

                let mut body_tokens = Vec::new();
                for (_, token) in data_tokens {
                    match token {
                        EntryToken::Data { is_constant, .. } => {
                            if is_constant {
                                body_tokens.push(token);

                            } else {
                                return Err(token.error(
                                    "Only constant Data Declarations are allowed inside of USING BLOCK".to_string()
                                ))
                            }
                        },
                        _ => return Err(token.error(
                            format!("Unexpected {:?}, only Data Declarations are allowed inside of USING BLOCK", token.typ())
                        ))
                    }
                }
                remaining_tokens.push((false, EntryToken::UsingStatement(inner, cmd, body_tokens)));

            // Expand and set instructions to volatile inside volatile statements
            } else if let EntryToken::VolatileStatement(_, tokens) = token {
                Self::parse_entries(context, usage, tokens, true, true, remaining_tokens, inactive_labels_only)?;

            } else if !inactive_labels_only {
                remaining_tokens.push((volatile_instructions, token));
            }
        }
        Ok(())
    }

}

impl Linker {

    fn setup_sections(sections: &mut Vec<Section>) -> Result<(), SourceError> {
        // Sort sections by base address
        sections.sort_by(|a, b| {
            if a.start_address == b.start_address {
                a.bank.cmp(&b.bank)

            } else {
                a.start_address.cmp(&b.start_address)
            }
        });

        // Limit end_address of sections to next section start_address - 1
        let section_starts: Vec<(usize, usize, Symbol)> = sections.iter().skip(1).map(|s| {
            (s.start_address, s.bank, s.segment.clone())

        }).collect();

        for (section, (next_start, next_bank, next_segment)) in sections.iter_mut().zip(section_starts.into_iter()) {
            if section.segment == next_segment && section.bank == next_bank && section.end_address >= next_start {
                section.end_address = next_start - 1;
            }
        }

        Ok(())
    }

    fn resolve_sections(
        sections: &mut [Section],
        context: &mut EvaluatorContext,
        usage: &mut UsageInformation

    ) -> Result<(), SourceError> {
        for s in sections.iter_mut() {
            s.resolve_addresses(context)?;
        }
        for s in sections.iter_mut() {
            s.resolve_arguments(context, usage)?;
        }
        Ok(())
    }

    fn optimize_instructions(sections: &mut Vec<Section>) -> bool {
        let mut optimizations_applied = false;
        for s in sections.iter_mut() {
            if s.is_rom {
                optimizations_applied |= optimizer::optimize_section_entries(&mut s.entries);
            }
        }
        optimizations_applied
    }

    fn required_rom_size(sections: &[Section]) -> usize {

        let mut max_start_address = 0;
        for s in sections.iter() {
            if s.is_rom {
                max_start_address = cmp::max(max_start_address, s.start_address + s.bank_offset);
            }
        }

        let mut v = cmp::max(max_start_address, 0x4000) / 0x4000;

        // TODO verify against size in header and warn if mismatch

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

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {

    use super::{Linker, SegmentUsage};
    use super::section::entry::EntryData;
    use crate::expression::data::{DataAlignment, DataEndianess};
    use crate::mocks::{entry_lex, entry_lex_binary, MockFileReader, entry_lex_child};
    use file_io::FileReader;

    pub fn linker<S: Into<String>>(s: S) -> Linker {
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex(s.into()), false, false, false).expect("Linker failed")
    }

    pub fn linker_reader<R: FileReader, S: Into<String>>(reader: &R, s: S) -> Linker {
        Linker::from_lexer(reader, entry_lex(s.into()), false, false, false).expect("Linker failed")
    }

    pub fn linker_error_reader<R: FileReader, S: Into<String>>(reader: &R, s: S) -> String {
        colored::control::set_override(false);
        Linker::from_lexer(reader, entry_lex(s.into()), false, false, false).err().expect("Expected a Linker Error").to_string()
    }

    pub fn linker_error<S: Into<String>>(s: S) -> String {
        colored::control::set_override(false);
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex(s.into()), false, false, false).err().expect("Expected a Linker Error").to_string()
    }

    pub fn linker_child<S: Into<String>>(s: S, c: S) -> Linker {
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex_child(s.into(), c.into()), false, false, false).expect("Linker failed")
    }

    pub fn linker_error_child<S: Into<String>>(s: S, c: S) -> String {
        colored::control::set_override(false);
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex_child(s.into(), c.into()), false, false, false).err().expect("Expected a Linker Error").to_string()
    }

    pub fn linker_strip_debug<S: Into<String>>(s: S) -> Linker {
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex(s.into()), true, false, false).expect("Debug stripping failed")
    }

    pub fn linker_optimize<S: Into<String>>(s: S) -> Linker {
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex(s.into()), true, true, false).expect("Instruction optimization failed")
    }

    pub fn linker_binary<S: Into<String>>(s: S, d: Vec<u8>) -> Linker {
        let reader = MockFileReader::default();
        Linker::from_lexer(&reader, entry_lex_binary(s.into(), d), false, false, false).expect("Binary Linker failed")
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

    #[test]
    fn test_error_constant_eval_file_local() {
        assert_eq!(linker_error_child("A EQU B\nINCLUDE 'child.gb.s'", "B EQU 1"), "In file \"main.gb.s\" on line 1, column 7: Reference to undeclared constant \"B\".\n\nA EQU B\n      ^--- Here\n\nA non-global constant with the same name is defined in file \"child.gb.s\" on line 1, column 1:\n\nB EQU 1\n^--- Here");
        assert_eq!(linker_error_child("A EQU 1 + B\nINCLUDE 'child.gb.s'", "B EQU 1"), "In file \"main.gb.s\" on line 1, column 11: Reference to undeclared constant \"B\".\n\nA EQU 1 + B\n          ^--- Here\n\nA non-global constant with the same name is defined in file \"child.gb.s\" on line 1, column 1:\n\nB EQU 1\n^--- Here");
        assert_eq!(linker_error_child("A EQU 1\nINCLUDE 'child.gb.s'", "B EQU A"), "In file \"child.gb.s\" on line 1, column 7: Reference to undeclared constant \"A\".\n\nB EQU A\n      ^--- Here\n\nincluded from file \"main.gb.s\" on line 2, column 9\n\nA non-global constant with the same name is defined in file \"main.gb.s\" on line 1, column 1:\n\nA EQU 1\n^--- Here");
        assert_eq!(linker_error_child("A EQU 1\nINCLUDE 'child.gb.s'", "B EQU 1 + A"), "In file \"child.gb.s\" on line 1, column 11: Reference to undeclared constant \"A\".\n\nB EQU 1 + A\n          ^--- Here\n\nincluded from file \"main.gb.s\" on line 2, column 9\n\nA non-global constant with the same name is defined in file \"main.gb.s\" on line 1, column 1:\n\nA EQU 1\n^--- Here");
    }

    #[test]
    fn test_error_label_lookup_file_local() {
        assert_eq!(linker_error_child("SECTION ROM0\nINCLUDE 'child.gb.s'\nDW child_label", "child_label:"), "In file \"main.gb.s\" on line 3, column 4: Reference to undeclared constant \"child_label\".\n\nDW child_label\n   ^--- Here\n\nA non-global label with the same name is defined in file \"child.gb.s\" on line 1, column 1:\n\nchild_label:\n^--- Here");
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

    // Symbols ----------------------------------------------------------------
    #[test]
    fn test_symbols() {
        let s = linker("SECTION ROM0\nglobal:\n.local:\nSECTION ROMX\nglobal_2:\n.local:\nSECTION WRAM0\nvar: DB\nSECTION RAMX,BANK[2]\nvar_two: DB\nSECTION HRAM\nbuffer: DS 5").to_symbol_list();
        assert_eq!(s, vec![
            (0, 0, "global".to_string()),
            (0, 0, "global.local".to_string()),
            (1, 16384, "global_2".to_string()),
            (1, 16384, "global_2.local".to_string()),
            (2, 40960, "var_two".to_string()),
            (0, 49152, "var".to_string()),
            (0, 65408, "buffer".to_string())
        ]);
        let s = linker("SECTION ROM0\nglobal:\nSECTION ROM0[$100]\n.local:").to_symbol_list();
        assert_eq!(s, vec![
            (0, 0, "global".to_string()),
            (0, 256, "local".to_string()),
        ]);
    }

    // Usage ------------------------------------------------------------------
    #[test]
    fn test_usage() {
        let u = linker("SECTION 'Data',ROM0\nDS 256\nDS 32\nSECTION 'Data1',ROM0\nDS 8\nDS 4\nSECTION 'Data2',ROM0\nDS 16\nSECTION ROM0\nDS 48\nSECTION 'Extra',ROM0[$200]\nDS 5\nSECTION ROMX\nDS 128\nSECTION 'X',ROMX\nDS 32\nSECTION 'GameRam',WRAM0[$C3B0]\nSECTION 'Vars',WRAM0[$C000]\nvar: DB\nSECTION 'Buffer',WRAM0\nbuffer: DS 512\nSECTION 'Vars',HRAM\nfoo: DS 128").to_usage_list();
        assert_eq!(u, vec![
            SegmentUsage {
                name: "ROM0".to_string(),
                bank: 0,
                start_address: 0,
                end_address: 16384,
                bytes_in_use: 369,
                ranges: vec![
                    (true, Some("Data".to_string()), 0, 288),
                    (true, Some("Data1".to_string()), 288, 300),
                    (true, Some("Data2".to_string()), 300, 364),
                    (false, None, 364, 512),
                    (true, Some("Extra".to_string()), 512, 517),
                    (false, None, 517, 16384)
                ]
            },
            SegmentUsage {
                name: "ROMX[1]".to_string(),
                bank: 1,
                start_address: 16384,
                end_address: 32768,
                bytes_in_use: 160,
                ranges: vec![
                    (true, None, 16384, 16512),
                    (true, Some("X".to_string()), 16512, 16544),
                    (false, None, 16544, 32768)
                ]
            },
            SegmentUsage {
                name: "WRAM0".to_string(),
                bank: 0,
                start_address: 49152,
                end_address: 53248,
                bytes_in_use: 513,
                ranges: vec![
                    (true, Some("Vars".to_string()), 49152, 49153),
                    (true, Some("Buffer".to_string()), 49153, 49665),
                    (false, None, 49665, 50096),
                    (true, Some("GameRam".to_string()), 50096, 50096),
                    (false, None, 50096, 53248)
                ]
            },
            SegmentUsage {
                name: "HRAM".to_string(),
                bank: 0,
                start_address: 65408,
                end_address: 65536,
                bytes_in_use: 128,
                ranges: vec![
                    (true, Some("Vars".to_string()), 65408, 65536)
                ]
            }
        ]);
    }

    // If Statements ----------------------------------------------------------
    #[test]
    fn test_if_statement() {
        let l = linker("SECTION ROM0\nIF 0 THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("SECTION ROM0\nIF 0 + 1 THEN DB 1 ELSE IF 2 THEN DB 2 ELSE DB 3 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
        let l = linker("A EQU 0\nSECTION ROM0\nIF A THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("A EQU 1\nSECTION ROM0\nIF A THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_if_statement_condition_types() {
        let l = linker("SECTION ROM0\nIF 0.0 THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("SECTION ROM0\nIF 1.0 THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
        let l = linker("SECTION ROM0\nIF '' THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("SECTION ROM0\nIF 'A' THEN DB 1 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
        let l = linker("SECTION ROM0\nIF 0 THEN\nIF 1 THEN DB 1\nENDIF\nENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
    }

    #[test]
    fn test_error_if_statement_condition() {
        assert_eq!(linker_error("SECTION ROM0\nIF A THEN DB 1 ENDIF"), "In file \"main.gb.s\" on line 2, column 4: Reference to undeclared constant \"A\".\n\nIF A THEN DB 1 ENDIF\n   ^--- Here");
    }

    #[test]
    fn test_if_statement_else() {
        let l = linker("SECTION ROM0\nIF 0 THEN DB 1 ELSE DB 2 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_if_statement_else_if() {
        let l = linker("SECTION ROM0\nIF 0 THEN DB 1 ELSE IF 0 THEN DB 2 ELSE DB 3 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![3]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_if_statement_nested() {
        let l = linker("SECTION ROM0\nIF 1 THEN IF 2 THEN DB 2 ENDIF ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_if_statement_constant_declare() {
        let l = linker("SECTION ROM0\nIF 1 THEN A EQU 1 ENDIF\nIF A THEN DB 2 ENDIF");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_if_statement_unreachable_label() {
        assert_eq!(
            linker_error("SECTION ROM0\ncall global_label\nIF 0 THEN\nglobal_label:\nENDIF"),
            "In file \"main.gb.s\" on line 2, column 6: Reference to unreachable label\n\ncall global_label\n     ^--- Here\n\nLabel is declared inside currently inactive IF branch in file \"main.gb.s\" on line 4, column 1:\n\nglobal_label:\n^--- Here"
        );
    }

    // FOR Statements ---------------------------------------------------------
    #[test]
    fn test_for_statement() {
        let l = linker("SECTION ROM0\nFOR x IN 0 TO 0 REPEAT DB 1 ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("SECTION ROM0\nFOR x IN 1 + 2 TO 0 REPEAT DB 1 ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![]
        ]);
        let l = linker("SECTION ROM0\nFOR x IN 0 TO 0 + 3 REPEAT DB 1 ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_for_statement_constant_replacement() {
        let l = linker("SECTION ROM0\nFOR x IN 0 TO 0 + 3 REPEAT DB x ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                }),
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![2]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_for_statement_constant_replacement_nested() {
        let l = linker("SECTION ROM0\nFOR x IN 0 TO 2 REPEAT\n FOR y IN 0 TO 2 REPEAT\n DB x, y\n ENDFOR\n ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0, 0]),
                    debug_only: false
                }),
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![0, 1]),
                    debug_only: false
                }),
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1, 0]),
                    debug_only: false
                }),
                (2, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1, 1]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_for_statement_constant_replacement_if() {
        let l = linker("SECTION ROM0\nFOR x IN 0 TO 2 REPEAT IF x > 0 THEN DB x ENDIF ENDFOR");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Data {
                    alignment: DataAlignment::Byte,
                    endianess: DataEndianess::Little,
                    expressions: None,
                    bytes: Some(vec![1]),
                    debug_only: false
                })
            ]
        ]);
    }

    #[test]
    fn test_error_for_statement_inner_constant_declaration() {
        assert_eq!(linker_error("SECTION ROM0\nFOR x IN 0 TO 2 REPEAT foo EQU x + 1ENDFOR"), "In file \"main.gb.s\" on line 2, column 24: Constant declaration is not allowed inside of a FOR statement body.\n\nFOR x IN 0 TO 2 REPEAT foo EQU x + 1ENDFOR\n                       ^--- Here");
    }

    #[test]
    fn test_error_for_statement_max_iterations() {
        linker("SECTION ROM0\nFOR x IN 0 TO 2048 REPEAT ENDFOR");
        assert_eq!(linker_error("SECTION ROM0\nFOR x IN 0 TO 2049 REPEAT ENDFOR"), "In file \"main.gb.s\" on line 2, column 1: FOR statement with 2049 iterations exceeds the maximum of 2048 allowed iterations.\n\nFOR x IN 0 TO 2049 REPEAT ENDFOR\n^--- Here");
    }

    // Macros -----------------------------------------------------------------
    #[test]
    fn test_macro_jump_to_internal_parent_label() {
        linker("SECTION ROM0\nMACRO FOO() parent:\njr parent\n ENDMACRO\nFOO()");
    }

    // Blocks -----------------------------------------------------------------
    #[test]
    fn test_error_block_using() {
        assert_eq!(linker_error("BLOCK USING 'cmd' nop ENDBLOCK"), "In file \"main.gb.s\" on line 1, column 19: Unexpected Instruction, only Data Declarations are allowed inside of USING BLOCK\n\nBLOCK USING \'cmd\' nop ENDBLOCK\n                  ^--- Here");
        assert_eq!(linker_error("global:\nBLOCK USING 'cmd' DW global ENDBLOCK"), "In file \"main.gb.s\" on line 2, column 19: Only constant Data Declarations are allowed inside of USING BLOCK\n\nBLOCK USING \'cmd\' DW global ENDBLOCK\n                  ^--- Here");
        assert_eq!(linker_error("BLOCK USING 'cmd' global: ENDBLOCK"), "In file \"main.gb.s\" on line 1, column 19: Unexpected ParentLabelDef, only Data Declarations are allowed inside of USING BLOCK\n\nBLOCK USING \'cmd\' global: ENDBLOCK\n                  ^--- Here");
    }

    #[test]
    fn test_block_volatile() {
        let l = linker("SECTION ROM0\nBLOCK VOLATILE nop\nld a,a\nccf ENDBLOCK");
        assert_eq!(linker_section_entries(l), vec![
            vec![
                (1, EntryData::Instruction {
                    op_code: 0,
                    expression: None,
                    bytes: vec![0],
                    volatile: true,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 127,
                    expression: None,
                    bytes: vec![127],
                    volatile: true,
                    debug_only: false
                }),
                (1, EntryData::Instruction {
                    op_code: 63,
                    expression: None,
                    bytes: vec![63],
                    volatile: true,
                    debug_only: false
                })
            ]
        ]);
    }

}

