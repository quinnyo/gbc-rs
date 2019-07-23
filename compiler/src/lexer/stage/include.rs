// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use file_io::FileReader;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use super::super::LexerStage;
use super::super::token::{TokenGenerator, TokenIterator, TokenChar};
use super::super::{InnerToken, LexerFile, LexerToken, Symbol};


// Include Specific Tokens ----------------------------------------------------
lexer_token!(IncludeToken, IncludeType, (Debug, Eq, PartialEq, Clone), {
    Name(()),
    Register(()),
    Flag(()),
    Reserved(()),
    Segment(()),
    Instruction(()),
    MetaInstruction(()),
    Parameter(()),
    Offset(()),
    NumberLiteral(()),
    StringLiteral(()),
    TokenGroup((Vec<IncludeToken>)),
    BinaryFile((Vec<u8>)),
    BuiltinCall((Vec<Vec<IncludeToken>>)),
    Comma(()),
    Point(()),
    Colon(()),
    Operator(()),
    OpenParen(()),
    CloseParen(()),
    OpenBracket(()),
    CloseBracket(())
});


// Types ----------------------------------------------------------------------
struct IncludeLexerState<'a, T: FileReader> {
    file_reader: &'a T,
    files: &'a mut Vec<LexerFile>,
    parent_path: Option<&'a PathBuf>,
    child_path: &'a PathBuf
}

impl<'a, T: FileReader> IncludeLexerState<'a, T> {
    fn split_off_child(mut self, parent_path: &'a PathBuf) -> Self {
        self.parent_path = Some(parent_path);
        self
    }
}


// Include Level Lexer Implementation -----------------------------------------
pub struct IncludeStage;
impl LexerStage for IncludeStage {

    type Input = Self;
    type Output = IncludeToken;
    type Data = ();

    fn from_file<R: FileReader>(
        file_reader: &R,
        child_path: &PathBuf,
        files: &mut Vec<LexerFile>

    ) -> Result<Vec<Self::Output>, SourceError> {
        Self::include_child(IncludeLexerState {
            file_reader,
            files,
            parent_path: None,
            child_path

        }, Vec::new(), 0, 0, None)
    }

}

impl IncludeStage {

    fn include_child<T: FileReader>(
        state: IncludeLexerState<T>,
        include_stack: Vec<InnerToken>,
        file_index: usize,
        index: usize,
        using: Option<String>

    ) -> Result<Vec<IncludeToken>, SourceError>{

        // Read in child file contents
        let (child_path, mut contents) = state.file_reader.read_file(state.parent_path, state.child_path).map_err(|err| {
            SourceError::new(file_index, index, format!("Failed to include file \"{}\": {}", err.path.display(), err.io))
        })?;

        // Apply optional command to contents
        if let Some(using) = using {
            contents = state.file_reader.execute_command(Some(child_path.clone()), using.as_str(), contents).map_err(|err| {
                SourceError::new(file_index, index, err.to_string())
            })?;
        }

        // Create new file abstraction
        state.files.push(LexerFile::new(state.files.len(), contents, child_path.clone(), include_stack));

        // Create new lexer state for child file
        let mut state = state.split_off_child(&child_path);
        let file = state.files.last().unwrap();

        // Lex child tokens
        let child_tokens = Self::tokenize(file, &file.contents)?;

        // Resolve any includes in the tokenized file
        Ok(Self::resolve_include_tokens(
            file.index,
            child_tokens,
            &mut state,
        )?)

    }

    fn resolve_include_tokens<T: FileReader>(
        parent_file_index: usize,
        tokens: Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,

    ) -> Result<Vec<IncludeToken>, SourceError> {

        let mut expanded = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::INCLUDE) {
                match tokens.next() {
                    Some(IncludeToken::StringLiteral(token)) => {
                        let mut include_stack = state.files[parent_file_index].include_stack.clone();
                        include_stack.push(token.clone());

                        let child_state = IncludeLexerState {
                            file_reader: state.file_reader,
                            files: state.files,
                            parent_path: state.parent_path,
                            child_path: &PathBuf::from(token.value.to_string()),
                        };

                        let using = Self::using_directive(&mut tokens)?;
                        expanded.append(&mut Self::include_directive(
                            child_state,
                            parent_file_index,
                            token.start_index,
                            include_stack,
                            using
                        )?);
                    },
                    Some(IncludeToken::Reserved(ref name)) if name.value == Symbol::BINARY => {
                        match tokens.next() {
                            Some(IncludeToken::StringLiteral(token)) => {
                                let child_state = IncludeLexerState {
                                    file_reader: state.file_reader,
                                    files: state.files,
                                    parent_path: state.parent_path,
                                    child_path: &PathBuf::from(token.value.to_string())
                                };
                                let using = Self::using_directive(&mut tokens)?;
                                expanded.push(Self::include_binary_directive(
                                    child_state,
                                    token,
                                    using
                                )?);
                            },
                            Some(other) => return Err(other.error("Expected a StringLiteral instead.".to_string())),
                            None => return Err(token.error("Expected a StringLiteral to follow.".to_string()))
                        }
                    },
                    Some(other) => return Err(other.error("Expected a StringLiteral or BINARY keyword instead.".to_string())),
                    None => return Err(token.error("Expected a StringLiteral or BINARY keyword to follow.".to_string()))
                }

            } else {
                expanded.push(token);
            }
        }

        Ok(expanded)

    }

    fn using_directive(tokens: &mut TokenIterator<IncludeToken>) -> Result<Option<String>, SourceError> {
        if tokens.peek_is(IncludeType::Reserved, Some(Symbol::USING)) {
            tokens.expect(IncludeType::Reserved, Some(Symbol::USING), "when parsing USING directive")?;
            let command = tokens.expect(IncludeType::StringLiteral, None, "when parsing USING directive")?;
            Ok(Some(command.into_inner().value.to_string()))

        } else {
            Ok(None)
        }
    }

    fn include_directive<T: FileReader>(
        state: IncludeLexerState<T>,
        file_index: usize,
        index: usize,
        include_stack: Vec<InnerToken>,
        using: Option<String>

    ) -> Result<Vec<IncludeToken>, SourceError> {
        Self::include_child(state, include_stack, file_index, index, using)
    }

    fn include_binary_directive<T: FileReader>(
        state: IncludeLexerState<T>,
        token: InnerToken,
        using: Option<String>

    ) -> Result<IncludeToken, SourceError> {
        let (binary_path, mut bytes) = state.file_reader.read_binary_file(state.parent_path, state.child_path).map_err(|err| {
            SourceError::new(token.file_index, token.start_index, format!("Failed to include file \"{}\": {}", err.path.display(), err.io))
        })?;

        // Apply optional command to bytes
        if let Some(using) = using {
            bytes = state.file_reader.execute_binary_command(Some(binary_path), using.as_str(), &bytes).map_err(|err| {
                SourceError::new( token.file_index, token.start_index, err.to_string())
            })?;
        }
        Ok(IncludeToken::BinaryFile(token, bytes))
    }

    fn tokenize(file: &LexerFile, text: &str) -> Result<Vec<IncludeToken>, SourceError> {
        let mut iter = TokenGenerator::new(&file, text);
        Self::collect_tokens(&mut iter, false)
    }

    fn collect_tokens(iter: &mut TokenGenerator, inside_token_group: bool) -> Result<Vec<IncludeToken>, SourceError> {
        let mut tokens = Vec::with_capacity(8);
        while iter.peek().is_some() {
            let token = match iter.next() {
                // Whitespace
                ' ' | '\t' => continue, // Ignore whitespace,
                '\n' | '\r' => continue, // Ignore newlines
                // Names
                'a'...'z' | 'A'...'Z' | '_' => {
                    let name = Self::collect_inner_name(iter, true)?;
                    match name.value {
                        // Split into Reserved Words
                        Symbol::DB | Symbol::DW | Symbol::BW | Symbol::DS | Symbol::IF | Symbol::TO | Symbol::IN |
                        Symbol::DS8 | Symbol::EQU | Symbol::FOR |
                        Symbol::DS16 | Symbol::BANK |
                        Symbol::THEN | Symbol::ELSE | Symbol::ENDIF |
                        Symbol::MACRO | Symbol::USING | Symbol::BLOCK |
                        Symbol::ENDFOR | Symbol::REPEAT | Symbol::BINARY | Symbol::DEFAULT | Symbol::SECTION | Symbol::GLOBAL |
                        Symbol::INCLUDE | Symbol::VOLATILE |
                        Symbol::ENDMACRO | Symbol::ENDBLOCK => {
                            Some(IncludeToken::Reserved(name))
                        },
                        // ROM Segments
                        Symbol::ROM0 | Symbol::ROMX | Symbol::WRAM0 | Symbol::WRAMX | Symbol::HRAM | Symbol::RAM | Symbol::RAMX => {
                            Some(IncludeToken::Segment(name))
                        },
                        // Registers (c is later also treated as a flag)
                        Symbol::AF | Symbol::BC | Symbol::DE | Symbol::HL |
                        Symbol::A | Symbol::B | Symbol::C | Symbol::D | Symbol::E | Symbol::H | Symbol::L |
                        Symbol::HLD | Symbol::HLI | Symbol::SP => {
                            Some(IncludeToken::Register(name))
                        },
                        // Flags
                        Symbol::Z | Symbol::NZ | Symbol::NC => {
                            Some(IncludeToken::Flag(name))
                        },
                        // LR35902 Instructions
                        Symbol::Cp | Symbol::Di | Symbol::Ei | Symbol::Jp | Symbol::Jr | Symbol::Or | Symbol::Rl | Symbol::Rr | Symbol::Ld |
                        Symbol::Adc | Symbol::Add | Symbol::And | Symbol::Bit | Symbol::Ccf | Symbol::Cpl | Symbol::Daa | Symbol::Dec | Symbol::Inc |
                        Symbol::Ldh | Symbol::Nop | Symbol::Pop | Symbol::Res | Symbol::Ret | Symbol::Rla | Symbol::Rlc | Symbol::Rra | Symbol::Rrc |
                        Symbol::Rst | Symbol::Sbc | Symbol::Scf | Symbol::Set | Symbol::Sla | Symbol::Sra | Symbol::Srl | Symbol::Sub | Symbol::Xor |
                        Symbol::Halt | Symbol::Push | Symbol::Call | Symbol::Reti | Symbol::Rlca | Symbol::Rrca | Symbol::Stop | Symbol::Swap | Symbol::Ldsp => {
                            Some(IncludeToken::Instruction(name))
                        },
                        // gbasm "meta" Instructions
                        Symbol::Msg | Symbol::Brk | Symbol::Mul | Symbol::Div | Symbol::Incx | Symbol::Decx | Symbol::Addw | Symbol::Subw | Symbol::Ldxa |
                        Symbol::Retx | Symbol::Vsync | Symbol::Pushx | Symbol::Popx => {
                            Some(IncludeToken::MetaInstruction(name))
                        },
                        // All other names
                        _ => Some(IncludeToken::Name(name))
                    }

                },
                // Parameter / Offset
                '@' => {
                    if let Some('+') | Some('-') = iter.peek() {
                        Some(IncludeToken::Offset(iter.collect(false, |c, _| {
                            if let '_' = c {
                                TokenChar::Ignore

                            } else if let '0'...'9' | '+' | '-' = c {
                                TokenChar::Valid(c)

                            } else {
                                TokenChar::Invalid
                            }
                        })?))

                    } else if let Some('a'...'z') | Some('A'...'Z') = iter.peek() {
                        Some(IncludeToken::Parameter(Self::collect_inner_name(iter, false)?))

                    } else {
                        None
                    }
                },
                // NumberLiteral
                '0'...'9' => Some(Self::collect_number_literal(iter)?),
                '$' => if let Some('0'...'9') | Some('a'...'f') | Some('A'...'F') = iter.peek() {
                    Some(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'...'9' | 'a'...'f' | 'A'...'F' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?))

                } else {
                    None
                },
                '%' => if let Some('0'...'1') = iter.peek() {
                    Some(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'...'1' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?))

                } else {
                    Some(IncludeToken::Operator(iter.collect_single()))
                },
                // StringLiteral
                '"' => {
                    Some(IncludeToken::StringLiteral(Self::collect_inner_string(iter, '"')?))
                },
                '\'' => {
                    Some(IncludeToken::StringLiteral(Self::collect_inner_string(iter, '\'')?))
                },
                // Token Groups
                '`' => if inside_token_group{
                    return Ok(tokens);

                } else {
                    let index_token = iter.collect_single();
                    let group_start = iter.index();
                    let tokens = Self::collect_tokens(iter, true)?;
                    iter.assert_char('`', "Unclosed token group.".to_string())?;
                    iter.assert_index_changed(group_start, "Unclosed token group.".to_string())?;
                    Some(IncludeToken::TokenGroup(index_token, tokens))
                },
                // Operator
                '-' | '!' | '&' | '*' | '/' | '=' | '|' | '+' | '~' | '<' | '>' | '^' => {
                    Some(IncludeToken::Operator(iter.collect_single()))
                },
                // Punctation
                ';' => {
                    iter.skip_with(|c| {
                        c == '\n' || c == '\r'
                    });
                    continue;
                },
                ':' => Some(IncludeToken::Colon(iter.collect_single())),
                '.' => Some(IncludeToken::Point(iter.collect_single())),
                ',' => Some(IncludeToken::Comma(iter.collect_single())),
                '(' => Some(IncludeToken::OpenParen(iter.collect_single())),
                ')' => Some(IncludeToken::CloseParen(iter.collect_single())),
                '[' => Some(IncludeToken::OpenBracket(iter.collect_single())),
                ']' => Some(IncludeToken::CloseBracket(iter.collect_single())),
                _ => None
            };
            if let Some(token) = token {
                tokens.push(token);

            } else {
                return Err(iter.error());
            }
        }
        Ok(tokens)
    }

    fn collect_inner_string(iter: &mut TokenGenerator, delimeter: char) -> Result<InnerToken, SourceError> {
        let t = iter.collect(false, |c, p| {
            // Ignore escape slashes
            if c == '\\' && p != '\\' {
                TokenChar::Ignore

            // Handle escaped characters
            } else if p == '\\' {
                match c {
                    '0' => TokenChar::Valid('\0'),
                    'n' => TokenChar::Valid('\n'),
                    'r' => TokenChar::Valid('\r'),
                    't' => TokenChar::Valid('\t'),
                    '\'' => {
                        TokenChar::Valid('\'')
                    },
                    '"' => TokenChar::Valid('"'),
                    _ => TokenChar::Ignore
                }

            } else if c == delimeter {
                TokenChar::Delimeter

            } else {
                TokenChar::Valid(c)
            }
        })?;
        iter.assert_char(delimeter, "Unclosed string literal.".to_string())?;
        Ok(t)
    }

    fn collect_inner_name(iter: &mut TokenGenerator, inclusive: bool) -> Result<InnerToken, SourceError> {
        Ok(iter.collect(inclusive, |c, _| {
            if let 'a'...'z' | 'A'...'Z' | '_' | '0'...'9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })?)
    }

    fn collect_number_literal(iter: &mut TokenGenerator) -> Result<IncludeToken, SourceError> {
        let mut float = false;
        Ok(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
            if let '_' = c {
                TokenChar::Ignore

            } else if let '.' = c {
                if float {
                    TokenChar::Invalid
                } else {
                    float = true;
                    TokenChar::Valid(c)
                }

            } else if let '0'...'9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })?))
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use std::path::PathBuf;
    use crate::mocks::{MockFileReader, include_lex};
    use crate::lexer::Lexer;
    use super::{IncludeStage, IncludeToken, InnerToken};

    fn include_lexer<S: Into<String>>(s: S) -> Vec<IncludeToken> {
        include_lex(s).tokens
    }

    fn include_lexer_error<S: Into<String>>(s: S) -> String {
        colored::control::set_override(false);
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap().to_string()
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new(0, $start, $end, $parsed.into()))
        }
    }

    macro_rules! tkf {
        ($file:expr, $tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new($file, $start, $end, $parsed.into()))
        }
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    macro_rules! token_types {
        ($tok:ident, $id:expr) => {
            assert_eq!(include_lexer($id), vec![tk!($tok, 0, $id.len(), $id)]);
        };
        ($tok:ident, $id:expr, $($rest:expr), +) => {
            token_types!($tok, $id);
            token_types!($tok, $($rest),+)
        }
    }

    #[test]
    fn test_empty() {
        assert_eq!(include_lexer(""), vec![]);
    }

    #[test]
    fn test_resolve_includes() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s'\nINCLUDE 'extra/bar.gb.s'\nINCLUDE '/abs.gb.s'");
        reader.add_file("src/foo.gb.s", "42");
        reader.add_file("src/extra/bar.gb.s", "BAR");
        reader.add_file("src/abs.gb.s", "ABS");

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(1, NumberLiteral, 0, 2, "42"),
            tkf!(2, Name, 0, 3, "BAR"),
            tkf!(3, Name, 0, 3, "ABS"),
        ]);

    }

    #[test]
    fn test_resolve_include_using() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["--arg".into(), "--arg-two".into(), "src/foo.gb.s".into()],
            vec![52, 50],
            vec![53, 51],
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s' USING 'cmd --arg --arg-two'");
        reader.add_file("src/foo.gb.s", "42");

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(1, NumberLiteral, 0, 2, "53")
        ]);

    }

    #[test]
    fn test_error_resolve_include_using_command_not_found() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s' USING 'cmd'");
        reader.add_file("src/foo.gb.s", "42");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().expect("Expected lexer error").to_string();
        assert_eq!(err, "In file \"src/main.gb.s\" on line 1, column 9: Failed to execute command \"cmd\" on included file \"src/foo.gb.s\":\n\n---\ncmd: mock command not found---\n\nINCLUDE \'foo.gb.s\' USING \'cmd\'\n        ^--- Here");

    }


    #[test]
    fn test_error_resolve_include_using_missing_command_name() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s' USING ''");
        reader.add_file("src/foo.gb.s", "42");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().expect("Expected lexer error").to_string();
        assert_eq!(err, "In file \"src/main.gb.s\" on line 1, column 9: Failed to execute command \"\" on included file \"src/foo.gb.s\":\n\n---\nMissing command name---\n\nINCLUDE \'foo.gb.s\' USING \'\'\n        ^--- Here");

    }

    #[test]
    fn test_error_resolve_include_using_invalid_utf_8_output() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["src/foo.gb.s".into()],
            vec![52, 50],
            vec![255, 0],
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s' USING 'cmd'");
        reader.add_file("src/foo.gb.s", "42");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().expect("Expected lexer error").to_string();
        assert_eq!(err, "In file \"src/main.gb.s\" on line 1, column 9: Failed to execute command \"cmd\" on included file \"src/foo.gb.s\":\n\n---\nCommand did not return a valid string: invalid utf-8 sequence of 1 bytes from index 0---\n\nINCLUDE \'foo.gb.s\' USING \'cmd\'\n        ^--- Here");

    }

    #[test]
    fn test_error_resolve_include_using_stderr() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["src/foo.gb.s".into()],
            vec![52, 50],
            vec![],
            Some("Mock failure".to_string())
        );
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin' USING 'cmd'");
        reader.add_binary_file("src/data.bin", vec![]);

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().expect("Expected lexer error").to_string();
        assert_eq!(err, "In file \"src/main.gb.s\" on line 1, column 16: Failed to execute command \"cmd\" on included file \"src/data.bin\":\n\n---\ncmd: mock command not found---\n\nINCLUDE BINARY \'data.bin\' USING \'cmd\'\n               ^--- Here");

    }

    #[test]
    fn test_resolve_nested_includes() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "2\nINCLUDE 'extra/two.gb.s'\n4");
        reader.add_file("src/extra/two.gb.s", "INCLUDE '/three.gb.s'");
        reader.add_file("src/three.gb.s", "3");

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(0, NumberLiteral, 0, 1, "1"),
            tkf!(1, NumberLiteral, 0, 1, "2"),
            tkf!(3, NumberLiteral, 0, 1, "3"),
            tkf!(1, NumberLiteral, 27, 28, "4"),
        ]);

    }

    #[test]
    fn test_resolve_nested_include_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/main.gb.s\" on line 2, column 9: Failed to include file \"src/one.gb.s\": No Mock file provided\n\nINCLUDE \'one.gb.s\'\n        ^--- Here");

    }

    #[test]
    fn test_resolve_nested_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "2\nINCLUDE 'extra/two.gb.s'\n4");
        reader.add_file("src/extra/two.gb.s", "INCLUDE '/three.gb.s'");
        reader.add_file("src/three.gb.s", "@");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/three.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/extra/two.gb.s\" on line 1, column 9\nincluded from file \"src/one.gb.s\" on line 2, column 9\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "@");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/one.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE 4"), "In file \"main.gb.s\" on line 1, column 9: Expected a StringLiteral or BINARY keyword instead.\n\nINCLUDE 4\n        ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE"), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral or BINARY keyword to follow.\n\nINCLUDE\n^--- Here");
    }

    #[test]
    fn test_resolve_include_using_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE 'foo' USING 4"), "In file \"main.gb.s\" on line 1, column 21: Unexpected token \"NumberLiteral\" when parsing USING directive, expected a \"StringLiteral\" token instead.\n\nINCLUDE \'foo\' USING 4\n                    ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE 'foo' USING"), "In file \"main.gb.s\" on line 1, column 15: Unexpected end of input when parsing USING directive, expected a \"StringLiteral\" token instead.\n\nINCLUDE \'foo\' USING\n              ^--- Here");
    }

    #[test]
    fn test_resolve_include_binary() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin'\nINCLUDE BINARY 'second.bin'");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);
        reader.add_binary_file("src/second.bin", vec![42]);

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::BinaryFile(itk!(15, 25, "data.bin"), vec![0, 1, 2, 3, 4, 5, 6, 7]),
            IncludeToken::BinaryFile(itk!(41, 53, "second.bin"), vec![42])
        ]);

    }

    #[test]
    fn test_resolve_include_binary_using() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["--arg".into(), "--arg-two".into(), "src/data.bin".into()],
            vec![0, 1, 2, 3, 4, 5, 6, 7],
            vec![42],
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin' USING 'cmd --arg --arg-two'\n");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::BinaryFile(itk!(15, 25, "data.bin"), vec![42])
        ]);

    }

    #[test]
    fn test_resolve_nested_include_binary_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin'");
        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/main.gb.s\" on line 1, column 16: Failed to include file \"src/data.bin\": No Mock file provided\n\nINCLUDE BINARY \'data.bin\'\n               ^--- Here");

    }

    #[test]
    fn test_resolve_include_binary_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE BINARY 4"), "In file \"main.gb.s\" on line 1, column 16: Expected a StringLiteral instead.\n\nINCLUDE BINARY 4\n               ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE BINARY"), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCLUDE BINARY\n^--- Here");
    }

    #[test]
    fn test_resolve_include_binary_using_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE BINARY 'foo' USING 4"), "In file \"main.gb.s\" on line 1, column 28: Unexpected token \"NumberLiteral\" when parsing USING directive, expected a \"StringLiteral\" token instead.\n\nINCLUDE BINARY \'foo\' USING 4\n                           ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE BINARY 'foo' USING"), "In file \"main.gb.s\" on line 1, column 22: Unexpected end of input when parsing USING directive, expected a \"StringLiteral\" token instead.\n\nINCLUDE BINARY \'foo\' USING\n                     ^--- Here");
    }

    #[test]
    fn test_newlines() {
        assert_eq!(include_lexer("\n\r\n\n\r"), vec![
        ]);
    }

    #[test]
    fn test_name() {
        assert_eq!(include_lexer("INCLUDEX"), vec![tk!(Name, 0, 8, "INCLUDEX")]);
        assert_eq!(include_lexer("ol"), vec![tk!(Name, 0, 2, "ol")]);
        assert_eq!(include_lexer("q"), vec![tk!(Name, 0, 1, "q")]);
        assert_eq!(include_lexer("foo_bar"), vec![tk!(Name, 0, 7, "foo_bar")]);
        assert_eq!(include_lexer("_test"), vec![tk!(Name, 0, 5, "_test")]);
        assert_eq!(include_lexer("abcdefghijklmnopqrstuvwxyz"), vec![tk!(Name, 0, 26, "abcdefghijklmnopqrstuvwxyz")]);
        assert_eq!(include_lexer("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), vec![tk!(Name, 0, 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]);
    }

    #[test]
    fn test_reserved() {
        token_types!(
            Reserved,
            "DB", "DW", "BW", "IF", "TO", "IN",
            "FOR", "DS8", "DS16", "EQU", "BANK", "THEN", "ELSE",
            "ENDIF", "MACRO", "ENDFOR", "REPEAT", "USING", "GLOBAL",
            "DEFAULT", "SECTION", "VOLATILE", "ENDMACRO", "ENDBLOCK"
        );
    }

    #[test]
    fn test_segment() {
        token_types!(Segment, "ROM0", "ROMX", "WRAM0", "WRAMX", "HRAM", "RAM", "RAMX");
    }

    #[test]
    fn test_registers() {
        token_types!(Register, "af", "bc", "de", "hl", "a", "b", "c", "d", "e", "h", "l", "hli", "hld", "sp");
    }

    #[test]
    fn test_flags() {
        token_types!(Flag, "nc", "z", "nz");
    }

    #[test]
    fn test_instructions() {
        token_types!(Instruction, "cp", "di", "ei", "jp", "jr", "or", "rl", "rr", "ld");
        token_types!(Instruction, "adc", "add", "and", "bit", "ccf", "cpl", "daa", "dec", "inc", "ldh", "nop", "pop", "res", "ret", "rla", "rlc", "rra", "rrc", "rst", "sbc", "scf", "set", "sla", "sra", "srl", "sub", "xor");
        token_types!(Instruction, "halt", "push", "call", "reti", "rlca", "rrca", "stop", "swap", "ldsp");
    }

    #[test]
    fn test_meta_instructions() {
        token_types!(MetaInstruction, "msg", "brk", "mul", "div", "retx", "incx", "decx", "addw", "subw", "ldxa", "vsync", "pushx", "popx");
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(include_lexer("20_48"), vec![
            tk!(NumberLiteral, 0, 5, "2048")
        ]);
        assert_eq!(include_lexer("-512"), vec![
            tk!(Operator, 0, 1, "-"),
            tk!(NumberLiteral, 1, 4, "512")
        ]);
        assert_eq!(include_lexer("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234")
        ]);
        assert_eq!(include_lexer("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234")
        ]);
        assert_eq!(include_lexer("$abcdefABCDEF"), vec![
            tk!(NumberLiteral, 0, 13, "$abcdefABCDEF")
        ]);
        assert_eq!(include_lexer("%1001_0020"), vec![
            tk!(NumberLiteral, 0, 8, "%100100"),
            tk!(NumberLiteral, 8, 10, "20")
        ]);
        assert_eq!(include_lexer("2.4"), vec![
            tk!(NumberLiteral, 0, 3, "2.4")
        ]);
        assert_eq!(include_lexer("2.4."), vec![
            tk!(NumberLiteral, 0, 3, "2.4"),
            tk!(Point, 3, 4, ".")
        ]);
        assert_eq!(include_lexer("2.4.1"), vec![
            tk!(NumberLiteral, 0, 3, "2.4"),
            tk!(Point, 3, 4, "."),
            tk!(NumberLiteral, 4, 5, "1"),
        ]);
    }

    #[test]
    fn test_number_literal_hex_incomplete() {
        assert_eq!(include_lexer_error("$"), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(include_lexer("'Hello World'"), vec![
            tk!(StringLiteral, 0, 13, "Hello World")
        ]);
        assert_eq!(include_lexer("\"Hello World\""), vec![
            tk!(StringLiteral, 0, 13, "Hello World")
        ]);
        assert_eq!(include_lexer("'\"'"), vec![
            tk!(StringLiteral, 0, 3, "\"")
        ]);
        assert_eq!(include_lexer("\"'\""), vec![
            tk!(StringLiteral, 0, 3, "'")
        ]);
        assert_eq!(include_lexer("'\0\t\n\r'"), vec![
            tk!(StringLiteral, 0, 6, "\0\t\n\r")
        ]);
    }

    #[test]
    fn test_string_literal_unclosed() {
        assert_eq!(include_lexer_error("'Hello World"), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n'Hello World\n            ^--- Here");
        assert_eq!(include_lexer_error("\"Hello World"), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n\"Hello World\n            ^--- Here");
        assert_eq!(include_lexer_error("'''"), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\'\'\'\n   ^--- Here");
        assert_eq!(include_lexer_error("\"\"\""), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\"\"\"\n   ^--- Here");
    }

    #[test]
    fn test_string_literal_escapes() {
        assert_eq!(include_lexer("'\\n'"), vec![
            tk!(StringLiteral, 0, 4, "\n")
        ]);
        assert_eq!(include_lexer("'\\r'"), vec![
            tk!(StringLiteral, 0, 4, "\r")
        ]);
        assert_eq!(include_lexer("'\\t'"), vec![
            tk!(StringLiteral, 0, 4, "\t")
        ]);
        assert_eq!(include_lexer("'\\''"), vec![
            tk!(StringLiteral, 0, 4, "'")
        ]);
        assert_eq!(include_lexer("\"\\\"\""), vec![
            tk!(StringLiteral, 0, 4, "\"")
        ]);
    }

    #[test]
    fn test_string_punct() {
        assert_eq!(include_lexer(","), vec![tk!(Comma, 0, 1, ",")]);
        assert_eq!(include_lexer("."), vec![tk!(Point, 0, 1, ".")]);
        assert_eq!(include_lexer(":"), vec![tk!(Colon, 0, 1, ":")]);
        assert_eq!(include_lexer("()"), vec![
            tk!(OpenParen, 0, 1, "("),
            tk!(CloseParen, 1, 2, ")")
        ]);
        assert_eq!(include_lexer("[]"), vec![
            tk!(OpenBracket, 0, 1, "["),
            tk!(CloseBracket, 1, 2, "]")
        ]);
        assert_eq!(include_lexer("=%&|!-+<>~*/^"), vec![
            tk!(Operator, 0, 1, "="),
            tk!(Operator, 1, 2, "%"),
            tk!(Operator, 2, 3, "&"),
            tk!(Operator, 3, 4, "|"),
            tk!(Operator, 4, 5, "!"),
            tk!(Operator, 5, 6, "-"),
            tk!(Operator, 6, 7, "+"),
            tk!(Operator, 7, 8, "<"),
            tk!(Operator, 8, 9, ">"),
            tk!(Operator, 9, 10, "~"),
            tk!(Operator, 10, 11, "*"),
            tk!(Operator, 11, 12, "/"),
            tk!(Operator, 12, 13, "^"),
        ]);
    }

    #[test]
    fn test_param_offset() {
        assert_eq!(include_lexer("@param"), vec![tk!(Parameter, 0, 6, "param")]);
        assert_eq!(include_lexer("@param_foo"), vec![tk!(Parameter, 0, 10, "param_foo")]);
        assert_eq!(include_lexer("@+4"), vec![tk!(Offset, 0, 3, "+4")]);
        assert_eq!(include_lexer("@-4"), vec![tk!(Offset, 0, 3, "-4")]);
    }

    #[test]
    fn test_param_offset_incomplete() {
        assert_eq!(include_lexer_error("@"), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here");
    }

    #[test]
    fn test_drop_comments() {
        assert_eq!(include_lexer("2 ; A Comment"), vec![
            tk!(NumberLiteral, 0, 1, "2")
        ]);
    }

    #[test]
    fn test_multiline() {
        assert_eq!(include_lexer("q\n'Text'\n4"), vec![
            tk!(Name, 0, 1, "q"),
            tk!(StringLiteral, 2, 8, "Text"),
            tk!(NumberLiteral, 9, 10, "4")
        ]);
        assert_eq!(include_lexer("; A Comment\n2"), vec![
            tk!(NumberLiteral, 12, 13, "2"),
        ]);
    }

    #[test]
    fn test_group() {
        assert_eq!(include_lexer("``"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), Vec::new())
        ]);
        assert_eq!(include_lexer("`q\n'Text'\n4`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), vec![
                tk!(Name, 1, 2, "q"),
                tk!(StringLiteral, 3, 9, "Text"),
                tk!(NumberLiteral, 10, 11, "4")
            ])
        ]);
        assert_eq!(include_lexer("`\n\n`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), vec![
            ])
        ]);
    }

    #[test]
    fn test_group_unclosed() {
        assert_eq!(include_lexer_error("`a"), "In file \"main.gb.s\" on line 1, column 3: Unclosed token group.\n\n`a\n  ^--- Here");
        assert_eq!(include_lexer_error("`a``a"), "In file \"main.gb.s\" on line 1, column 6: Unclosed token group.\n\n`a``a\n     ^--- Here");
        assert_eq!(include_lexer_error("```"), "In file \"main.gb.s\" on line 1, column 4: Unclosed token group.\n\n```\n   ^--- Here");
    }

    #[test]
    fn test_error_location() {
        assert_eq!(include_lexer_error(" $"), "In file \"main.gb.s\" on line 1, column 2: Unexpected character \"$\".\n\n $\n ^--- Here");
        assert_eq!(include_lexer_error("\n$"), "In file \"main.gb.s\" on line 2, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
        assert_eq!(include_lexer_error("\n\n$"), "In file \"main.gb.s\" on line 3, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

}

