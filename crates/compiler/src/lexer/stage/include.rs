// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use file_io::FileReader;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use super::super::LexerStage;
use super::super::token::{TokenGenerator, TokenChar};
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
    Member(()),
    NumberLiteral(()),
    StringLiteral(()),
    TokenGroup((Vec<IncludeToken>)),
    TokenGroupClose(()),
    BinaryFile((Vec<u8>)),
    BuiltinCall((Vec<Vec<IncludeToken>>)),
    ParentLabelCall((Vec<Vec<IncludeToken>>)),
    ParentLabeLDef((Vec<IncludeToken>)),
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
    parent_path: Option<PathBuf>,
    parent_file_index: usize,
    source_index: usize,
    file_path: &'a PathBuf
}

enum IncludeResult  {
    Text(PathBuf, String),
    Binary(PathBuf, Vec<u8>)
}


// Include Level Lexer Implementation -----------------------------------------
pub struct IncludeStage;
impl LexerStage for IncludeStage {

    type Input = Self;
    type Output = IncludeToken;
    type Data = ();

    fn from_file<R: FileReader>(
        file_reader: &R,
        file_path: &PathBuf,
        files: &mut Vec<LexerFile>

    ) -> Result<Vec<Self::Output>, SourceError> {
        let mut state = IncludeLexerState {
            file_reader,
            files,
            parent_path: None,
            parent_file_index: 0,
            source_index: 0,
            file_path
        };

        let (file_path, contents) = state.file_reader.read_file(state.parent_path.as_ref(), state.file_path).map_err(|err| {
            SourceError::new(state.parent_file_index, state.source_index, format!("Failed to include file \"{}\": {}", err.path.display(), err.io))
        })?;

        state.files.push(LexerFile::new(
            state.files.len(),
            contents,
            file_path.clone(),
            Vec::with_capacity(8)
        ));

        state.parent_path = Some(file_path);

        let mut tokens = Vec::with_capacity(2048);
        Self::tokenize(&mut tokens, &mut state, true)?;
        Ok(tokens)
    }

}

impl IncludeStage {

    pub fn tokenize_single<R: FileReader>(
        file_reader: &R,
        file_path: &PathBuf,
        files: &mut Vec<LexerFile>

    ) -> Result<Vec<IncludeToken>, SourceError> {
        let mut state = IncludeLexerState {
            file_reader,
            files,
            parent_path: None,
            parent_file_index: 0,
            source_index: 0,
            file_path
        };

        let (file_path, contents) = state.file_reader.read_file(state.parent_path.as_ref(), state.file_path).map_err(|err| {
            SourceError::new(state.parent_file_index, state.source_index, format!("Failed to include file \"{}\": {}", err.path.display(), err.io))
        })?;

        state.files.push(LexerFile::new(
            state.files.len(),
            contents,
            file_path,
            Vec::with_capacity(8)
        ));

        let mut tokens = Vec::with_capacity(2048);
        Self::tokenize(&mut tokens, &mut state, false)?;
        Ok(tokens)
    }

    fn tokenize<T: FileReader>(
        tokens: &mut Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,
        resolve: bool

    ) -> Result<(), SourceError> {
        let (mut gen, file_index) = {
            let file = state.files.last().unwrap();
            (TokenGenerator::new(file, &file.contents), file.index)
        };
        while gen.peek().is_some() {
            if let Some(token) = Self::match_token(&mut gen, false)? {
                match token {
                    ref t if t.is(IncludeType::Reserved) && t.is_symbol(Symbol::INCLUDE) => if resolve {
                        Self::tokenize_include(&mut gen, tokens, state, file_index)?;
                    },
                    t => tokens.push(t)
                }
            }
        }
        Ok(())
    }

    fn tokenize_include<T: FileReader>(
        gen: &mut TokenGenerator,
        tokens: &mut Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,
        parent_file_index: usize

    ) -> Result<(), SourceError> {
        match Self::next_token(gen, "when parsing INCLUDE directive, expected a StringLiteral or BINARY keyword instead")? {
            IncludeToken::StringLiteral(token) => {
                let using = Self::tokenize_using(gen)?;
                Self::incude_file(token, tokens, state, parent_file_index, using, false)
            },
            IncludeToken::Reserved(ref name) if name.value == Symbol::BINARY => {
                match Self::next_token(gen, "when parsing INCLUDE BINARY directive")? {
                    IncludeToken::StringLiteral(token) => {
                        let using = Self::tokenize_using(gen)?;
                        Self::incude_file(token, tokens, state, parent_file_index, using, true)
                    },
                    other => Err(other.error("Expected a StringLiteral instead.".to_string()))
                }
            },
            other => Err(other.error("Expected a StringLiteral or BINARY keyword instead.".to_string()))
        }
    }

    fn incude_file<T: FileReader>(
        token: InnerToken,
        tokens: &mut Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,
        parent_file_index: usize,
        using: Option<(String, bool)>,
        binary_input: bool

    ) -> Result<(), SourceError> {
        let mut include_stack = state.files[parent_file_index].include_stack.clone();
        include_stack.push(token.clone());

        let mut state = IncludeLexerState {
            file_reader: state.file_reader,
            files: state.files,
            parent_path: state.parent_path.clone(),
            parent_file_index,
            source_index: token.start_index,
            file_path: &PathBuf::from(token.value.to_string())
        };

        // Read Binary Files
        let input = if binary_input {
            let (file_path, bytes) = state.file_reader.read_binary_file(
                state.parent_path.as_ref(),
                state.file_path

            ).map_err(|err| {
                SourceError::new(
                    state.parent_file_index,
                    state.source_index,
                    format!("Failed to include file \"{}\": {}", err.path.display(), err.io)
                )
            })?;

            // Transform via Using Command
            if let Some((using, output_binary)) = using {
                let bytes = state.file_reader.execute_binary_command(
                    Some(file_path.clone()),
                    using.as_str(),
                    &bytes

                ).map_err(|err| {
                    SourceError::new(token.file_index, token.start_index, err.to_string())
                })?;

                // Convert Output for desired include
                if output_binary {
                    IncludeResult::Binary(file_path, bytes)

                } else {
                    IncludeResult::Text(file_path, String::from_utf8_lossy(&bytes).to_string())
                }

            } else {
                IncludeResult::Binary(file_path, bytes)
            }

        // Read Text Files
        } else {
            let (file_path, text) = state.file_reader.read_file(
                state.parent_path.as_ref(),
                state.file_path

            ).map_err(|err| {
                SourceError::new(
                    state.parent_file_index,
                    state.source_index,
                    format!("Failed to include file \"{}\": {}", err.path.display(), err.io)
                )
            })?;

            // Transform via Using Command
            if let Some((using, output_binary)) = using {
                let text = state.file_reader.execute_command(
                    Some(file_path.clone()),
                    using.as_str(),
                    text

                ).map_err(|err| {
                    SourceError::new(token.file_index, token.start_index, err.to_string())
                })?;

                // Convert Output for desired include
                if output_binary {
                    IncludeResult::Binary(file_path, text.into_bytes())

                } else {
                    IncludeResult::Text(file_path, text)
                }

            } else {
                IncludeResult::Text(file_path, text)
            }
        };

        match input {
            IncludeResult::Binary(_, bytes) => {
                tokens.push(IncludeToken::BinaryFile(token, bytes));
                Ok(())
            },
            IncludeResult::Text(file_path, text) => {
                state.files.push(LexerFile::new(
                    state.files.len(),
                    text,
                    file_path.clone(),
                    include_stack
                ));
                state.parent_path = Some(file_path);
                Self::tokenize(tokens, &mut state, true)
            }
        }

    }

    fn tokenize_using(gen: &mut TokenGenerator) -> Result<Option<(String, bool)>, SourceError> {
        let pre_state = gen.state();
        if let Ok(using_token) = Self::next_token(gen, "") {
            if using_token.is(IncludeType::Reserved) && using_token.is_symbol(Symbol::USING) {
                let binary_token = Self::next_token(gen, "when parsing USING directive, expected a StringLiteral or BINARY keyword instead")?;
                // USING BINARY "..."
                if binary_token.is(IncludeType::Reserved) && binary_token.is_symbol(Symbol::BINARY) {
                    let command_token = Self::next_token(gen, "when parsing USING directive, expected a StringLiteral or BINARY keyword instead")?;
                    if command_token.is(IncludeType::StringLiteral) {
                        return Ok(Some((
                            command_token.into_inner().value.to_string(),
                            true
                        )))

                    } else {
                        return Err(command_token.error("Expected a StringLiteral or BINARY keyword after USING keyword.".to_string()));
                    }

                // USING "..."
                } else if binary_token.is(IncludeType::StringLiteral) {
                    return Ok(Some((
                        binary_token.into_inner().value.to_string(),
                        false
                    )))

                } else {
                    return Err(binary_token.error("Expected a StringLiteral or BINARY keyword after USING keyword.".to_string()));
                }
            }
        }
        gen.set_state(pre_state);
        Ok(None)
    }

    fn next_token(
        gen: &mut TokenGenerator,
        message: &str

    ) -> Result<IncludeToken, SourceError> {
        if gen.peek().is_some()  {
            match Self::match_token(gen, false)? {
                Some(token) => Ok(token),
                None => Self::next_token(gen, message)
            }

        } else {
            Err(gen.end_of_input(message))
        }
    }

    fn match_token(gen: &mut TokenGenerator, inside_token_group: bool) -> Result<Option<IncludeToken>, SourceError> {
        match gen.next() {
            // Whitespace
            ' ' | '\t' | '\n' | '\r' => Ok(None),

            // Names
            'a'..='z' | 'A'..='Z' | '_' => {
                let name = Self::collect_inner_name(gen, true)?;
                match name.value {
                    // Split into Reserved Words
                    Symbol::DB | Symbol::DW | Symbol::BW | Symbol::DS | Symbol::IF | Symbol::TO | Symbol::IN |
                    Symbol::DS8 | Symbol::EQU | Symbol::FOR |
                    Symbol::DS16 | Symbol::BANK |
                    Symbol::THEN | Symbol::ELSE | Symbol::ENDIF | Symbol::CONST |
                    Symbol::MACRO | Symbol::USING | Symbol::BLOCK | Symbol::NAMESPACE |
                    Symbol::ENDFOR | Symbol::REPEAT | Symbol::BINARY | Symbol::DEFAULT | Symbol::SECTION | Symbol::GLOBAL |
                    Symbol::INCLUDE | Symbol::VOLATILE |
                    Symbol::ENDMACRO | Symbol::ENDBLOCK | Symbol::ENDNAMESPACE => {
                        Ok(Some(IncludeToken::Reserved(name)))
                    },
                    // ROM Segments
                    Symbol::ROM0 | Symbol::ROMX | Symbol::WRAM0 | Symbol::WRAMX | Symbol::HRAM | Symbol::RAM | Symbol::RAMX => {
                        Ok(Some(IncludeToken::Segment(name)))
                    },
                    // Registers (c is later also treated as a flag)
                    Symbol::AF | Symbol::BC | Symbol::DE | Symbol::HL |
                    Symbol::A | Symbol::B | Symbol::C | Symbol::D | Symbol::E | Symbol::H | Symbol::L |
                    Symbol::HLD | Symbol::HLI | Symbol::SP => {
                        Ok(Some(IncludeToken::Register(name)))
                    },
                    // Flags
                    Symbol::Z | Symbol::NZ | Symbol::NC => {
                        Ok(Some(IncludeToken::Flag(name)))
                    },
                    // LR35902 Instructions
                    Symbol::Cp | Symbol::Di | Symbol::Ei | Symbol::Jp | Symbol::Jr | Symbol::Or | Symbol::Rl | Symbol::Rr | Symbol::Ld |
                    Symbol::Adc | Symbol::Add | Symbol::And | Symbol::Bit | Symbol::Ccf | Symbol::Cpl | Symbol::Daa | Symbol::Dec | Symbol::Inc |
                    Symbol::Ldh | Symbol::Nop | Symbol::Pop | Symbol::Res | Symbol::Ret | Symbol::Rla | Symbol::Rlc | Symbol::Rra | Symbol::Rrc |
                    Symbol::Rst | Symbol::Sbc | Symbol::Scf | Symbol::Set | Symbol::Sla | Symbol::Sra | Symbol::Srl | Symbol::Sub | Symbol::Xor |
                    Symbol::Halt | Symbol::Push | Symbol::Call | Symbol::Reti | Symbol::Rlca | Symbol::Rrca | Symbol::Stop | Symbol::Swap | Symbol::Ldsp => {
                        Ok(Some(IncludeToken::Instruction(name)))
                    },
                    // gbasm "meta" Instructions
                    Symbol::Msg | Symbol::Brk | Symbol::Mul | Symbol::Div | Symbol::Neg | Symbol::Incx | Symbol::Decx | Symbol::Addw | Symbol::Subw | Symbol::Ldxa |
                    Symbol::Retx | Symbol::Vsync | Symbol::Pushx | Symbol::Popx | Symbol::Djnz | Symbol::Jc => {
                        Ok(Some(IncludeToken::MetaInstruction(name)))
                    },
                    // All other names
                    _ => Ok(Some(IncludeToken::Name(name)))
                }

            },
            // Parameter / Offset
            '@' => {
                if let Some('+') | Some('-') = gen.peek() {
                    Ok(Some(IncludeToken::Offset(gen.collect(false, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'..='9' | '+' | '-' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?)))

                } else if let Some('a'..='z') | Some('A'..='Z') = gen.peek() {
                    Ok(Some(IncludeToken::Parameter(Self::collect_inner_name(gen, false)?)))

                } else {
                    Err(gen.error("Expected a valid jump offset value"))
                }
            },
            // NumberLiteral
            '0'..='9' => Ok(Some(Self::collect_number_literal(gen)?)),
            '$' => if let Some('0'..='9') | Some('a'..='f') | Some('A'..='F') = gen.peek() {
                Ok(Some(IncludeToken::NumberLiteral(gen.collect(true, |c, _| {
                    if let '_' = c {
                        TokenChar::Ignore

                    } else if let '0'..='9' | 'a'..='f' | 'A'..='F' = c {
                        TokenChar::Valid(c)

                    } else {
                        TokenChar::Invalid
                    }
                })?)))

            } else {
                Err(gen.error("Expected a valid hexadecimal digit"))
            },
            '%' => if let Some('0'..='1') = gen.peek() {
                Ok(Some(IncludeToken::NumberLiteral(gen.collect(true, |c, _| {
                    if let '_' = c {
                        TokenChar::Ignore

                    } else if let '0'..='1' = c {
                        TokenChar::Valid(c)

                    } else {
                        TokenChar::Invalid
                    }
                })?)))

            } else {
                Ok(Some(IncludeToken::Operator(gen.collect_single())))
            },
            // StringLiteral
            '"' => {
                Ok(Some(IncludeToken::StringLiteral(Self::collect_inner_string(gen, '"')?)))
            },
            '\'' => {
                Ok(Some(IncludeToken::StringLiteral(Self::collect_inner_string(gen, '\'')?)))
            },
            // Token Groups
            '`' => if inside_token_group {
                Ok(Some(IncludeToken::TokenGroupClose(gen.collect_single())))

            } else {
                let index_token = gen.collect_single();
                let group_start = gen.index();
                let mut tokens = Vec::with_capacity(16);
                while gen.peek().is_some() {
                    match Self::match_token(gen, true)? {
                        Some(IncludeToken::TokenGroupClose(_)) => break,
                        Some(token) => tokens.push(token),
                        None => {}
                    }
                }
                gen.assert_char('`', "Unclosed token group.".to_string())?;
                gen.assert_index_changed(group_start, "Unclosed token group.".to_string())?;
                Ok(Some(IncludeToken::TokenGroup(index_token, tokens)))
            },
            // Operator
            '-'  => Ok(Some(IncludeToken::Operator(gen.collect_single()))),
            '!' | '&' | '*' | '/' | '=' | '|' | '+' | '~' | '<' | '>' | '^' => {
                Ok(Some(IncludeToken::Operator(gen.collect_single())))
            },
            // Comments
            ';' => {
                gen.skip_with(|c| {
                    c == '\n' || c == '\r'
                });
                Ok(None)
            },
            // Punctation
            ':' => {
                if let Some(':') = gen.peek() {
                    Ok(Some(IncludeToken::Member(gen.collect_double())))

                } else {
                    Ok(Some(IncludeToken::Colon(gen.collect_single())))
                }
            },
            '.' => Ok(Some(IncludeToken::Point(gen.collect_single()))),
            ',' => Ok(Some(IncludeToken::Comma(gen.collect_single()))),
            '(' => Ok(Some(IncludeToken::OpenParen(gen.collect_single()))),
            ')' => Ok(Some(IncludeToken::CloseParen(gen.collect_single()))),
            '[' => Ok(Some(IncludeToken::OpenBracket(gen.collect_single()))),
            ']' => Ok(Some(IncludeToken::CloseBracket(gen.collect_single()))),

            // Unexpected character
            _ => Err(gen.error("Unexpected character"))
        }
    }

    fn collect_inner_string(gen: &mut TokenGenerator, delimeter: char) -> Result<InnerToken, SourceError> {
        let t = gen.collect(false, |c, p| {
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
        gen.assert_char(delimeter, "Unclosed string literal.".to_string())?;
        Ok(t)
    }

    fn collect_inner_name(gen: &mut TokenGenerator, inclusive: bool) -> Result<InnerToken, SourceError> {
        gen.collect(inclusive, |c, _| {
            if let 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })
    }

    fn collect_number_literal(gen: &mut TokenGenerator) -> Result<IncludeToken, SourceError> {
        let mut float = false;
        Ok(IncludeToken::NumberLiteral(gen.collect(true, |c, _| {
            if let '_' = c {
                TokenChar::Ignore

            } else if let '.' = c {
                if float {
                    TokenChar::Invalid
                } else {
                    float = true;
                    TokenChar::Valid(c)
                }

            } else if let '0'..='9' = c {
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

    macro_rules! itkf {
        ($start:expr, $end:expr, $parsed:expr, $file:expr) => {
            InnerToken::new($file, $start, $end, $parsed.into())
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
    fn test_resolve_include_using_text() {

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
    fn test_resolve_include_using_binary() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["--arg".into(), "--arg-two".into(), "src/foo.gb.s".into()],
            vec![52, 50],
            vec![42],
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s' USING BINARY 'cmd --arg --arg-two'");
        reader.add_file("src/foo.gb.s", "42");

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::BinaryFile(itk!(8, 18, "foo.gb.s"), vec![42])
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
        assert_eq!(err.to_string(), "In file \"src/three.gb.s\" on line 1, column 1: Expected a valid jump offset value \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/extra/two.gb.s\" on line 1, column 9\nincluded from file \"src/one.gb.s\" on line 2, column 9\nincluded from file \"src/main.gb.s\" on line 2, column 9")

    }

    #[test]
    fn test_resolve_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "@");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/one.gb.s\" on line 1, column 1: Expected a valid jump offset value \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE 4"), "In file \"main.gb.s\" on line 1, column 9: Expected a StringLiteral or BINARY keyword instead.\n\nINCLUDE 4\n        ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE"), "In file \"main.gb.s\" on line 1, column 7: Unexpected end of input when parsing INCLUDE directive, expected a StringLiteral or BINARY keyword instead.\n\nINCLUDE\n      ^--- Here");
    }

    #[test]
    fn test_resolve_include_using_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE 'foo' USING 4"), "In file \"main.gb.s\" on line 1, column 21: Expected a StringLiteral or BINARY keyword after USING keyword.\n\nINCLUDE \'foo\' USING 4\n                    ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE 'foo' USING"), "In file \"main.gb.s\" on line 1, column 19: Unexpected end of input when parsing USING directive, expected a StringLiteral or BINARY keyword instead.\n\nINCLUDE \'foo\' USING\n                  ^--- Here");
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
    fn test_resolve_include_binary_using_text() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["--arg".into(), "--arg-two".into(), "src/data.bin".into()],
            vec![0, 1, 2, 3, 4, 5, 6, 7],
            b"DB $1, $2".to_vec(),
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin' USING 'cmd --arg --arg-two'\n");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::Reserved(itkf!(0, 2, "DB", 1)),
            IncludeToken::NumberLiteral(itkf!(3, 5, "$1", 1)),
            IncludeToken::Comma(itkf!(5, 6, ",", 1)),
            IncludeToken::NumberLiteral(itkf!(7, 9, "$2", 1))
        ]);

    }

    #[test]
    fn test_resolve_include_binary_using_binary() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_command(
            "cmd",
            vec!["--arg".into(), "--arg-two".into(), "src/data.bin".into()],
            vec![0, 1, 2, 3, 4, 5, 6, 7],
            vec![42],
            None
        );
        reader.add_file("src/main.gb.s", "INCLUDE BINARY 'data.bin' USING BINARY 'cmd --arg --arg-two'\n");
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
        assert_eq!(include_lexer_error("INCLUDE BINARY"), "In file \"main.gb.s\" on line 1, column 14: Unexpected end of input when parsing INCLUDE BINARY directive.\n\nINCLUDE BINARY\n             ^--- Here");
    }

    #[test]
    fn test_resolve_include_binary_using_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE BINARY 'foo' USING 4"), "In file \"main.gb.s\" on line 1, column 28: Expected a StringLiteral or BINARY keyword after USING keyword.\n\nINCLUDE BINARY \'foo\' USING 4\n                           ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE BINARY 'foo' USING"), "In file \"main.gb.s\" on line 1, column 26: Unexpected end of input when parsing USING directive, expected a StringLiteral or BINARY keyword instead.\n\nINCLUDE BINARY \'foo\' USING\n                         ^--- Here");
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
            "CONST", "ENDIF", "MACRO", "ENDFOR", "REPEAT", "USING", "GLOBAL", "NAMESPACE",
            "DEFAULT", "SECTION", "VOLATILE", "ENDMACRO", "ENDBLOCK", "ENDNAMESPACE"
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
        token_types!(MetaInstruction, "msg", "brk", "mul", "div", "neg", "retx", "incx", "decx", "addw", "subw", "ldxa", "vsync", "pushx", "popx");
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
        assert_eq!(include_lexer_error("$"), "In file \"main.gb.s\" on line 1, column 1: Expected a valid hexadecimal digit \"$\".\n\n$\n^--- Here");
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
        assert_eq!(include_lexer("::"), vec![tk!(Member, 0, 2, "::")]);
        assert_eq!(include_lexer(":::"), vec![tk!(Member, 0, 2, "::"), tk!(Colon, 2, 3, ":")]);
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
        assert_eq!(include_lexer_error("@"), "In file \"main.gb.s\" on line 1, column 1: Expected a valid jump offset value \"@\".\n\n@\n^--- Here");
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
        assert_eq!(include_lexer_error(" $"), "In file \"main.gb.s\" on line 1, column 2: Expected a valid hexadecimal digit \"$\".\n\n $\n ^--- Here");
        assert_eq!(include_lexer_error("\n$"), "In file \"main.gb.s\" on line 2, column 1: Expected a valid hexadecimal digit \"$\".\n\n$\n^--- Here");
        assert_eq!(include_lexer_error("\n\n$"), "In file \"main.gb.s\" on line 3, column 1: Expected a valid hexadecimal digit \"$\".\n\n$\n^--- Here");
    }

}

