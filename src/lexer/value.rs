// STD Dependencies -----------------------------------------------------------
use std::error::Error;
use std::collections::HashSet;


// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use super::{MacroLexer, InnerToken, TokenIterator, TokenType, LexerToken, LexerFile, LexerError};
use super::macros::MacroToken;


// Value Specific Tokens ------------------------------------------------------
#[allow(unused)]
#[derive(Debug, Eq, PartialEq)]
pub enum ValueToken {
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
    Comma(InnerToken),
    OpenParen(InnerToken),
    CloseParen(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
    BuiltinCall(InnerToken, Vec<Vec<ValueToken>>),
    Offset {
        inner: InnerToken,
        value: i32
    },
    Float {
        inner: InnerToken,
        value: OrderedFloat<f32>
    },
    Integer {
        inner: InnerToken,
        value: i32
    },
    String {
        inner: InnerToken,
        value: String
    },
    GlobalLabelDef {
        inner: InnerToken,
        name: String,
    },
    LocalLabelDef {
        inner: InnerToken,
        name: String
    },
    LocalLabelRef {
        inner: InnerToken,
        name: String
    },
    Operator {
        inner: InnerToken,
        typ: Operator
    }
}

impl LexerToken for ValueToken {

    fn typ(&self) -> TokenType {
        match self {
            ValueToken::Name(_) => TokenType::Name,
            ValueToken::Reserved(_) => TokenType::Reserved,
            ValueToken::Instruction(_) => TokenType::Instruction,
            ValueToken::BinaryFile(_, _) => TokenType::BinaryFile,
            ValueToken::BuiltinCall(_, _) => TokenType::BuiltinCall,
            ValueToken::Comma(_) => TokenType::Comma,
            ValueToken::OpenParen(_) => TokenType::OpenParen,
            ValueToken::CloseParen(_) => TokenType::CloseParen,
            ValueToken::OpenBracket(_) => TokenType::OpenBracket,
            ValueToken::CloseBracket(_) => TokenType::CloseBracket,
            ValueToken::Offset { .. } => TokenType::Offset,
            ValueToken::Float { .. } => TokenType::Float,
            ValueToken::Integer { .. } => TokenType::Integer,
            ValueToken::String { .. } => TokenType::String,
            ValueToken::GlobalLabelDef { .. } => TokenType::GlobalLabelDef,
            ValueToken::LocalLabelDef { .. } => TokenType::LocalLabelDef,
            ValueToken::LocalLabelRef { .. } => TokenType::LocalLabelRef,
            ValueToken::Operator { .. } => TokenType::Operator,
        }
    }

    fn inner(&self) -> &InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                &inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::GlobalLabelDef { inner, .. } |
            ValueToken::LocalLabelDef { inner, .. } |
            ValueToken::LocalLabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                &inner
            }
        }
    }

    fn inner_mut(&mut self) -> &mut InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::GlobalLabelDef { inner, .. } |
            ValueToken::LocalLabelDef { inner, .. } |
            ValueToken::LocalLabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::GlobalLabelDef { inner, .. } |
            ValueToken::LocalLabelDef { inner, .. } |
            ValueToken::LocalLabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

}

#[derive(Debug, Eq, PartialEq)]
pub enum Operator {
    ShiftRight,
    ShiftLeft,
    LogicalAnd,
    LogicalOr,
    Equals,
    Unequals,
    GreaterThanEqual,
    LessThanEqual,
    Pow,
    DivInt,
    LessThan,
    GreaterThan,
    LogicalNot,
    Plus,
    Minus,
    Mul,
    Div,
    Modulo,
    BitAnd,
    BitOr,
    BitNegate,
    BitXor,
}

// Value Level Lexer Implementation -------------------------------------------
pub struct ValueLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<ValueToken>
}

impl ValueLexer {

    pub fn try_from(lexer: MacroLexer) -> Result<ValueLexer, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let tokens = Self::from_tokens(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        Ok(Self {
            files,
            tokens
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    fn from_tokens(tokens: Vec<MacroToken>) -> Result<Vec<ValueToken>, LexerError> {

        let mut global_labels: HashSet<(String, Option<usize>)> = HashSet::new();
        let mut global_labels_names: Vec<String> = Vec::new();
        let mut local_labels: HashSet<String> = HashSet::new();

        let mut value_tokens = Vec::new();
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            let value_token = match token {

                // Pass through
                MacroToken::Reserved(inner) => ValueToken::Reserved(inner),
                MacroToken::Instruction(inner) => ValueToken::Instruction(inner),
                MacroToken::BinaryFile(inner, bytes) => ValueToken::BinaryFile(inner, bytes),
                MacroToken::Comma(inner) => ValueToken::Comma(inner),
                MacroToken::OpenParen(inner) => ValueToken::OpenParen(inner),
                MacroToken::CloseParen(inner) => ValueToken::CloseParen(inner),
                MacroToken::OpenBracket(inner) => ValueToken::OpenBracket(inner),
                MacroToken::CloseBracket(inner)=> ValueToken::CloseBracket(inner),

                // Drop Comments
                MacroToken::Comment(_) => continue,

                // Values
                MacroToken::BuiltinCall(inner, args) => {
                    let mut value_args = Vec::new();
                    for tokens in args {
                        value_args.push(Self::from_tokens(tokens)?);
                    }
                    ValueToken::BuiltinCall(inner, value_args)
                },
                MacroToken::Name(mut inner) => {
                    if tokens.peek(TokenType::Colon, None) {
                        let colon = tokens.expect(TokenType::Colon, None, "when parsing global label definition")?.into_inner();

                        // Postfix labels created by macros calls so they are unique
                        let name = if let Some(call_id) = inner.macro_call_id() {
                            // TODO the old implementation did not correct global label jump targets
                            // created by macros, can the new one somehow achieve this?
                            format!("{}_from_macro_call_{}", inner.value, call_id)

                        } else {
                            inner.value.to_string()
                        };

                        // Handle file local global labels that are prefixed with _
                        let label_id = if name.starts_with("_") {
                            (format!("{}_file_local_{}", name, inner.file_index), Some(inner.file_index))

                        } else {
                            (name.clone(), None)
                        };

                        if global_labels.contains(&label_id) {
                            // TODO add information about previous label definition / location
                            return Err(inner.error(format!(
                                "Duplicate definition of global label \"{}\".",
                                name
                            )));

                        } else {
                            global_labels.insert(label_id.clone());
                            global_labels_names.push(label_id.0.clone());
                            local_labels.clear();

                            inner.end_index = colon.end_index;
                            ValueToken::GlobalLabelDef {
                                inner,
                                name: label_id.0
                            }
                        }

                    } else {
                        ValueToken::Name(inner)
                    }
                },
                MacroToken::Point(mut inner) => {

                    // For local labels all kinds of names are allowed
                    let name_token = if tokens.peek(TokenType::Instruction, None) {
                        tokens.expect(TokenType::Instruction, None, "when parsing local label")?.into_inner()

                    } else if tokens.peek(TokenType::Reserved, None) {
                        tokens.expect(TokenType::Reserved, None, "when parsing local label")?.into_inner()

                    } else {
                        tokens.expect(TokenType::Name, None, "when parsing local label")?.into_inner()
                    };

                    // Postfix labels created by macros calls so they are unique
                    let name = if let Some(call_id) = name_token.macro_call_id() {
                        format!("{}_from_macro_call_{}", name_token.value, call_id)

                    } else {
                        name_token.value.to_string()
                    };

                    if tokens.peek(TokenType::Colon, None) {
                        let colon = tokens.expect(TokenType::Colon, None, "when parsing local label definition")?.into_inner();
                        if global_labels.is_empty() {
                            return Err(inner.error(format!(
                                "Unexpected definition of local label \"{}\" before any global label was defined.",
                                name_token.value
                            )));

                        } else if local_labels.contains(&name) {
                            // TODO add information about previous label definition / location
                            return Err(inner.error(format!(
                                "Duplicate definition of local label \"{}\", label was already defined under the current global label \"{}\".",
                                name,
                                global_labels_names.last().unwrap()
                            )));

                        } else {
                            local_labels.insert(name.clone());

                            inner.end_index = colon.end_index;
                            ValueToken::LocalLabelDef {
                                inner,
                                name
                            }
                        }

                    } else if global_labels.is_empty() {
                        return Err(inner.error(format!(
                            "Unexpected reference to local label \"{}\" before any global label was defined.",
                            name_token.value
                        )));

                    } else {
                        inner.end_index = name_token.end_index;
                        ValueToken::LocalLabelRef {
                            inner,
                            name
                        }
                    }
                },
                MacroToken::Offset(inner) => ValueToken::Offset {
                    value: Self::parse_integer(&inner, 0, 10)?,
                    inner
                },
                MacroToken::NumberLiteral(inner) => {
                    match inner.value.chars().next().unwrap() {
                        '$' => ValueToken::Integer {
                            value: Self::parse_integer(&inner, 1, 16)?,
                            inner
                        },
                        '%' => ValueToken::Integer {
                            value: Self::parse_integer(&inner, 1, 2)?,
                            inner
                        },
                        _ => if inner.value.contains(".") {
                            ValueToken::Float {
                                value: OrderedFloat::from(Self::parse_float(&inner)?),
                                inner
                            }

                        } else {
                            ValueToken::Integer {
                                value: Self::parse_integer(&inner, 0, 10)?,
                                inner
                            }
                        }
                    }
                },
                MacroToken::StringLiteral(inner) => ValueToken::String {
                    value: inner.value.clone(),
                    inner
                },
                MacroToken::Colon(inner) => {
                    return Err(inner.error(format!("Unexpected standalone \"{}\", expected a \"Name\" token to preceed it.", inner.value)))
                },
                MacroToken::Operator(mut inner) => {
                    let typ = if tokens.peek(TokenType::Operator, None) {
                        let second = tokens.expect(TokenType::Operator, None, "when parsing operator")?.into_inner();
                        inner.end_index = second.end_index;
                        match (inner.value.chars().next().unwrap(), second.value.chars().next().unwrap()) {
                            ('>', '>') => Operator::ShiftRight,
                            ('<', '<') => Operator::ShiftLeft,
                            ('&', '&') => Operator::LogicalAnd,
                            ('|', '|') => Operator::LogicalOr,
                            ('=', '=') => Operator::Equals,
                            ('!', '=') => Operator::Unequals,
                            ('>', '=') => Operator::GreaterThanEqual,
                            ('<', '=') => Operator::LessThanEqual,
                            ('*', '*') => Operator::Pow,
                            ('/', '/') => Operator::DivInt,
                            _ => {
                                return Err(inner.error(format!("Unknown operator \"{}{}\".", inner.value, second.value)));
                            }
                        }

                    } else {
                        match inner.value.chars().next().unwrap() {
                            '<' => Operator::LessThan,
                            '>' => Operator::GreaterThan,
                            '!' => Operator::LogicalNot,
                            '+' => Operator::Plus,
                            '-' => Operator::Minus,
                            '*' => Operator::Mul,
                            '/' => Operator::Div,
                            '%' => Operator::Modulo,
                            '&' => Operator::BitAnd,
                            '|' => Operator::BitOr,
                            '~' => Operator::BitNegate,
                            '^' => Operator::BitXor,
                            _ => unreachable!()
                        }
                    };
                    ValueToken::Operator {
                        inner,
                        typ
                    }
                }
            };
            value_tokens.push(value_token);
        }

        // TODO convert Names that match a global label def into GlobalLabelRef
            // TODO if prefixed with _ match by file_index

        Ok(value_tokens)
    }

    fn parse_integer(inner: &InnerToken, from: usize, radix: u32) -> Result<i32, LexerError> {
        i32::from_str_radix(&inner.value[from..], radix).map_err(|_| {
            inner.error("Failed to parse integer value.".to_string())
        })
    }

    fn parse_float(inner: &InnerToken) -> Result<f32, LexerError> {
        inner.value.parse::<f32>().map_err(|_| {
            inner.error("Failed to parse float value.".to_string())
        })
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use super::{ValueLexer, ValueToken, InnerToken, Operator};
    use crate::lexer::mocks::{macro_lex, macro_lex_child};

    fn value_lexer<S: Into<String>>(s: S) -> ValueLexer {
        ValueLexer::try_from(macro_lex(s)).expect("ValueLexer failed")
    }

    fn value_lexer_child<S: Into<String>>(s: S, c: S) -> ValueLexer {
        ValueLexer::try_from(macro_lex_child(s, c)).expect("ValueLexer failed")
    }

    fn value_lexer_error<S: Into<String>>(s: S) -> String {
        ValueLexer::try_from(macro_lex(s)).err().unwrap().to_string()
    }

    fn value_lexer_child_error<S: Into<String>>(s: S, c: S) -> String {
        ValueLexer::try_from(macro_lex_child(s, c)).err().unwrap().to_string()
    }

    fn tfv<S: Into<String>>(s: S) -> Vec<ValueToken> {
        value_lexer(s).tokens
    }

    macro_rules! itf {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr, $file:expr) => {
            InnerToken::new($file, $start, $end, $raw.into(), $parsed.into())
        }
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $raw.into(), $parsed.into())
        }
    }

    macro_rules! itkm {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $raw, $parsed);
                t.set_macro_call_id($id);
                t
            }
        }
    }

    macro_rules! vtk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            ValueToken::$tok(itk!($start, $end, $raw, $parsed))
        }
    }

    macro_rules! vtko {
        ($typ:path, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            ValueToken::Operator {
                inner: itk!($start, $end, $raw, $parsed),
                typ: $typ
            }
        }
    }

    // Value Parsing ----------------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfv(""), vec![]);
    }

    #[test]
    fn test_strip_comments() {
        assert_eq!(tfv("; Foo"), vec![]);
    }

    #[test]
    fn test_passthrough() {
        assert_eq!(tfv("name\nDS\ncp\n,()[]"), vec![
            vtk!(Name, 0, 4, "name", "name"),
            vtk!(Reserved, 5, 7, "DS", "DS"),
            vtk!(Instruction, 8, 10, "cp", "cp"),
            vtk!(Comma, 11, 12, ",", ","),
            vtk!(OpenParen, 12, 13, "(", "("),
            vtk!(CloseParen, 13, 14, ")", ")"),
            vtk!(OpenBracket, 14, 15, "[", "["),
            vtk!(CloseBracket, 15, 16, "]", "]"),
        ]);
    }

    #[test]
    fn test_offset() {
        assert_eq!(tfv("@+4\n@-4"), vec![
            ValueToken::Offset {
                inner: itk!(0, 3, "@+4", "+4"),
                value: 4
            },
            ValueToken::Offset {
                inner: itk!(4, 7, "@-4", "-4"),
                value: -4
            }
        ]);
    }

    #[test]
    fn test_number() {
        assert_eq!(tfv("123\n-123\n%0000_1010\n$80\n1.24\n-2.48"), vec![
            ValueToken::Integer {
                inner: itk!(0, 3, "123", "123"),
                value: 123
            },
            ValueToken::Integer {
                inner: itk!(4, 8, "-123", "-123"),
                value: -123
            },
            ValueToken::Integer {
                inner: itk!(9, 19, "%0000_1010", "%00001010"),
                value: 10
            },
            ValueToken::Integer {
                inner: itk!(20, 23, "$80", "$80"),
                value: 128
            },
            ValueToken::Float {
                inner: itk!(24, 28, "1.24", "1.24"),
                value: OrderedFloat::from(1.24)
            },
            ValueToken::Float {
                inner: itk!(29, 34, "-2.48", "-2.48"),
                value: OrderedFloat::from(-2.48)
            }
        ]);
    }

    #[test]
    fn test_string() {
        assert_eq!(tfv("'Hello World'\n\"Hello World\""), vec![
            ValueToken::String {
                inner: itk!(0, 13, "'Hello World'", "Hello World"),
                value: "Hello World".to_string()
            },
            ValueToken::String {
                inner: itk!(14, 27, "\"Hello World\"", "Hello World"),
                value: "Hello World".to_string()
            }
        ]);
    }

    #[test]
    fn test_builtin_call() {
        assert_eq!(tfv("FLOOR(4.2)"), vec![
           ValueToken::BuiltinCall(
                itk!(0, 5, "FLOOR", "FLOOR"),
                vec![vec![
                    ValueToken::Float {
                        inner: itk!(6, 9, "4.2", "4.2"),
                        value: OrderedFloat::from(4.2)
                    }
                ]]
           )
        ]);
    }

    #[test]
    fn test_global_label_def() {
        assert_eq!(tfv("global_label:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label", "global_label"),
            name: "global_label".to_string()
        }]);
    }

    #[test]
    fn test_global_file_local_label_def() {
        let tokens = value_lexer_child(
            "_global_file_local_label:\nINCLUDE 'child.gb.s'",
            "_global_file_local_label:"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", "_global_file_local_label", 0),
            name: "_global_file_local_label_file_local_0".to_string()

        }, ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", "_global_file_local_label", 1),
            name: "_global_file_local_label_file_local_1".to_string()
        }]);
    }

    #[test]
    fn test_global_label_def_duplicate() {
        assert_eq!(value_lexer_error("global_label:\nglobal_label:"), "In file \"main.gb.s\" on line 2, column 1: Duplicate definition of global label \"global_label\".\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_global_file_local_label_def_duplicate() {
        assert_eq!(value_lexer_error("_global_file_local_label:\n_global_file_local_label:"), "In file \"main.gb.s\" on line 2, column 1: Duplicate definition of global label \"_global_file_local_label\".\n\n_global_file_local_label:\n^--- Here");
    }

    #[test]
    fn test_global_label_def_duplicate_child() {
        assert_eq!(value_lexer_child_error(
            "global_label:\nINCLUDE 'child.gb.s'",
            "global_label:"

        ), "In file \"child.gb.s\" on line 1, column 1: Duplicate definition of global label \"global_label\".\n\nglobal_label:\n^--- Here\n\nincluded from file \"main.gb.s\" on line 2, column 9");
    }

    #[test]
    fn test_local_label_def() {
        assert_eq!(tfv("global_label:\n.local_label:\n.local_other_label:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label", "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 27, ".", "."),
            name: "local_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(28, 47, ".", "."),
            name: "local_other_label".to_string()
        }]);
    }

    #[test]
    fn test_local_label_def_instruction() {
        assert_eq!(tfv("global_label:\n.stop:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label", "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 20, ".", "."),
            name: "stop".to_string()
        }]);
    }

    #[test]
    fn test_local_label_def_reserved() {
        assert_eq!(tfv("global_label:\n.DS:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label", "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 18, ".", "."),
            name: "DS".to_string()
        }]);
    }

    #[test]
    fn test_local_label_ref() {
        assert_eq!(tfv("global_label:\n.local_label\n.local_other_label"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label", "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelRef {
            inner: itk!(14, 26, ".", "."),
            name: "local_label".to_string()

        }, ValueToken::LocalLabelRef {
            inner: itk!(27, 45, ".", "."),
            name: "local_other_label".to_string()
        }]);
    }

    #[test]
    fn test_label_macro_postfix() {
        assert_eq!(tfv("FOO() FOO() MACRO FOO()\nmacro_label_def:\n.local_macro_label_def:\n.local_macro_label_ref\nENDMACRO"), vec![
            ValueToken::GlobalLabelDef {
                inner: itkm!(24, 40, "macro_label_def", "macro_label_def", 0),
                name: "macro_label_def_from_macro_call_0".to_string()
            },
            ValueToken::LocalLabelDef {
                inner: itkm!(41, 64, ".", ".", 0),
                name: "local_macro_label_def_from_macro_call_0".to_string()
            },
            ValueToken::LocalLabelRef {
                inner: itkm!(65, 87, ".", ".", 0),
                name: "local_macro_label_ref_from_macro_call_0".to_string()
            },
            ValueToken::GlobalLabelDef {
                inner: itkm!(24, 40, "macro_label_def", "macro_label_def", 1),
                name: "macro_label_def_from_macro_call_1".to_string()
            },
            ValueToken::LocalLabelDef {
                inner: itkm!(41, 64, ".", ".", 1),
                name: "local_macro_label_def_from_macro_call_1".to_string()
            },
            ValueToken::LocalLabelRef {
                inner: itkm!(65, 87, ".", ".", 1),
                name: "local_macro_label_ref_from_macro_call_1".to_string()
            }
        ]);
    }

    #[test]
    fn test_error_local_label_def_outside_global() {
        assert_eq!(value_lexer_error(".local_label:"), "In file \"main.gb.s\" on line 1, column 1: Unexpected definition of local label \"local_label\" before any global label was defined.\n\n.local_label:\n^--- Here");
    }

    #[test]
    fn test_error_local_label_ref_outside_global() {
        assert_eq!(value_lexer_error(".local_label"), "In file \"main.gb.s\" on line 1, column 1: Unexpected reference to local label \"local_label\" before any global label was defined.\n\n.local_label\n^--- Here");
    }

    #[test]
    fn test_error_local_label_def_duplicate() {
        assert_eq!(value_lexer_error("global_label:\n.local_label:\n.local_label:"), "In file \"main.gb.s\" on line 3, column 1: Duplicate definition of local label \"local_label\", label was already defined under the current global label \"global_label\".\n\n.local_label:\n^--- Here");
    }

    #[test]
    fn test_error_local_label_def() {
        assert_eq!(value_lexer_error(".4"), "In file \"main.gb.s\" on line 1, column 2: Unexpected token \"NumberLiteral\" when parsing local label, expected a \"Name\" token instead.\n\n.4\n ^--- Here");
    }

    #[test]
    fn test_operators() {
        assert_eq!(tfv(">>"), vec![vtko!(Operator::ShiftRight, 0, 2, ">", ">")]);
        assert_eq!(tfv("<<"), vec![vtko!(Operator::ShiftLeft, 0, 2, "<", "<")]);
        assert_eq!(tfv("&&"), vec![vtko!(Operator::LogicalAnd, 0, 2, "&", "&")]);
        assert_eq!(tfv("||"), vec![vtko!(Operator::LogicalOr, 0, 2, "|", "|")]);
        assert_eq!(tfv("=="), vec![vtko!(Operator::Equals, 0, 2, "=", "=")]);
        assert_eq!(tfv("!="), vec![vtko!(Operator::Unequals, 0, 2, "!", "!")]);
        assert_eq!(tfv(">="), vec![vtko!(Operator::GreaterThanEqual, 0, 2, ">", ">")]);
        assert_eq!(tfv("<="), vec![vtko!(Operator::LessThanEqual, 0, 2, "<", "<")]);
        assert_eq!(tfv("**"), vec![vtko!(Operator::Pow, 0, 2, "*", "*")]);
        assert_eq!(tfv("//"), vec![vtko!(Operator::DivInt, 0, 2, "/", "/")]);
        assert_eq!(tfv("<"), vec![vtko!(Operator::LessThan, 0, 1, "<", "<")]);
        assert_eq!(tfv(">"), vec![vtko!(Operator::GreaterThan, 0, 1, ">", ">")]);
        assert_eq!(tfv("!"), vec![vtko!(Operator::LogicalNot, 0, 1, "!", "!")]);
        assert_eq!(tfv("+"), vec![vtko!(Operator::Plus, 0, 1, "+", "+")]);
        assert_eq!(tfv("-"), vec![vtko!(Operator::Minus, 0, 1, "-", "-")]);
        assert_eq!(tfv("*"), vec![vtko!(Operator::Mul, 0, 1, "*", "*")]);
        assert_eq!(tfv("/"), vec![vtko!(Operator::Div, 0, 1, "/", "/")]);
        assert_eq!(tfv("%"), vec![vtko!(Operator::Modulo, 0, 1, "%", "%")]);
        assert_eq!(tfv("&"), vec![vtko!(Operator::BitAnd, 0, 1, "&", "&")]);
        assert_eq!(tfv("|"), vec![vtko!(Operator::BitOr, 0, 1, "|", "|")]);
        assert_eq!(tfv("~"), vec![vtko!(Operator::BitNegate, 0, 1, "~", "~")]);
        assert_eq!(tfv("^"), vec![vtko!(Operator::BitXor, 0, 1, "^", "^")]);
    }

    #[test]
    fn test_error_unknown_operator() {
        assert_eq!(value_lexer_error("&="), "In file \"main.gb.s\" on line 1, column 1: Unknown operator \"&=\".\n\n&=\n^--- Here");
    }

    // Value Errors -----------------------------------------------------------
    #[test]
    fn test_error_colon_standlone() {
        assert_eq!(value_lexer_error(":"), "In file \"main.gb.s\" on line 1, column 1: Unexpected standalone \":\", expected a \"Name\" token to preceed it.\n\n:\n^--- Here");
    }

}

