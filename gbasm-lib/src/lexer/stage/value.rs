// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::cpu::{Flag, Register};
use crate::lexer::MacroStage;
use super::macros::{MacroCall, MacroToken};
use super::super::{LexerStage, InnerToken, TokenIterator, TokenType, LexerToken, LexerError};


// Value Specific Tokens ------------------------------------------------------
lexer_token!(ValueToken, (Debug, Eq, PartialEq), {
    Name(()),
    Reserved(()),
    Segment(()),
    Instruction(()),
    MetaInstruction(()),
    BinaryFile((Vec<u8>)),
    Comma(()),
    OpenParen(()),
    CloseParen(()),
    OpenBracket(()),
    CloseBracket(()),
    BuiltinCall((Vec<Vec<ValueToken>>))
}, {
    Offset {
        value => i32
    },
    Float {
        value => OrderedFloat<f32>
    },
    Integer {
        value => i32
    },
    String {
        value => String
    },
    GlobalLabelDef {
        name => String
    },
    GlobalLabelRef {
        name => String
    },
    LocalLabelDef {
        name => String
    },
    LocalLabelRef {
        name => String
    },
    Operator {
        typ => Operator
    },
    Register {
        name => Register
    },
    Flag {
        typ => Flag
    }
});


// Types ----------------------------------------------------------------------
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

impl Operator {

    pub fn len(&self) -> usize {
        match self {
            Operator::ShiftRight | Operator::ShiftLeft | Operator::LogicalAnd | Operator::LogicalOr |
            Operator::Equals | Operator::Unequals | Operator::GreaterThanEqual | Operator::LessThanEqual |
            Operator::Pow | Operator::DivInt => 2,
            _ => 1
        }
    }

    pub fn associativity(&self) -> usize {
        match self {
            Operator::Pow | Operator::BitXor => 0,
            _ => 1
        }
    }

    pub fn precedence(&self) -> usize {
        match self {
            Operator::LogicalOr => 1,
            Operator::LogicalAnd => 2,
            Operator::BitOr => 3,
            Operator::BitXor => 4,
            Operator::BitAnd => 5,
            Operator::Equals | Operator::Unequals => 6,
            Operator::GreaterThanEqual | Operator::LessThanEqual | Operator::LessThan | Operator::GreaterThan => 7,
            Operator::ShiftRight | Operator::ShiftLeft => 8,
            Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate => 9,
            Operator::Mul | Operator::Div | Operator::Modulo | Operator::DivInt => 11,
            Operator::Pow => 12,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate => true,
            _ => false
        }
    }

    pub fn is_unary_exclusive(&self) -> bool {
        match self {
            Operator::LogicalNot | Operator::BitNegate => true,
            _ => false
        }
    }

}

// Value Level Lexer Implementation -------------------------------------------
pub struct ValueStage;
impl LexerStage for ValueStage {

    type Input = MacroStage;
    type Output = ValueToken;
    type Data = ();

    fn from_tokens(
        tokens: Vec<<Self::Input as LexerStage>::Output>,
        _macro_calls: &mut Vec<MacroCall>,
        _data: &mut Vec<Self::Data>

    ) -> Result<Vec<Self::Output>, LexerError> {
        let mut global_labels: HashMap<(String, Option<usize>), InnerToken> = HashMap::new();
        let mut global_labels_names: Vec<String> = Vec::new();
        Self::parse_tokens(
            &mut global_labels,
            &mut global_labels_names,
            false,
            tokens
        )
    }

}

impl ValueStage {

    fn parse_tokens(
        global_labels: &mut HashMap<(String, Option<usize>), InnerToken>,
        global_labels_names: &mut Vec<String>,
        is_argument: bool,
        tokens: Vec<MacroToken>

    ) -> Result<Vec<ValueToken>, LexerError> {

        let mut local_labels: HashMap<String, InnerToken> = HashMap::new();

        let mut value_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            let value_token = match token {

                // Pass through
                MacroToken::Reserved(inner) => ValueToken::Reserved(inner),
                MacroToken::Segment(inner) => ValueToken::Segment(inner),
                MacroToken::Instruction(inner) => ValueToken::Instruction(inner),
                MacroToken::MetaInstruction(inner) => ValueToken::MetaInstruction(inner),
                MacroToken::BinaryFile(inner, bytes) => ValueToken::BinaryFile(inner, bytes),
                MacroToken::Comma(inner) => ValueToken::Comma(inner),
                MacroToken::OpenParen(inner) => ValueToken::OpenParen(inner),
                MacroToken::CloseParen(inner) => ValueToken::CloseParen(inner),
                MacroToken::OpenBracket(inner) => ValueToken::OpenBracket(inner),
                MacroToken::CloseBracket(inner)=> ValueToken::CloseBracket(inner),

                // Registers
                MacroToken::Register(inner) => ValueToken::Register {
                    name: Register::from(inner.value.as_str()),
                    inner
                },

                // Flags
                MacroToken::Flag(inner) => ValueToken::Flag {
                    typ: Flag::from(inner.value.as_str()),
                    inner
                },

                // Drop Comments
                MacroToken::Comment(_) => continue,

                // Values
                MacroToken::BuiltinCall(inner, args) => {
                    let mut value_args = Vec::with_capacity(args.len());
                    for tokens in args {
                        value_args.push(Self::parse_tokens(
                            global_labels,
                            global_labels_names,
                            true,
                            tokens
                        )?);
                    }
                    ValueToken::BuiltinCall(inner, value_args)
                },
                MacroToken::Name(mut inner) => {
                    if tokens.peek_is(TokenType::Colon, None) {
                        let colon = tokens.expect(TokenType::Colon, None, "when parsing global label definition")?.into_inner();
                        let label_id = Self::global_label_id(&inner);
                        if let Some(previous) = global_labels.get(&label_id) {
                            return Err(inner.error(format!(
                                "Global label \"{}\" was already defined.",
                                inner.value

                            )).with_reference(previous, "Original definition of global label was"));

                        } else if is_argument {
                            return Err(inner.error("Global label cannot be defined inside an argument list".to_string()));

                        } else {
                            inner.end_index = colon.end_index;
                            global_labels.insert(label_id.clone(), inner.clone());
                            global_labels_names.push(label_id.0.clone());
                            local_labels.clear();

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
                    let name_token = if tokens.peek_is(TokenType::Instruction, None) {
                        tokens.expect(TokenType::Instruction, None, "when parsing local label")?.into_inner()

                    } else if tokens.peek_is(TokenType::Reserved, None) {
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

                    if tokens.peek_is(TokenType::Colon, None) {
                        let colon = tokens.expect(TokenType::Colon, None, "when parsing local label definition")?.into_inner();
                        if global_labels.is_empty() {
                            return Err(inner.error(format!(
                                "Unexpected definition of local label \"{}\" before any global label was defined.",
                                name_token.value
                            )));

                        } else if let Some(previous) = local_labels.get(&name) {
                            return Err(inner.error(format!(
                                "Local label \"{}\" was already defined under the current global label \"{}\".",
                                name,
                                global_labels_names.last().unwrap()

                            )).with_reference(previous, "Original definition of local label was"));

                        } else if is_argument {
                            return Err(inner.error("Local label cannot be defined inside an argument list".to_string()));

                        } else {
                            inner.end_index = colon.end_index;
                            local_labels.insert(name.clone(), inner.clone());

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
                    let typ = if tokens.peek_is(TokenType::Operator, None) {
                        match Self::parse_operator_double(&inner, tokens.peek().unwrap().inner()) {
                            Some(typ) => {
                                tokens.expect(TokenType::Operator, None, "when parsing operator")?;
                                typ
                            },
                            None => Self::parse_operator_single(&inner)?
                        }

                    } else {
                        Self::parse_operator_single(&inner)?
                    };
                    inner.end_index = inner.start_index + typ.len();
                    ValueToken::Operator {
                        inner,
                        typ
                    }
                }
            };
            value_tokens.push(value_token);
        }

        // Verify and build label references
        Self::verify_local_label_refs(&value_tokens, None)?;
        Ok(Self::convert_global_label_refs(&global_labels, value_tokens))
    }

    fn verify_local_label_refs<'a>(
        tokens: &'a [ValueToken],
        mut global_label: Option<(&'a InnerToken, &'a String, Vec<&'a String>, Vec<(&'a InnerToken, &'a String)>)>

    ) -> Result<(), LexerError> {
        for token in tokens {
            match token {
                ValueToken::GlobalLabelDef { inner, name } => {
                    Self::verify_local_label_refs_under_global(global_label.take())?;
                    global_label = Some((inner, name, Vec::new(), Vec::new()));
                },
                ValueToken::LocalLabelDef { name, .. } => {
                    if let Some(global_label) = global_label.as_mut() {
                        global_label.2.push(&name);
                    }
                },
                ValueToken::LocalLabelRef { inner, name } => {
                    if let Some(global_label) = global_label.as_mut() {
                        global_label.3.push((&inner, &name));
                    }
                },
                ValueToken::BuiltinCall(_, arguments) => {
                    for tokens in arguments {
                        Self::verify_local_label_refs(tokens, global_label.clone())?;
                    }
                },
                _ => {}
            }
        }
        Self::verify_local_label_refs_under_global(global_label.take())
    }

    fn verify_local_label_refs_under_global(global_label: Option<(&InnerToken, &String, Vec<&String>, Vec<(&InnerToken, &String)>)>) -> Result<(), LexerError> {

        // Check local labels under previous label
        if let Some((previous, parent_name, local_defs, local_refs)) = global_label {
            for (inner, name) in &local_refs {
                if !local_defs.contains(name) {
                    return Err(inner.error(format!(
                        "Reference to unknown local label \"{}\", not defined under the current global label \"{}\".",
                        name,
                        parent_name

                    )).with_reference(previous, "Definition of global label was"));
                }
            }
        }

        Ok(())
    }

    fn convert_global_label_refs(
        global_labels: &HashMap<(String, Option<usize>), InnerToken>,
        tokens: Vec<ValueToken>

    ) -> Vec<ValueToken> {
        tokens.into_iter().map(|token| {
            if let ValueToken::Name(inner) = token {

                // Generate references to global labels
                let label_id = Self::global_label_id(&inner);
                if global_labels.contains_key(&label_id) {
                    ValueToken::GlobalLabelRef {
                        inner,
                        name: label_id.0
                    }

                } else {
                    ValueToken::Name(inner)
                }

            } else if let ValueToken::BuiltinCall(inner, arguments) = token {
                ValueToken::BuiltinCall(inner, arguments.into_iter().map(|tokens| {
                    Self::convert_global_label_refs(global_labels, tokens)

                }).collect())

            } else {
                token
            }

        }).collect()
    }

    fn global_label_id(inner: &InnerToken) -> (String, Option<usize>) {
        let name = if let Some(call_id) = inner.macro_call_id() {
            // Postfix labels created by macros calls so they are unique
            format!("{}_from_macro_call_{}", inner.value, call_id)

        } else {
            inner.value.to_string()
        };

        if name.starts_with("_") {
            // Handle file local global labels that are prefixed with _
            (format!("{}_file_local_{}", name, inner.file_index), Some(inner.file_index))

        } else {
            (name, None)
        }
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

    fn parse_operator_single(inner: &InnerToken) -> Result<Operator, LexerError> {
        match inner.value.chars().next().unwrap() {
            '<' => Ok(Operator::LessThan),
            '>' => Ok(Operator::GreaterThan),
            '!' => Ok(Operator::LogicalNot),
            '+' => Ok(Operator::Plus),
            '-' => Ok(Operator::Minus),
            '*' => Ok(Operator::Mul),
            '/' => Ok(Operator::Div),
            '%' => Ok(Operator::Modulo),
            '&' => Ok(Operator::BitAnd),
            '|' => Ok(Operator::BitOr),
            '~' => Ok(Operator::BitNegate),
            '^' => Ok(Operator::BitXor),
            _ => Err(inner.error(format!("Unknown operator \"{}\".", inner.value)))
        }
    }

    fn parse_operator_double(first: &InnerToken, second: &InnerToken) -> Option<Operator> {
        match (
            first.value.chars().next().unwrap(),
            second.value.chars().next().unwrap()
        ) {
            ('>', '>') => Some(Operator::ShiftRight),
            ('<', '<') => Some(Operator::ShiftLeft),
            ('&', '&') => Some(Operator::LogicalAnd),
            ('|', '|') => Some(Operator::LogicalOr),
            ('=', '=') => Some(Operator::Equals),
            ('!', '=') => Some(Operator::Unequals),
            ('>', '=') => Some(Operator::GreaterThanEqual),
            ('<', '=') => Some(Operator::LessThanEqual),
            ('*', '*') => Some(Operator::Pow),
            ('/', '/') => Some(Operator::DivInt),
            (_, _) => None
        }
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use crate::lexer::Lexer;
    use super::{ValueStage, ValueToken, InnerToken, Operator, Register, Flag};
    use super::super::mocks::{macro_lex, macro_lex_child};

    fn value_lexer<S: Into<String>>(s: S) -> Lexer<ValueStage> {
        Lexer::<ValueStage>::from_lexer(macro_lex(s)).expect("ValueLexer failed")
    }

    fn value_lexer_child<S: Into<String>>(s: S, c: S) -> Lexer<ValueStage> {
        Lexer::<ValueStage>::from_lexer(macro_lex_child(s, c)).expect("ValueLexer failed")
    }

    fn value_lexer_error<S: Into<String>>(s: S) -> String {
        Lexer::<ValueStage>::from_lexer(macro_lex(s)).err().unwrap().to_string()
    }

    fn value_lexer_child_error<S: Into<String>>(s: S, c: S) -> String {
        Lexer::<ValueStage>::from_lexer(macro_lex_child(s, c)).err().unwrap().to_string()
    }

    fn tfv<S: Into<String>>(s: S) -> Vec<ValueToken> {
        value_lexer(s).tokens
    }

    macro_rules! itf {
        ($start:expr, $end:expr, $parsed:expr, $file:expr) => {
            InnerToken::new($file, $start, $end, $parsed.into())
        }
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    macro_rules! itkm {
        ($start:expr, $end:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $parsed);
                t.set_macro_call_id($id);
                t
            }
        }
    }

    macro_rules! vtk {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            ValueToken::$tok(itk!($start, $end, $parsed))
        }
    }

    macro_rules! vtko {
        ($typ:path, $start:expr, $end:expr, $parsed:expr) => {
            ValueToken::Operator {
                inner: itk!($start, $end, $parsed),
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
            vtk!(Name, 0, 4, "name"),
            vtk!(Reserved, 5, 7, "DS"),
            vtk!(Instruction, 8, 10, "cp"),
            vtk!(Comma, 11, 12, ","),
            vtk!(OpenParen, 12, 13, "("),
            vtk!(CloseParen, 13, 14, ")"),
            vtk!(OpenBracket, 14, 15, "["),
            vtk!(CloseBracket, 15, 16, "]"),
        ]);
    }

    #[test]
    fn test_offset() {
        assert_eq!(tfv("@+4\n@-4"), vec![
            ValueToken::Offset {
                inner: itk!(0, 3, "+4"),
                value: 4
            },
            ValueToken::Offset {
                inner: itk!(4, 7, "-4"),
                value: -4
            }
        ]);
    }

    #[test]
    fn test_number() {
        assert_eq!(tfv("123\n-123\n%0000_1010\n$80\n1.24\n-2.48"), vec![
            ValueToken::Integer {
                inner: itk!(0, 3, "123"),
                value: 123
            },
            ValueToken::Integer {
                inner: itk!(4, 8, "-123"),
                value: -123
            },
            ValueToken::Integer {
                inner: itk!(9, 19, "%00001010"),
                value: 10
            },
            ValueToken::Integer {
                inner: itk!(20, 23, "$80"),
                value: 128
            },
            ValueToken::Float {
                inner: itk!(24, 28, "1.24"),
                value: OrderedFloat::from(1.24)
            },
            ValueToken::Float {
                inner: itk!(29, 34, "-2.48"),
                value: OrderedFloat::from(-2.48)
            }
        ]);
    }

    #[test]
    fn test_string() {
        assert_eq!(tfv("'Hello World'\n\"Hello World\""), vec![
            ValueToken::String {
                inner: itk!(0, 13, "Hello World"),
                value: "Hello World".to_string()
            },
            ValueToken::String {
                inner: itk!(14, 27, "Hello World"),
                value: "Hello World".to_string()
            }
        ]);
    }

    #[test]
    fn test_builtin_call() {
        assert_eq!(tfv("FLOOR(4.2)"), vec![
           ValueToken::BuiltinCall(
                itk!(0, 5, "FLOOR"),
                vec![vec![
                    ValueToken::Float {
                        inner: itk!(6, 9, "4.2"),
                        value: OrderedFloat::from(4.2)
                    }
                ]]
           )
        ]);
    }

    #[test]
    fn test_global_label_def() {
        assert_eq!(tfv("global_label:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()
        }]);
    }

    #[test]
    fn test_global_label_ref() {
        assert_eq!(tfv("global_label:\nglobal_label"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::GlobalLabelRef {
            inner: itf!(14, 26, "global_label", 0),
            name: "global_label".to_string()
        }]);
        assert_eq!(tfv("global_label:\nCEIL(global_label)"), vec![
            ValueToken::GlobalLabelDef {
                inner: itk!(0, 13, "global_label"),
                name: "global_label".to_string()

            },
            ValueToken::BuiltinCall(
                itk!(14, 18, "CEIL"),
                vec![
                    vec![
                        ValueToken::GlobalLabelRef {
                            inner: itf!(19, 31, "global_label", 0),
                            name: "global_label".to_string()
                        }
                    ]
                ]
            )
        ]);
    }

    #[test]
    fn test_global_label_no_ref() {
        assert_eq!(tfv("global_label"), vec![ValueToken::Name(
            itf!(0, 12, "global_label", 0)
        )]);
    }

    #[test]
    fn test_global_file_local_label_def() {
        let tokens = value_lexer_child(
            "_global_file_local_label:\nINCLUDE 'child.gb.s'",
            "_global_file_local_label:"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", 0),
            name: "_global_file_local_label_file_local_0".to_string()

        }, ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", 1),
            name: "_global_file_local_label_file_local_1".to_string()
        }]);
    }

    #[test]
    fn test_global_file_local_label_ref() {
        let tokens = value_lexer_child(
            "_global_file_local_label:\n_global_file_local_label\nINCLUDE 'child.gb.s'",
            "_global_file_local_label:\n_global_file_local_label"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", 0),
            name: "_global_file_local_label_file_local_0".to_string()

        }, ValueToken::GlobalLabelRef {
            inner: itf!(26, 50, "_global_file_local_label", 0),
            name: "_global_file_local_label_file_local_0".to_string()

        }, ValueToken::GlobalLabelDef {
            inner: itf!(0, 25, "_global_file_local_label", 1),
            name: "_global_file_local_label_file_local_1".to_string()

        }, ValueToken::GlobalLabelRef {
            inner: itf!(26, 50, "_global_file_local_label", 1),
            name: "_global_file_local_label_file_local_1".to_string()
        }]);
    }

    #[test]
    fn test_global_file_local_label_no_ref() {
        let tokens = value_lexer_child(
            "_global_file_local_label\nINCLUDE 'child.gb.s'",
            "_global_file_local_label"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::Name(
            itf!(0, 24, "_global_file_local_label", 0)

        ), ValueToken::Name(
            itf!(0, 24, "_global_file_local_label", 1)
        )]);
    }

    #[test]
    fn test_error_global_label_def_duplicate() {
        assert_eq!(value_lexer_error(
            "global_label:\nglobal_label:"

        ), "In file \"main.gb.s\" on line 2, column 1: Global label \"global_label\" was already defined.\n\nglobal_label:\n^--- Here\n\nOriginal definition of global label was in file \"main.gb.s\" on line 1, column 1:\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_error_global_file_local_label_def_duplicate() {
        assert_eq!(
            value_lexer_error("_global_file_local_label:\n_global_file_local_label:"),
            "In file \"main.gb.s\" on line 2, column 1: Global label \"_global_file_local_label\" was already defined.\n\n_global_file_local_label:\n^--- Here\n\nOriginal definition of global label was in file \"main.gb.s\" on line 1, column 1:\n\n_global_file_local_label:\n^--- Here"
        );
    }

    #[test]
    fn test_error_global_label_def_duplicate_child() {
        assert_eq!(value_lexer_child_error(
            "global_label:\nINCLUDE 'child.gb.s'",
            "global_label:"

        ), "In file \"child.gb.s\" on line 1, column 1: Global label \"global_label\" was already defined.\n\nglobal_label:\n^--- Here\n\nincluded from file \"main.gb.s\" on line 2, column 9\n\nOriginal definition of global label was in file \"main.gb.s\" on line 1, column 1:\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_error_global_label_def_in_call() {
        assert_eq!(value_lexer_error(
            "CEIL(global_label:)"

        ), "In file \"main.gb.s\" on line 1, column 6: Global label cannot be defined inside an argument list\n\nCEIL(global_label:)\n     ^--- Here");
    }

    #[test]
    fn test_error_local_label_def_in_call() {
        assert_eq!(value_lexer_error(
            "global_label:\nCEIL(.locall_label:)"

        ), "In file \"main.gb.s\" on line 2, column 6: Local label cannot be defined inside an argument list\n\nCEIL(.locall_label:)\n     ^--- Here");
    }

    #[test]
    fn test_local_label_def() {
        assert_eq!(tfv("global_label:\n.local_label:\n.local_other_label:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 27, "."),
            name: "local_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(28, 47, "."),
            name: "local_other_label".to_string()
        }]);
    }

    #[test]
    fn test_local_label_def_instruction() {
        assert_eq!(tfv("global_label:\n.stop:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 20, "."),
            name: "stop".to_string()
        }]);
    }

    #[test]
    fn test_local_label_def_reserved() {
        assert_eq!(tfv("global_label:\n.DS:"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 18, "."),
            name: "DS".to_string()
        }]);
    }

    #[test]
    fn test_local_label_ref() {
        assert_eq!(tfv("global_label:\n.local_label:\n.local_label"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 27, "."),
            name: "local_label".to_string()

        }, ValueToken::LocalLabelRef {
            inner: itk!(28, 40, "."),
            name: "local_label".to_string()
        }]);
        assert_eq!(tfv("global_label:\n.local_label:\nCEIL(.local_label)"), vec![ValueToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, ValueToken::LocalLabelDef {
            inner: itk!(14, 27, "."),
            name: "local_label".to_string()

        }, ValueToken::BuiltinCall(
            itk!(28, 32, "CEIL"),
            vec![
                vec![
                    ValueToken::LocalLabelRef {
                        inner: itf!(33, 45, ".", 0),
                        name: "local_label".to_string()
                    }
                ]
            ]
        )]);

    }

    #[test]
    fn test_label_macro_postfix() {
        assert_eq!(tfv("FOO() FOO() MACRO FOO()\nmacro_label_def:\n.local_macro_label_def:\n.local_macro_label_def\nENDMACRO"), vec![
            ValueToken::GlobalLabelDef {
                inner: itkm!(24, 40, "macro_label_def", 0),
                name: "macro_label_def_from_macro_call_0".to_string()
            },
            ValueToken::LocalLabelDef {
                inner: itkm!(41, 64, ".", 0),
                name: "local_macro_label_def_from_macro_call_0".to_string()
            },
            ValueToken::LocalLabelRef {
                inner: itkm!(65, 87, ".", 0),
                name: "local_macro_label_def_from_macro_call_0".to_string()
            },
            ValueToken::GlobalLabelDef {
                inner: itkm!(24, 40, "macro_label_def", 1),
                name: "macro_label_def_from_macro_call_1".to_string()
            },
            ValueToken::LocalLabelDef {
                inner: itkm!(41, 64, ".", 1),
                name: "local_macro_label_def_from_macro_call_1".to_string()
            },
            ValueToken::LocalLabelRef {
                inner: itkm!(65, 87, ".", 1),
                name: "local_macro_label_def_from_macro_call_1".to_string()
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
    fn test_error_local_label_ref_without_def() {
        assert_eq!(value_lexer_error("global_label:\n.local_label"), "In file \"main.gb.s\" on line 2, column 1: Reference to unknown local label \"local_label\", not defined under the current global label \"global_label\".\n\n.local_label\n^--- Here\n\nDefinition of global label was in file \"main.gb.s\" on line 1, column 1:\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_error_local_label_ref_without_def_in_builtin_call() {
        assert_eq!(value_lexer_error("global_label:\nCEIL(.local_label)"), "In file \"main.gb.s\" on line 2, column 6: Reference to unknown local label \"local_label\", not defined under the current global label \"global_label\".\n\nCEIL(.local_label)\n     ^--- Here\n\nDefinition of global label was in file \"main.gb.s\" on line 1, column 1:\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_error_local_label_def_duplicate() {
        assert_eq!(
            value_lexer_error("global_label:\n.local_label:\n.local_label:"),
            "In file \"main.gb.s\" on line 3, column 1: Local label \"local_label\" was already defined under the current global label \"global_label\".\n\n.local_label:\n^--- Here\n\nOriginal definition of local label was in file \"main.gb.s\" on line 2, column 1:\n\n.local_label:\n^--- Here"
        );
    }

    #[test]
    fn test_error_local_label_def() {
        assert_eq!(value_lexer_error(".4"), "In file \"main.gb.s\" on line 1, column 2: Unexpected token \"NumberLiteral\" when parsing local label, expected a \"Name\" token instead.\n\n.4\n ^--- Here");
    }

    #[test]
    fn test_operators() {
        assert_eq!(tfv(">>"), vec![vtko!(Operator::ShiftRight, 0, 2, ">")]);
        assert_eq!(tfv("<<"), vec![vtko!(Operator::ShiftLeft, 0, 2, "<")]);
        assert_eq!(tfv("&&"), vec![vtko!(Operator::LogicalAnd, 0, 2, "&")]);
        assert_eq!(tfv("||"), vec![vtko!(Operator::LogicalOr, 0, 2, "|")]);
        assert_eq!(tfv("=="), vec![vtko!(Operator::Equals, 0, 2, "=")]);
        assert_eq!(tfv("!="), vec![vtko!(Operator::Unequals, 0, 2, "!")]);
        assert_eq!(tfv(">="), vec![vtko!(Operator::GreaterThanEqual, 0, 2, ">")]);
        assert_eq!(tfv("<="), vec![vtko!(Operator::LessThanEqual, 0, 2, "<")]);
        assert_eq!(tfv("**"), vec![vtko!(Operator::Pow, 0, 2, "*")]);
        assert_eq!(tfv("//"), vec![vtko!(Operator::DivInt, 0, 2, "/")]);
        assert_eq!(tfv("<"), vec![vtko!(Operator::LessThan, 0, 1, "<")]);
        assert_eq!(tfv(">"), vec![vtko!(Operator::GreaterThan, 0, 1, ">")]);
        assert_eq!(tfv("!"), vec![vtko!(Operator::LogicalNot, 0, 1, "!")]);
        assert_eq!(tfv("+"), vec![vtko!(Operator::Plus, 0, 1, "+")]);
        assert_eq!(tfv("-"), vec![vtko!(Operator::Minus, 0, 1, "-")]);
        assert_eq!(tfv("*"), vec![vtko!(Operator::Mul, 0, 1, "*")]);
        assert_eq!(tfv("/"), vec![vtko!(Operator::Div, 0, 1, "/")]);
        assert_eq!(tfv("%"), vec![vtko!(Operator::Modulo, 0, 1, "%")]);
        assert_eq!(tfv("&"), vec![vtko!(Operator::BitAnd, 0, 1, "&")]);
        assert_eq!(tfv("|"), vec![vtko!(Operator::BitOr, 0, 1, "|")]);
        assert_eq!(tfv("~"), vec![vtko!(Operator::BitNegate, 0, 1, "~")]);
        assert_eq!(tfv("^"), vec![vtko!(Operator::BitXor, 0, 1, "^")]);
    }

    #[test]
    fn test_operators_multiple() {
        assert_eq!(tfv("+-*%"), vec![
            vtko!(Operator::Plus, 0, 1, "+"),
            vtko!(Operator::Minus, 1, 2, "-"),
            vtko!(Operator::Mul, 2, 3, "*"),
            vtko!(Operator::Modulo, 3, 4, "%")
        ]);
    }

    #[test]
    fn test_registers() {
        let registers = vec![
            (Register::Accumulator, "a"),
            (Register::B, "b"),
            (Register::C, "c"),
            (Register::D, "d"),
            (Register::E, "e"),
            (Register::H, "h"),
            (Register::L, "l"),
            (Register::BC, "bc"),
            (Register::DE, "de"),
            (Register::HL, "hl"),
            (Register::HLIncrement, "hli"),
            (Register::HLDecrement, "hld"),
            (Register::SP, "sp")
        ];
        for (r, s) in registers {
            assert_eq!(tfv(s), vec![
                ValueToken::Register {
                    inner: itk!(0, s.len(), s),
                    name: r
                }
            ]);
        }
    }

    #[test]
    fn test_flags() {
        let flags = vec![
            (Flag::Zero, "z"),
            (Flag::NoZero, "nz"),
            (Flag::NoCarry, "nc")
        ];
        for (f, s) in flags {
            assert_eq!(tfv(s), vec![
                ValueToken::Flag {
                    inner: itk!(0, s.len(), s),
                    typ: f
                }
            ]);
        }
    }

    #[test]
    fn test_error_unknown_double_operator() {
        assert_eq!(value_lexer_error("="), "In file \"main.gb.s\" on line 1, column 1: Unknown operator \"=\".\n\n=\n^--- Here");
    }

    // Value Errors -----------------------------------------------------------
    #[test]
    fn test_error_colon_standlone() {
        assert_eq!(value_lexer_error(":"), "In file \"main.gb.s\" on line 1, column 1: Unexpected standalone \":\", expected a \"Name\" token to preceed it.\n\n:\n^--- Here");
    }

}

