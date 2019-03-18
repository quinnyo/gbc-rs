// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use gbasm_cpu::{Flag, Register};
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
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
    BuiltinCall((Vec<Vec<ValueToken>>)),
    GlobalLabelDef((usize)),
    GlobalLabelRef((usize)),
    LocalLabelDef((usize)),
    LocalLabelRef((usize))

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

#[derive(Debug, Clone)]
enum LocalLabelRef {
    Global {
        index: usize,
        target: usize
    },
    InsideCall {
        index: usize,
        arg_index: usize,
        inner_index: usize,
        target: usize
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
        let mut global_labels: HashMap<(String, Option<usize>), (InnerToken, usize)> = HashMap::new();
        let mut global_labels_names: Vec<String> = Vec::new();
        let mut unique_label_id = 0;

        let mut tokens = Self::parse_tokens(
            &mut global_labels,
            &mut global_labels_names,
            &mut unique_label_id,
            false,
            tokens
        )?;

        // Verify and assign IDs to local label references
        let mut global_label_map = HashMap::new();
        let mut local_label_map = Vec::new();
        Self::assign_and_verify_local_label_refs(
            &mut tokens,
            &mut global_label_map,
            &mut local_label_map,
            None
        )?;
        Ok(tokens)
    }

}

impl ValueStage {

    fn parse_tokens(
        global_labels: &mut HashMap<(String, Option<usize>), (InnerToken, usize)>,
        global_labels_names: &mut Vec<String>,
        unique_label_id: &mut usize,
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
                            unique_label_id,
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
                        if let Some((previous, _)) = global_labels.get(&label_id) {
                            return Err(inner.error(format!(
                                "Global label \"{}\" was already defined.",
                                inner.value

                            )).with_reference(previous, "Original definition of global label was"));

                        } else if is_argument {
                            return Err(inner.error("Global label cannot be defined inside an argument list".to_string()));

                        } else {
                            *unique_label_id += 1;
                            inner.end_index = colon.end_index;
                            global_labels.insert(label_id.clone(), (inner.clone(), *unique_label_id));
                            global_labels_names.push(label_id.0.clone());
                            local_labels.clear();

                            ValueToken::GlobalLabelDef(inner, *unique_label_id)
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
                            inner.value = name.clone();
                            local_labels.insert(name.clone(), inner.clone());

                            *unique_label_id += 1;
                            ValueToken::LocalLabelDef(inner, *unique_label_id)
                        }

                    } else if global_labels.is_empty() {
                        return Err(inner.error(format!(
                            "Unexpected reference to local label \"{}\" before any global label was defined.",
                            name_token.value
                        )));

                    } else {
                        inner.value = name.clone();
                        inner.end_index = name_token.end_index;
                        ValueToken::LocalLabelRef(inner, 0)
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

        // Convert name tokens into corresponding global label references
        Ok(Self::convert_global_label_refs(&global_labels, value_tokens))
    }

    fn assign_and_verify_local_label_refs<'a>(
        tokens: &mut [ValueToken],
        global_label_map: &mut HashMap<
            Option<usize>,
            Option<(usize, Vec<(String, usize)>, Vec<(String, usize, Option<(usize, usize)>)>)>
        >,
        local_label_refs: &mut Vec<LocalLabelRef>,
        call_parent: Option<(usize, usize)>

    ) -> Result<(), LexerError> {
        let mut error = None;
        for (index, token) in tokens.iter_mut().enumerate() {
            match token {
                ValueToken::GlobalLabelDef(inner, _) if call_parent.is_none() => {
                    let global_label = global_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Err(e) = Self::verify_local_label_refs_under_global(global_label.take(), local_label_refs) {
                        error = Some(e);
                        break;
                    }
                    *global_label = Some((index, Vec::new(), Vec::new()));
                },
                ValueToken::LocalLabelDef(inner, id) => {
                    let global_label = global_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(global_label) = global_label.as_mut() {
                        global_label.1.push((inner.value.clone(), *id));
                    }
                },
                ValueToken::LocalLabelRef(inner, _) => {
                    let global_label = global_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(global_label) = global_label.as_mut() {
                        global_label.2.push((inner.value.clone(), index, call_parent));
                    }
                },
                ValueToken::BuiltinCall(_, arguments) => {
                    for (arg_index, arg_tokens) in arguments.iter_mut().enumerate() {
                        Self::assign_and_verify_local_label_refs(
                            arg_tokens,
                            global_label_map,
                            local_label_refs,
                            Some((index, arg_index))
                        )?;
                    }
                },
                _ => {}
            }
        }

        if call_parent.is_none() {

            for (_, mut global_label) in global_label_map.drain() {
                if let Err(e) = Self::verify_local_label_refs_under_global(global_label.take(), local_label_refs) {
                    error = Some(e);
                    break;
                }
            }

            if let Some(error) = error {
                let parent = tokens[error.1].inner();
                let label = if let Some((index, arg_index)) = error.2 {
                    if let Some(ValueToken::BuiltinCall(_, ref arguments)) = tokens.get(index) {
                        arguments[arg_index][error.0].inner()

                    } else {
                        unreachable!();
                    }

                } else {
                    tokens[error.0].inner()
                };
                return Err(label.error(format!(
                    "Reference to unknown local label \"{}\", not defined under the current global label \"{}\".",
                    label.value,
                    parent.value

                )).with_reference(parent, "Definition of global label was"));
            }

            for r in local_label_refs.drain(0..) {
                match r {
                    LocalLabelRef::Global { index, target } => {
                        if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = tokens.get_mut(index) {
                            *id = target;
                            continue;
                        }
                    },
                    LocalLabelRef::InsideCall { index, arg_index, inner_index, target} => {
                        if let Some(ValueToken::BuiltinCall(_, ref mut arguments)) = tokens.get_mut(index) {
                            if let Some(arg_tokens) = arguments.get_mut(arg_index) {
                                if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = arg_tokens.get_mut(inner_index) {
                                    *id = target;
                                    continue;
                                }
                            }
                        }
                    }
                }
                unreachable!("Invalid local label ref generated: {:?}", r);
            }

        }
        Ok(())

    }

    fn verify_local_label_refs_under_global(
        global_label: Option<(usize, Vec<(String, usize)>, Vec<(String, usize, Option<(usize, usize)>)>)>,
        local_label_refs: &mut Vec<LocalLabelRef>

    ) -> Result<(), (usize, usize, Option<(usize, usize)>)> {

        // Check local labels under previous label
        if let Some((previous_index, local_defs, local_refs)) = global_label {
            for (ref_name, token_index, call_parent) in &local_refs {
                let mut label_exists = false;
                for (def_name, label_id) in &local_defs {
                    if def_name == ref_name {
                        label_exists = true;
                        if let Some((index, arg_index)) = call_parent {
                            local_label_refs.push(LocalLabelRef::InsideCall {
                                index: *index,
                                arg_index: *arg_index,
                                inner_index: *token_index,
                                target: *label_id
                            });

                        } else {
                            local_label_refs.push(LocalLabelRef::Global{
                                index: *token_index,
                                target: *label_id
                            });
                        }
                        break;
                    }
                }
                if !label_exists {
                    return Err((*token_index, previous_index, call_parent.clone()));
                }
            }
        }

        Ok(())
    }

    fn convert_global_label_refs(
        global_labels: &HashMap<(String, Option<usize>), (InnerToken, usize)>,
        tokens: Vec<ValueToken>

    ) -> Vec<ValueToken> {
        tokens.into_iter().map(|token| {
            if let ValueToken::Name(inner) = token {

                // Generate references to global labels
                let label_id = Self::global_label_id(&inner);
                if let Some((_, id)) = global_labels.get(&label_id) {
                    ValueToken::GlobalLabelRef(inner, *id)

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
        assert_eq!(tfv("global_label:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1
        )]);
    }

    #[test]
    fn test_global_label_ref() {
        assert_eq!(tfv("global_label:\nglobal_label"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::GlobalLabelRef(
            itf!(14, 26, "global_label", 0),
            1
        )]);
        assert_eq!(tfv("global_label:\nCEIL(global_label)"), vec![
            ValueToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1
            ),
            ValueToken::BuiltinCall(
                itk!(14, 18, "CEIL"),
                vec![
                    vec![
                        ValueToken::GlobalLabelRef(
                            itf!(19, 31, "global_label", 0),
                            1
                        )
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
        assert_eq!(tokens, vec![ValueToken::GlobalLabelDef(
            itf!(0, 25, "_global_file_local_label", 0),
            1

        ), ValueToken::GlobalLabelDef(
            itf!(0, 25, "_global_file_local_label", 1),
            2
        )]);
    }

    #[test]
    fn test_global_file_local_label_ref() {
        let tokens = value_lexer_child(
            "_global_file_local_label:\n_global_file_local_label\nINCLUDE 'child.gb.s'",
            "_global_file_local_label:\n_global_file_local_label"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::GlobalLabelDef(
            itf!(0, 25, "_global_file_local_label", 0),
            1

        ), ValueToken::GlobalLabelRef(
            itf!(26, 50, "_global_file_local_label", 0),
            1

        ), ValueToken::GlobalLabelDef(
            itf!(0, 25, "_global_file_local_label", 1),
            2

        ), ValueToken::GlobalLabelRef(
            itf!(26, 50, "_global_file_local_label", 1),
            2
        )]);
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
        assert_eq!(tfv("global_label:\n.local_label:\n.local_other_label:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelDef(
            itk!(14, 27, "local_label"),
            2

        ), ValueToken::LocalLabelDef(
            itk!(28, 47, "local_other_label"),
            3
        )]);
    }

    #[test]
    fn test_local_label_def_instruction() {
        assert_eq!(tfv("global_label:\n.stop:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelDef(
            itk!(14, 20, "stop"),
            2
        )]);
    }

    #[test]
    fn test_local_label_def_reserved() {
        assert_eq!(tfv("global_label:\n.DS:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelDef(
            itk!(14, 18, "DS"),
            2
        )]);
    }

    #[test]
    fn test_local_label_ref_backward() {
        assert_eq!(tfv("global_label:\n.local_label:\n.local_label"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelDef(
            itk!(14, 27, "local_label"),
            2

        ), ValueToken::LocalLabelRef(
            itk!(28, 40, "local_label"),
            2
        )]);
    }

    #[test]
    fn test_local_label_ref_backward_builtin_call() {
        assert_eq!(tfv("global_label:\n.local_label:\nCEIL(.local_label)"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelDef(
            itk!(14, 27, "local_label"),
            2

        ), ValueToken::BuiltinCall(
            itk!(28, 32, "CEIL"),
            vec![
                vec![
                    ValueToken::LocalLabelRef(
                        itf!(33, 45, "local_label", 0),
                        2
                    )
                ]
            ]
        )]);
    }

    #[test]
    fn test_local_label_ref_forward() {
        assert_eq!(tfv("global_label:\n.local_label\n.local_label:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::LocalLabelRef(
            itk!(14, 26, "local_label"),
            2

        ), ValueToken::LocalLabelDef(
            itk!(27, 40, "local_label"),
            2
        )]);
    }

    #[test]
    fn test_local_label_ref_forward_builtin_call() {
        assert_eq!(tfv("global_label:\nCEIL(.local_label)\n.local_label:"), vec![ValueToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), ValueToken::BuiltinCall(
            itk!(14, 18, "CEIL"),
            vec![
                vec![
                    ValueToken::LocalLabelRef( itf!(19, 31, "local_label", 0), 2)
                ]
            ]
        ), ValueToken::LocalLabelDef(
            itk!(33, 46, "local_label"),
            2
        )]);
    }

    #[test]
    fn test_local_label_ref_forward_macro_intercept() {
        assert_eq!(
            tfv("global_label:\n.local_label\nFOO()\n.local_label:\nMACRO FOO()\n_macro_label:\n.macro_local\n.macro_local:ENDMACRO"), vec![
            ValueToken::GlobalLabelDef(
                itk!(0, 13, "global_label"),
                1

            ), ValueToken::LocalLabelRef(
                itk!(14, 26, "local_label"),
                4

            ), ValueToken::GlobalLabelDef(
                itkm!(59, 72, "_macro_label", 0),
                2

            ), ValueToken::LocalLabelRef(
                itkm!(73, 85, "macro_local_from_macro_call_0", 0),
                3

            ), ValueToken::LocalLabelDef(
                itkm!(86, 99, "macro_local_from_macro_call_0", 0),
                3

            ), ValueToken::LocalLabelDef(
                itk!(33, 46, "local_label"),
                4
            )
        ]);
    }

    #[test]
    fn test_error_local_label_ref_no_macro_leak() {
        assert_eq!(value_lexer_error("global_label:\n.local_label\nFOO()\nMACRO FOO()_macro_label:\n.local_label:\nENDMACRO"), "In file \"main.gb.s\" on line 2, column 1: Reference to unknown local label \"local_label\", not defined under the current global label \"global_label\".\n\n.local_label\n^--- Here\n\nDefinition of global label was in file \"main.gb.s\" on line 1, column 1:\n\nglobal_label:\n^--- Here");
    }

    #[test]
    fn test_label_macro_postfix() {
        assert_eq!(tfv("FOO() FOO() MACRO FOO()\nmacro_label_def:\n.local_macro_label_def:\n.local_macro_label_def\nENDMACRO"), vec![
            ValueToken::GlobalLabelDef(
                itkm!(24, 40, "macro_label_def", 0), 1
            ),
            ValueToken::LocalLabelDef(itkm!(41, 64, "local_macro_label_def_from_macro_call_0", 0), 2),
            ValueToken::LocalLabelRef(itkm!(65, 87, "local_macro_label_def_from_macro_call_0", 0), 2),
            ValueToken::GlobalLabelDef(itkm!(24, 40, "macro_label_def", 1), 3),
            ValueToken::LocalLabelDef(itkm!(41, 64, "local_macro_label_def_from_macro_call_1", 1), 4),
            ValueToken::LocalLabelRef(itkm!(65, 87, "local_macro_label_def_from_macro_call_1", 1), 4)
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

