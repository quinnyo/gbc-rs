// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use gb_cpu::{Flag, Register};
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{MacroStage, Symbol};
use crate::error::SourceError;
use crate::expression::Operator;
use super::macros::{MacroCall, MacroToken, MacroTokenType, IfStatementBranch, ForStatement, BlockStatement};
use super::super::{LexerStage, InnerToken, TokenIterator, LexerToken};


// Modules --------------------------------------------------------------------
mod label_resolver;
use self::label_resolver::{LabelResolver, ParentLabelIndex, ChildLabelIndex, NamespaceIndex};


// Types ----------------------------------------------------------------------
type MacroCallIndex = Option<usize>;
type NamespaceMap = HashMap<NamespaceIndex, (InnerToken, HashMap<String, (InnerToken, usize)>)>;
type NamespaceData<'a> = (NamespaceIndex, bool, &'a InnerToken);


// Value Specific Tokens ------------------------------------------------------
lexer_token!(ValueToken, ValueTokenType, (Debug, Eq, PartialEq), {
    Name(()),
    GlobalName(()),
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
    ParentLabelCall((usize, Option<Vec<Vec<ValueToken>>>)),
    IfStatement((Vec<IfStatementBranch<ValueToken>>)),
    ForStatement((ForStatement<ValueToken>)),
    BlockStatement((BlockStatement<ValueToken>)),
    Lookup((Vec<ValueToken>)),
    ParentLabelDef((usize, Option<Vec<Register>>, bool)),
    ParentLabelRef((usize)),
    ChildLabelDef((usize, Option<usize>)),
    ChildLabelRef((usize, Option<usize>))

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

    ) -> Result<Vec<Self::Output>, SourceError> {
        let mut parent_labels: HashMap<ParentLabelIndex, (InnerToken, usize)> = HashMap::with_capacity(512);
        let mut parent_labels_names: Vec<Symbol> = Vec::with_capacity(64);
        let mut namespaces: NamespaceMap = HashMap::with_capacity(16);
        let mut unique_label_id = 0;
        let mut tokens = Self::parse_tokens(
            &mut parent_labels,
            &mut parent_labels_names,
            &mut unique_label_id,
            &mut namespaces,
            false,
            tokens,
            true,
            None
        )?;
        LabelResolver::convert_child_labels_refs(&mut tokens)?;
        Ok(tokens)
    }

}

impl ValueStage {

    #[allow(clippy::too_many_arguments)]
    fn parse_tokens(
        parent_labels: &mut HashMap<ParentLabelIndex, (InnerToken, usize)>,
        parent_labels_names: &mut Vec<Symbol>,
        unique_label_id: &mut usize,
        namespaces: &mut NamespaceMap,
        is_argument: bool,
        tokens: Vec<MacroToken>,
        resolve_labels: bool,
        namespace_data: Option<NamespaceData>

    ) -> Result<Vec<ValueToken>, SourceError> {

        let mut child_labels: HashMap<ChildLabelIndex, InnerToken> = HashMap::with_capacity(16);

        let mut value_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {

            let mut is_global_namespace = false;
            let token = if token.is_symbol(Symbol::GLOBAL) {
                // Handle global namespace declarations
                if tokens.peek_is(MacroTokenType::NamespaceStatement, None) {
                    let st = tokens.expect(MacroTokenType::NamespaceStatement, None, "when parsing GLOBAL declaration")?;
                    is_global_namespace = true;
                    st

                } else if tokens.peek_is(MacroTokenType::Name, None) {
                    // Pre-Combine "GLOBAL Name" Token Pairs to allow for global label
                    // parsing and to simplify later lexer stages
                    let name = tokens.expect(MacroTokenType::Name, None, "when parsing GLOBAL declaration")?;
                    MacroToken::GlobalName(name.into_inner())

                } else {
                    token
                }

            } else {
                token
            };

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
                MacroToken::CloseBracket(inner) => ValueToken::CloseBracket(inner),

                // Convert Statements
                MacroToken::NamespaceStatement(outer, st) => {

                    let (is_global_namespace, inner) = if let Some((_, is_global, parent)) = namespace_data {
                        // Concatenate existing namespace path
                        let mut inner = outer.clone();
                        inner.value = format!("{}::{}", parent.value, st.name.inner().value).into();
                        (is_global, inner)

                    } else {
                        (is_global_namespace, st.name.inner().clone())
                    };

                    // Insert new top level namespaces
                    let namespace_id = LabelResolver::namespace_id(&inner, false, if !is_global_namespace {
                        Some(inner.file_index)

                    } else {
                        None
                    });

                    if let Some((existing, _)) = namespaces.get(&namespace_id) {
                        return Err(inner.error(format!(
                            "A namespace with the name \"{}\" was already defined.",
                            inner.value

                        )).with_reference(existing, "Original definition of namespace was"));

                    } else {
                        namespaces.insert(
                            namespace_id.clone(),
                            (inner.clone(), HashMap::with_capacity(8))
                        );
                    }

                    let mut body = Self::parse_tokens(
                        parent_labels,
                        parent_labels_names,
                        unique_label_id,
                        namespaces,
                        false,
                        st.body,
                        false,
                        Some((namespace_id, is_global_namespace, &inner))
                    )?;
                    value_tokens.append(&mut body);
                    continue;
                },
                MacroToken::IfStatement(inner, branches) => {
                    let mut value_branches = Vec::with_capacity(branches.len());
                    for branch in branches {
                        value_branches.push(branch.into_other(|tokens| {
                            Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, tokens, false, None)
                        })?);
                    }
                    ValueToken::IfStatement(inner, value_branches)
                },
                MacroToken::ForStatement(inner, for_statement) => {
                    let mut binding = Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, vec![*for_statement.binding], true, None)?;
                    ValueToken::ForStatement(inner, ForStatement {
                        binding: Box::new(binding.remove(0)),
                        from: Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, for_statement.from, true, None)?,
                        to: Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, for_statement.to, true, None)?,
                        body: Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, for_statement.body, false, None)?
                    })
                },
                MacroToken::BlockStatement(inner, block) => {
                    ValueToken::BlockStatement(inner, match block {
                        BlockStatement::Using(cmd, body) => BlockStatement::Using(cmd, Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, body, false, None)?),
                        BlockStatement::Volatile(body) => BlockStatement::Volatile(Self::parse_tokens(parent_labels, parent_labels_names, unique_label_id, namespaces, false, body, false, None)?)
                    })
                },

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

                // Values
                MacroToken::BuiltinCall(inner, args) => {
                    let mut value_args = Vec::with_capacity(args.len());
                    for tokens in args {
                        value_args.push(Self::parse_tokens(
                            parent_labels,
                            parent_labels_names,
                            unique_label_id,
                            namespaces,
                            true,
                            tokens,
                            true,
                            None
                        )?);
                    }
                    ValueToken::BuiltinCall(inner, value_args)
                },
                MacroToken::ParentLabelCall(inner, args) => {
                    let mut value_args = Vec::with_capacity(args.len());
                    for tokens in args {
                        value_args.push(Self::parse_tokens(
                            parent_labels,
                            parent_labels_names,
                            unique_label_id,
                            namespaces,
                            true,
                            tokens,
                            true,
                            None
                        )?);
                    }
                    ValueToken::ParentLabelCall(inner, 0, Some(value_args))
                },
                // Labels
                MacroToken::GlobalName(inner) => {
                    Self::parse_parent_label(
                        &mut tokens,
                        parent_labels,
                        parent_labels_names,
                        unique_label_id,
                        is_argument,
                        true,
                        &mut child_labels,
                        inner
                    )?
                },
                MacroToken::Name(inner) => {
                    if let Some(ref data) = namespace_data {
                        Self::parse_namespace_member(
                            &mut tokens,
                            namespaces,
                            data,
                            unique_label_id,
                            inner
                        )?
                    } else {
                        Self::parse_parent_label(
                            &mut tokens,
                            parent_labels,
                            parent_labels_names,
                            unique_label_id,
                            is_argument,
                            false,
                            &mut child_labels,
                            inner
                        )?
                    }
                },
                MacroToken::Point(inner) => Self::parse_child_label(
                    &mut tokens,
                    parent_labels,
                    parent_labels_names,
                    unique_label_id,
                    is_argument,
                    &mut child_labels,
                    inner
                )?,
                MacroToken::Lookup(inner, tokens) => {
                    ValueToken::Lookup(inner, Self::parse_tokens(
                        parent_labels,
                        parent_labels_names,
                        unique_label_id,
                        namespaces,
                        false,
                        tokens,
                        true,
                        None
                    )?)
                },
                // Offsets
                MacroToken::Offset(inner) => ValueToken::Offset {
                    value: Self::parse_integer(&inner, 0, 10)?,
                    inner
                },
                // Literals
                MacroToken::NumberLiteral(inner) => Self::parse_number_literal(inner)?,
                MacroToken::StringLiteral(inner) => ValueToken::String {
                    value: inner.value.to_string(),
                    inner
                },
                MacroToken::Colon(inner) => {
                    return Err(inner.error(format!("Unexpected standalone \"{}\", expected a \"Name\" token to preceed it.", inner.value)))
                },
                // Operators
                MacroToken::Operator(mut inner) => {
                    let typ = if tokens.peek_is(MacroTokenType::Operator, None) {
                        match Self::parse_operator_double(&inner, tokens.peek().unwrap().inner()) {
                            Some(typ) => {
                                tokens.expect(MacroTokenType::Operator, None, "when parsing operator")?;
                                typ
                            },
                            None => Self::parse_operator_single(&inner)?
                        }

                    } else {
                        Self::parse_operator_single(&inner)?
                    };
                    inner.end_index = inner.start_index + typ.width();
                    ValueToken::Operator {
                        inner,
                        typ
                    }
                }
            };
            value_tokens.push(value_token);
        }

        // Convert name tokens into corresponding parent label references
        if resolve_labels {
            LabelResolver::convert_parent_label_refs(parent_labels, namespaces, &mut value_tokens)?;
        }

        Ok(value_tokens)
    }

    fn parse_namespace_member(
        tokens: &mut TokenIterator<MacroToken>,
        namespaces: &mut NamespaceMap,
        namespace_data: &NamespaceData,
        unique_label_id: &mut usize,
        mut inner: InnerToken

    ) -> Result<ValueToken, SourceError> {

        let argument_list = Self::parse_label_argument_list(tokens)?;
        if argument_list.is_some() || tokens.peek_is(MacroTokenType::Colon, None) {

            let colon = tokens.expect(MacroTokenType::Colon, None, "when parsing namespace member definition")?.into_inner();
            let (sid, _, parent) = namespace_data;
            let namespace_members = &mut namespaces.get_mut(sid).unwrap().1;

            // Check if the member already exists
            let member_id = format!("{}::{}", parent.value, inner.value);
            if let Some((existing, _)) = namespace_members.get(&member_id) {
                Err(inner.error(format!(
                    "A member with the name \"{}\" was already defined within the current namespace.",
                    inner.value

                )).with_reference(existing, "Original definition of member was"))

            } else {
                // Add member to namespace
                *unique_label_id += 1;
                inner.value = member_id.clone().into();
                inner.end_index = colon.end_index;
                namespace_members.insert(member_id, (inner.clone(), *unique_label_id));

                Ok(ValueToken::ParentLabelDef(inner, *unique_label_id, argument_list, false))
            }

        } else {
            Ok(ValueToken::Name(inner))
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_parent_label(
        tokens: &mut TokenIterator<MacroToken>,
        parent_labels: &mut HashMap<ParentLabelIndex, (InnerToken, usize)>,
        parent_labels_names: &mut Vec<Symbol>,
        unique_label_id: &mut usize,
        is_argument: bool,
        is_global: bool,
        child_labels: &mut HashMap<ChildLabelIndex, InnerToken>,
        mut inner: InnerToken

    ) -> Result<ValueToken, SourceError> {

        // Parse optional label arguments
        let argument_list = Self::parse_label_argument_list(tokens)?;
        if argument_list.is_some() || tokens.peek_is(MacroTokenType::Colon, None) {

            let colon = tokens.expect(MacroTokenType::Colon, None, "when parsing label definition")?.into_inner();
            let label_id = LabelResolver::parent_label_id(&inner, false, if !is_global {
                Some(inner.file_index)

            } else {
                None
            });

            if let Some((existing, _)) = parent_labels.get(&label_id) {
                Err(inner.error(format!(
                    "A label with the name \"{}\" was already defined.",
                    inner.value

                )).with_reference(existing, "Original definition of label was"))

            } else if is_argument {
                Err(inner.error("A label cannot be defined inside an argument list".to_string()))

            } else {
                *unique_label_id += 1;
                inner.end_index = colon.end_index;
                parent_labels.insert(label_id.clone(), (inner.clone(), *unique_label_id));
                parent_labels_names.push(label_id.0.clone());
                child_labels.clear();

                Ok(ValueToken::ParentLabelDef(inner, *unique_label_id, argument_list, is_global))
            }

        } else if is_global {
            Ok(ValueToken::GlobalName(inner))

        } else {
            Ok(ValueToken::Name(inner))
        }
    }

    fn parse_label_argument_list(
        tokens: &mut TokenIterator<MacroToken>

    ) -> Result<Option<Vec<Register>>, SourceError> {

        if tokens.peek_is(MacroTokenType::OpenParen, None) {
            tokens.expect(MacroTokenType::OpenParen, None, "when parsing label definition")?;

            let mut arguments = Vec::with_capacity(2);
            for token in tokens.by_ref() {
                if token.is(MacroTokenType::Register) {
                    let reg = Register::from(token.inner().value.as_str());
                    if reg.is_loadable() {
                        if arguments.contains(&reg) {
                            return Err(token.error("Duplicate register in label argument list".to_string()));
                        }
                        arguments.push(reg);

                    } else {
                        return Err(token.error("Invalid register in label argument list".to_string()));
                    }
                    continue;

                } else if token.is(MacroTokenType::Comma) {
                    continue;

                } else if token.is(MacroTokenType::CloseParen) {
                    break;

                } else {
                    return Err(token.error("Only registers are allowed inside label argument".to_string()));
                }
            }
            Ok(Some(arguments))

        } else {
            Ok(None)
        }
    }

    fn parse_child_label(
        tokens: &mut TokenIterator<MacroToken>,
        parent_labels: &mut HashMap<ParentLabelIndex, (InnerToken, usize)>,
        parent_labels_names: &mut [Symbol],
        unique_label_id: &mut usize,
        is_argument: bool,
        child_labels: &mut HashMap<ChildLabelIndex, InnerToken>,
        mut inner: InnerToken

    ) -> Result<ValueToken, SourceError> {
        // For child labels all kinds of names are allowed
        let name_token = if tokens.peek_is(MacroTokenType::Instruction, None) {
            tokens.expect(MacroTokenType::Instruction, None, "when parsing child label")?.into_inner()

        } else if tokens.peek_is(MacroTokenType::Reserved, None) {
            tokens.expect(MacroTokenType::Reserved, None, "when parsing child label")?.into_inner()

        } else {
            tokens.expect(MacroTokenType::Name, None, "when parsing child label")?.into_inner()
        };

        // Postfix labels created by macros calls so they are unique
        let label_index = if let Some(call_id) = name_token.macro_call_id() {
            (name_token.value.clone(), Some(call_id))

        } else {
            (name_token.value.clone(), None)
        };

        if tokens.peek_is(MacroTokenType::Colon, None) {
            let colon = tokens.expect(MacroTokenType::Colon, None, "when parsing child label definition")?.into_inner();
            if parent_labels.is_empty() {
                Err(inner.error(format!(
                    "Unexpected definition of child label \"{}\" without parent.",
                    name_token.value
                )))

            } else if let Some(previous) = child_labels.get(&label_index) {
                Err(inner.error(format!(
                    "child label \"{}\" was already defined under the current parent label \"{}\".",
                    label_index.0,
                    parent_labels_names.last().unwrap()

                )).with_reference(previous, "Original definition of child label was"))

            } else if is_argument {
                Err(inner.error("A child label cannot be defined inside an argument list".to_string()))

            } else {
                inner.end_index = colon.end_index;
                inner.value = label_index.0.clone();
                child_labels.insert(label_index.clone(), inner.clone());

                *unique_label_id += 1;
                Ok(ValueToken::ChildLabelDef(inner, *unique_label_id, label_index.1))
            }

        } else if parent_labels.is_empty() {
            Err(inner.error(format!(
                "Unexpected definition of child label \"{}\" without parent.",
                name_token.value
            )))

        } else {
            inner.value = label_index.0;
            inner.end_index = name_token.end_index;
            Ok(ValueToken::ChildLabelRef(inner, 0, label_index.1))
        }
    }

    fn parse_number_literal(inner: InnerToken) -> Result<ValueToken, SourceError> {
        Ok(match inner.value.as_str().chars().next().unwrap() {
            '$' => ValueToken::Integer {
                value: Self::parse_integer(&inner, 1, 16)?,
                inner
            },
            '%' => ValueToken::Integer {
                value: Self::parse_integer(&inner, 1, 2)?,
                inner
            },
            _ => if inner.value.as_str().contains('.') {
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
        })
    }

    fn parse_integer(inner: &InnerToken, from: usize, radix: u32) -> Result<i32, SourceError> {
        i32::from_str_radix(&inner.value.as_str()[from..], radix).map_err(|_| {
            inner.error("Failed to parse integer value.".to_string())
        })
    }

    fn parse_float(inner: &InnerToken) -> Result<f32, SourceError> {
        inner.value.as_str().parse::<f32>().map_err(|_| {
            inner.error("Failed to parse float value.".to_string())
        })
    }

    fn parse_operator_single(inner: &InnerToken) -> Result<Operator, SourceError> {
        match inner.value.as_str().chars().next().unwrap() {
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
            first.value.as_str().chars().next().unwrap(),
            second.value.as_str().chars().next().unwrap()
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
    use crate::mocks::{macro_lex, macro_lex_child};
    use super::{ValueStage, ValueToken, InnerToken, Operator, Register, Flag, IfStatementBranch, ForStatement, BlockStatement};

    fn value_lexer<S: Into<String>>(s: S) -> Lexer<ValueStage> {
        Lexer::<ValueStage>::from_lexer(macro_lex(s)).expect("ValueLexer failed")
    }

    fn value_lexer_child<S: Into<String>>(s: S, c: S) -> Lexer<ValueStage> {
        Lexer::<ValueStage>::from_lexer(macro_lex_child(s, c)).expect("ValueLexer failed")
    }

    fn value_lexer_error<S: Into<String>>(s: S) -> String {
        colored::control::set_override(false);
        Lexer::<ValueStage>::from_lexer(macro_lex(s)).err().unwrap().to_string()
    }

    fn value_lexer_child_error<S: Into<String>>(s: S, c: S) -> String {
        colored::control::set_override(false);
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

    macro_rules! vtkm {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $parsed);
                t.set_macro_call_id($id);
                ValueToken::$tok(t)
            }
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
            ValueToken::Operator {
                inner: itk!(4, 5, "-"),
                typ: Operator::Minus
            },
            ValueToken::Integer {
                inner: itk!(5, 8, "123"),
                value: 123
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
            ValueToken::Operator {
                inner: itk!(29, 30, "-"),
                typ: Operator::Minus
            },
            ValueToken::Float {
                inner: itk!(30, 34, "2.48"),
                value: OrderedFloat::from(2.48)
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
    fn test_parent_label_def() {
        assert_eq!(tfv("parent_label:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false
        )]);
    }

    #[test]
    fn test_parent_label_def_with_arguments() {
        assert_eq!(tfv("parent_label(bc, de, hl):"), vec![ValueToken::ParentLabelDef(
            itk!(0, 25, "parent_label"),
            1,
            Some(vec![Register::BC, Register::DE, Register::HL]),
            false
        )]);
        assert_eq!(tfv("parent_label(a, b, c, d, e, h, l):"), vec![ValueToken::ParentLabelDef(
            itk!(0, 34, "parent_label"),
            1,
            Some(vec![
                Register::Accumulator,
                Register::B,
                Register::C,
                Register::D,
                Register::E,
                Register::H,
                Register::L
            ]),
            false
        )]);
    }

    #[test]
    fn test_parent_label_call_with_arguments() {
        assert_eq!(tfv("parent_label():\nparent_label()"), vec![ValueToken::ParentLabelDef(
            itk!(0, 15, "parent_label"),
            1,
            Some(vec![]),
            false

        ), ValueToken::ParentLabelCall(
            itk!(16, 28, "parent_label"),
            1,
            Some(vec![])
        )]);
    }

    #[test]
    fn test_error_parent_label_def_with_arguments() {
        assert_eq!(
            value_lexer_error("parent_label(ld):"),
            "In file \"main.gb.s\" on line 1, column 14: Only registers are allowed inside label argument\n\nparent_label(ld):\n             ^--- Here"
        );
        assert_eq!(
            value_lexer_error("parent_label(sp):"),
            "In file \"main.gb.s\" on line 1, column 14: Invalid register in label argument list\n\nparent_label(sp):\n             ^--- Here"
        );
        assert_eq!(
            value_lexer_error("parent_label(a, a):"),
            "In file \"main.gb.s\" on line 1, column 17: Duplicate register in label argument list\n\nparent_label(a, a):\n                ^--- Here"
        );
    }

    #[test]
    fn test_parent_label_ref() {
        assert_eq!(tfv("parent_label:\nparent_label"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ParentLabelRef(
            itf!(14, 26, "parent_label", 0),
            1
        )]);
        assert_eq!(tfv("parent_label:\nCEIL(parent_label)"), vec![
            ValueToken::ParentLabelDef(
                itk!(0, 13, "parent_label"),
                1,
                None,
                false
            ),
            ValueToken::BuiltinCall(
                itk!(14, 18, "CEIL"),
                vec![
                    vec![
                        ValueToken::ParentLabelRef(
                            itf!(19, 31, "parent_label", 0),
                            1
                        )
                    ]
                ]
            )
        ]);
    }

    #[test]
    fn test_parent_label_no_ref() {
        assert_eq!(tfv("parent_label"), vec![ValueToken::Name(
            itf!(0, 12, "parent_label", 0)
        )]);
    }

    #[test]
    fn test_parent_file_child_label_def() {
        let tokens = value_lexer_child(
            "_parent_file_child_label:\nINCLUDE 'child.gb.s'",
            "_parent_file_child_label:"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::ParentLabelDef(
            itf!(0, 25, "_parent_file_child_label", 0),
            1,
            None,
            false

        ), ValueToken::ParentLabelDef(
            itf!(0, 25, "_parent_file_child_label", 1),
            2,
            None,
            false
        )]);
    }

    #[test]
    fn test_parent_file_child_label_ref() {
        let tokens = value_lexer_child(
            "_parent_file_child_label:\n_parent_file_child_label\nINCLUDE 'child.gb.s'",
            "_parent_file_child_label:\n_parent_file_child_label"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::ParentLabelDef(
            itf!(0, 25, "_parent_file_child_label", 0),
            1,
            None,
            false

        ), ValueToken::ParentLabelRef(
            itf!(26, 50, "_parent_file_child_label", 0),
            1

        ), ValueToken::ParentLabelDef(
            itf!(0, 25, "_parent_file_child_label", 1),
            2,
            None,
            false

        ), ValueToken::ParentLabelRef(
            itf!(26, 50, "_parent_file_child_label", 1),
            2
        )]);
    }

    #[test]
    fn test_parent_file_child_label_no_ref() {
        let tokens = value_lexer_child(
            "_parent_file_child_label\nINCLUDE 'child.gb.s'",
            "_parent_file_child_label"

        ).tokens;
        assert_eq!(tokens, vec![ValueToken::Name(
            itf!(0, 24, "_parent_file_child_label", 0)

        ), ValueToken::Name(
            itf!(0, 24, "_parent_file_child_label", 1)
        )]);
    }

    #[test]
    fn test_error_parent_label_def_duplicate() {
        assert_eq!(value_lexer_error(
            "parent_label:\nparent_label:"

        ), "In file \"main.gb.s\" on line 2, column 1: A label with the name \"parent_label\" was already defined.\n\nparent_label:\n^--- Here\n\nOriginal definition of label was in file \"main.gb.s\" on line 1, column 1:\n\nparent_label:\n^--- Here");
    }

    #[test]
    fn test_error_parent_file_child_label_def_duplicate() {
        assert_eq!(
            value_lexer_error("_parent_file_child_label:\n_parent_file_child_label:"),
            "In file \"main.gb.s\" on line 2, column 1: A label with the name \"_parent_file_child_label\" was already defined.\n\n_parent_file_child_label:\n^--- Here\n\nOriginal definition of label was in file \"main.gb.s\" on line 1, column 1:\n\n_parent_file_child_label:\n^--- Here"
        );
    }

    #[test]
    fn test_parent_label_def_child_both_local() {
        let tokens = value_lexer_child(
            "parent_label:\nINCLUDE 'child.gb.s'",
            "parent_label:"
        ).tokens;

        assert_eq!(tokens, vec![
            ValueToken::ParentLabelDef(itf!(0, 13, "parent_label", 0), 1, None, false),
            ValueToken::ParentLabelDef(itf!(0, 13, "parent_label", 1), 2, None, false)
        ]);
    }

    #[test]
    fn test_parent_label_def_child_one_local() {
        let tokens = value_lexer_child(
            "GLOBAL parent_label:\nINCLUDE 'child.gb.s'",
            "parent_label:"
        ).tokens;

        assert_eq!(tokens, vec![
            ValueToken::ParentLabelDef(itf!(7, 20, "parent_label", 0), 1, None, true),
            ValueToken::ParentLabelDef(itf!(0, 13, "parent_label", 1), 2, None, false)
        ]);
    }


    #[test]
    fn test_parent_label_ref_global_from_child() {
        let tokens = value_lexer_child(
            "GLOBAL parent_label:\nINCLUDE 'child.gb.s'",
            "parent_label"
        ).tokens;

        assert_eq!(tokens, vec![
            ValueToken::ParentLabelDef(itf!(7, 20, "parent_label", 0), 1, None, true),
            ValueToken::ParentLabelRef(itf!(0, 12, "parent_label", 1), 1)
        ]);
    }

    #[test]
    fn test_parent_label_ref_local_child_keep_name() {
        let tokens = value_lexer_child(
            "parent_label:\nINCLUDE 'child.gb.s'",
            "parent_label"
        ).tokens;

        assert_eq!(tokens, vec![
            ValueToken::ParentLabelDef(itf!(0, 13, "parent_label", 0), 1, None, false),
            ValueToken::Name(itf!(0, 12, "parent_label", 1))
        ]);
    }

    #[test]
    fn test_error_parent_label_def_duplicate_global_child() {
        assert_eq!(value_lexer_child_error(
            "GLOBAL parent_label:\nINCLUDE 'child.gb.s'",
            "GLOBAL parent_label:"

        ), "In file \"child.gb.s\" on line 1, column 8: A label with the name \"parent_label\" was already defined.\n\nGLOBAL parent_label:\n       ^--- Here\n\nincluded from file \"main.gb.s\" on line 2, column 9\n\nOriginal definition of label was in file \"main.gb.s\" on line 1, column 8:\n\nGLOBAL parent_label:\n       ^--- Here");
    }

    #[test]
    fn test_error_parent_label_def_in_call() {
        assert_eq!(value_lexer_error(
            "CEIL(parent_label:)"

        ), "In file \"main.gb.s\" on line 1, column 6: A label cannot be defined inside an argument list\n\nCEIL(parent_label:)\n     ^--- Here");
    }

    #[test]
    fn test_error_child_label_def_in_call() {
        assert_eq!(value_lexer_error(
            "parent_label:\nCEIL(.child_label:)"

        ), "In file \"main.gb.s\" on line 2, column 6: A child label cannot be defined inside an argument list\n\nCEIL(.child_label:)\n     ^--- Here");
    }

    #[test]
    fn test_child_label_def() {
        assert_eq!(tfv("parent_label:\n.child_label:\n.child_other_label:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelDef(
            itk!(14, 27, "child_label"),
            2,
            None

        ), ValueToken::ChildLabelDef(
            itk!(28, 47, "child_other_label"),
            3,
            None
        )]);
    }

    #[test]
    fn test_child_label_def_instruction() {
        assert_eq!(tfv("parent_label:\n.stop:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelDef(
            itk!(14, 20, "stop"),
            2,
            None
        )]);
    }

    #[test]
    fn test_child_label_def_reserved() {
        assert_eq!(tfv("parent_label:\n.DS:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelDef(
            itk!(14, 18, "DS"),
            2,
            None
        )]);
    }

    #[test]
    fn test_child_label_ref_backward() {
        assert_eq!(tfv("parent_label:\n.child_label:\n.child_label"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelDef(
            itk!(14, 27, "child_label"),
            2,
            None

        ), ValueToken::ChildLabelRef(
            itk!(28, 40, "child_label"),
            2,
            None
        )]);
    }

    #[test]
    fn test_child_label_ref_backward_builtin_call() {
        assert_eq!(tfv("parent_label:\n.child_label:\nCEIL(.child_label)"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelDef(
            itk!(14, 27, "child_label"),
            2,
            None

        ), ValueToken::BuiltinCall(
            itk!(28, 32, "CEIL"),
            vec![
                vec![
                    ValueToken::ChildLabelRef(
                        itf!(33, 45, "child_label", 0),
                        2,
                        None
                    )
                ]
            ]
        )]);
    }

    #[test]
    fn test_child_label_ref_forward() {
        assert_eq!(tfv("parent_label:\n.child_label\n.child_label:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::ChildLabelRef(
            itk!(14, 26, "child_label"),
            2,
            None

        ), ValueToken::ChildLabelDef(
            itk!(27, 40, "child_label"),
            2,
            None
        )]);
    }

    #[test]
    fn test_child_label_ref_forward_builtin_call() {
        assert_eq!(tfv("parent_label:\nCEIL(.child_label)\n.child_label:"), vec![ValueToken::ParentLabelDef(
            itk!(0, 13, "parent_label"),
            1,
            None,
            false

        ), ValueToken::BuiltinCall(
            itk!(14, 18, "CEIL"),
            vec![
                vec![
                    ValueToken::ChildLabelRef( itf!(19, 31, "child_label", 0), 2, None)
                ]
            ]
        ), ValueToken::ChildLabelDef(
            itk!(33, 46, "child_label"),
            2,
            None
        )]);
    }

    #[test]
    fn test_child_label_ref_forward_macro_intercept() {
        assert_eq!(
            tfv("parent_label:\n.child_label\nFOO()\n.child_label:\nMACRO FOO()\n_macro_label:\n.macro_child\n.macro_child:ENDMACRO"), vec![
            ValueToken::ParentLabelDef(
                itk!(0, 13, "parent_label"),
                1,
                None,
                false

            ), ValueToken::ChildLabelRef(
                itk!(14, 26, "child_label"),
                4,
                None

            ), ValueToken::ParentLabelDef(
                itkm!(59, 72, "_macro_label", 0),
                2,
                None,
                false

            ), ValueToken::ChildLabelRef(
                itkm!(73, 85, "macro_child", 0),
                3,
                Some(0)

            ), ValueToken::ChildLabelDef(
                itkm!(86, 99, "macro_child", 0),
                3,
                Some(0)

            ), ValueToken::ChildLabelDef(
                itk!(33, 46, "child_label"),
                4,
                None
            )
        ]);
    }

    #[test]
    fn test_error_child_label_ref_no_macro_leak() {
        assert_eq!(value_lexer_error("parent_label:\n.child_label\nFOO()\nMACRO FOO()_macro_label:\n.child_label:\nENDMACRO"), "In file \"main.gb.s\" on line 2, column 1: Reference to unknown child label \"child_label\", not defined under the current parent label \"parent_label\".\n\n.child_label\n^--- Here\n\nDefinition of parent label was in file \"main.gb.s\" on line 1, column 1:\n\nparent_label:\n^--- Here");
    }

    #[test]
    fn test_label_macro_postfix() {
        assert_eq!(tfv("FOO() FOO() MACRO FOO()\nmacro_label_def:\n.child_macro_label_def:\n.child_macro_label_def\nENDMACRO"), vec![
            ValueToken::ParentLabelDef(
                itkm!(24, 40, "macro_label_def", 0), 1, None, false
            ),
            ValueToken::ChildLabelDef(itkm!(41, 64, "child_macro_label_def", 0), 2, Some(0)),
            ValueToken::ChildLabelRef(itkm!(65, 87, "child_macro_label_def", 0), 2, Some(0)),
            ValueToken::ParentLabelDef(itkm!(24, 40, "macro_label_def", 1), 3, None, false),
            ValueToken::ChildLabelDef(itkm!(41, 64, "child_macro_label_def", 1), 4, Some(1)),
            ValueToken::ChildLabelRef(itkm!(65, 87, "child_macro_label_def", 1), 4, Some(1))
        ]);
    }

    #[test]
    fn test_macro_arg_parent_label_ref() {
        assert_eq!(tfv("parent:\nMACRO FOO(@a) DB @a ENDMACRO\nFOO(parent)"), vec![
            ValueToken::ParentLabelDef(
                itk!(0, 7, "parent"), 1, None, false
            ),
            vtkm!(Reserved, 22, 24, "DB", 0),
            ValueToken::ParentLabelRef(
                itkm!(41, 47, "parent", 0),
                1
            )
        ]);
    }

    #[test]
    fn test_macro_parent_label_ref() {
        assert_eq!(tfv("MACRO FOO() parent_label:\nparent_label\n ENDMACRO\nFOO()"), vec![
            ValueToken::ParentLabelDef(itkm!(12, 25, "parent_label", 0), 1, None, false),
            ValueToken::ParentLabelRef(itkm!(26, 38, "parent_label", 0), 1)
        ]);
    }

    #[test]
    fn test_macro_child_label_ref_with_external_parent() {
        assert_eq!(
            value_lexer_error("MACRO FOO() .child_label:\n.child_label\n ENDMACRO\nparent_label:\nFOO()"),
            "In file \"main.gb.s\" on line 2, column 1: Reference to child label inside of macro without a any parent label inside the macro.\n\n.child_label\n^--- Here\n\nIn file \"main.gb.s\" on line 5, column 1: Triggered by previous macro invocation\n\nFOO()\n^--- Here"
        );
    }

    #[test]
    fn test_error_child_label_def_outside_parent() {
        assert_eq!(value_lexer_error(".child_label:"), "In file \"main.gb.s\" on line 1, column 1: Unexpected definition of child label \"child_label\" without parent.\n\n.child_label:\n^--- Here");
    }

    #[test]
    fn test_error_child_label_ref_outside_parent() {
        assert_eq!(value_lexer_error(".child_label"), "In file \"main.gb.s\" on line 1, column 1: Unexpected definition of child label \"child_label\" without parent.\n\n.child_label\n^--- Here");
    }

    #[test]
    fn test_error_child_label_ref_without_def() {
        assert_eq!(value_lexer_error("parent_label:\n.child_label"), "In file \"main.gb.s\" on line 2, column 1: Reference to unknown child label \"child_label\", not defined under the current parent label \"parent_label\".\n\n.child_label\n^--- Here\n\nDefinition of parent label was in file \"main.gb.s\" on line 1, column 1:\n\nparent_label:\n^--- Here");
    }

    #[test]
    fn test_error_child_label_ref_without_def_in_builtin_call() {
        assert_eq!(value_lexer_error("parent_label:\nCEIL(.child_label)"), "In file \"main.gb.s\" on line 2, column 6: Reference to unknown child label \"child_label\", not defined under the current parent label \"parent_label\".\n\nCEIL(.child_label)\n     ^--- Here\n\nDefinition of parent label was in file \"main.gb.s\" on line 1, column 1:\n\nparent_label:\n^--- Here");
    }

    #[test]
    fn test_error_child_label_def_duplicate() {
        assert_eq!(
            value_lexer_error("parent_label:\n.child_label:\n.child_label:"),
            "In file \"main.gb.s\" on line 3, column 1: child label \"child_label\" was already defined under the current parent label \"parent_label\".\n\n.child_label:\n^--- Here\n\nOriginal definition of child label was in file \"main.gb.s\" on line 2, column 1:\n\n.child_label:\n^--- Here"
        );
    }

    #[test]
    fn test_error_child_label_def() {
        assert_eq!(value_lexer_error(".4"), "In file \"main.gb.s\" on line 1, column 2: Unexpected token \"NumberLiteral\" when parsing child label, expected a \"Name\" token instead.\n\n.4\n ^--- Here");
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

    // If Statements ----------------------------------------------------------
    #[test]
    fn test_if_statement_forwarding() {
        let lexer = value_lexer("IF foo THEN IF bar THEN baz ENDIF ENDIF");
        assert_eq!(lexer.tokens, vec![
            ValueToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![ValueToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![
                        ValueToken::IfStatement(itk!(12, 14, "IF"), vec![
                            IfStatementBranch {
                                condition: Some(vec![ValueToken::Name(itk!(15, 18, "bar"))]),
                                body: vec![
                                    ValueToken::Name(itk!(24, 27, "baz"))
                                ]
                            }
                        ])
                    ]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_label_reference() {
        let lexer = value_lexer("IF foo THEN jp global_label ENDIF\nglobal_label:");
        assert_eq!(lexer.tokens, vec![
            ValueToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![ValueToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![
                        ValueToken::Instruction(itk!(12, 14, "jp")),
                        ValueToken::ParentLabelRef(itf!(15, 27, "global_label", 0), 1)
                    ]
                }
            ]),
            ValueToken::ParentLabelDef(itf!(34, 47, "global_label", 0), 1, None, false)
        ]);
    }

    // FOR Statements ---------------------------------------------------------
    #[test]
    fn test_for_statement_forwarding() {
        let lexer = value_lexer("FOR x IN 0 TO 10 REPEAT bar ENDFOR");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ForStatement(itk!(0, 3, "FOR"), ForStatement {
                binding: Box::new(ValueToken::Name(itk!(4, 5, "x"))),
                from: vec![ValueToken::Integer {
                    inner: itk!(9, 10, "0"),
                    value: 0
                }],
                to: vec![ValueToken::Integer {
                    inner: itk!(14, 16, "10"),
                    value: 10
                }],
                body: vec![ValueToken::Name(itk!(24, 27, "bar"))]
            })
        ]);
    }

    #[test]
    fn test_for_statement_label_reference() {
        let lexer = value_lexer("FOR x IN 0 TO 10 REPEAT global_label ENDFOR\nglobal_label:");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ForStatement(itk!(0, 3, "FOR"), ForStatement {
                binding: Box::new(ValueToken::Name(itk!(4, 5, "x"))),
                from: vec![ValueToken::Integer {
                    inner: itk!(9, 10, "0"),
                    value: 0
                }],
                to: vec![ValueToken::Integer {
                    inner: itk!(14, 16, "10"),
                    value: 10
                }],
                body: vec![
                    ValueToken::ParentLabelRef(itf!(24, 36, "global_label", 0), 1)
                ]
            }),
            ValueToken::ParentLabelDef(itf!(44, 57, "global_label", 0), 1, None, false)
        ]);
    }

    // Blocks -----------------------------------------------------------------
    #[test]
    fn test_block_using_forwarding() {
        let lexer = value_lexer("BLOCK USING 'cmd' DB 1 ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ValueToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Using(
                "cmd".to_string(),
                vec![
                    ValueToken::Reserved(itk!(18, 20, "DB")),
                    ValueToken::Integer {
                        inner: itk!(21, 22, "1"),
                        value: 1
                    }
                ])
            )
        ]);
    }

    #[test]
    fn test_block_volatile_forwarding() {
        let lexer = value_lexer("BLOCK VOLATILE nop ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ValueToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Volatile(
                vec![
                    ValueToken::Instruction(itk!(15, 18, "nop"))
                ])
            )
        ]);
    }

    // Block Label IDs --------------------------------------------------------
    #[test]
    fn test_label_ref_block() {
        let lexer = value_lexer("BLOCK VOLATILE\nparent:\n.child:\nDB .child\nENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ValueToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Volatile(
                vec![
                    ValueToken::ParentLabelDef(itk!(15, 22, "parent"), 1, None, false),
                    ValueToken::ChildLabelDef(itk!(23, 30, "child"), 2, None),
                    ValueToken::Reserved(itk!(31, 33, "DB")),
                    ValueToken::ChildLabelRef(itk!(34, 40, "child"), 2, None)
                ]
            ))
        ]);
    }

    #[test]
    fn test_label_ref_if_statement() {
        let lexer = value_lexer("IF foo THEN\nparent:\n.child:\nDB .child\nENDIF");
        assert_eq!(lexer.tokens, vec![
            ValueToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![ValueToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![
                        ValueToken::ParentLabelDef(itk!(12, 19, "parent"), 1, None, false),
                        ValueToken::ChildLabelDef(itk!(20, 27, "child"), 2, None),
                        ValueToken::Reserved(itk!(28, 30, "DB")),
                        ValueToken::ChildLabelRef(itk!(31, 37, "child"), 2, None)
                    ]
                }
            ])
        ]);
    }

    #[test]
    fn test_label_ref_for_statement() {
        let lexer = value_lexer("FOR x IN 0 TO 10 REPEAT\nparent:\n.child:\nDB .child\nENDFOR");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ForStatement(itk!(0, 3, "FOR"), ForStatement {
                binding: Box::new(ValueToken::Name(itk!(4, 5, "x"))),
                from: vec![ValueToken::Integer {
                    inner: itk!(9, 10, "0"),
                    value: 0
                }],
                to: vec![ValueToken::Integer {
                    inner: itk!(14, 16, "10"),
                    value: 10
                }],
                body: vec![
                    ValueToken::ParentLabelDef(itk!(24, 31, "parent"), 1, None, false),
                    ValueToken::ChildLabelDef(itk!(32, 39, "child"), 2, None),
                    ValueToken::Reserved(itk!(40, 42, "DB")),
                    ValueToken::ChildLabelRef(itk!(43, 49, "child"), 2, None)
                ]
            })
        ]);
    }

    // Namespace Statements ---------------------------------------------------
    #[test]
    fn test_namespace_statement_local() {
        let lexer = value_lexer("NAMESPACE foo field: DB ENDNAMESPACE\nfoo::field");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ParentLabelDef(itk!(14, 20, "foo::field"), 1, None, false),
            ValueToken::Reserved(itk!(21, 23, "DB")),
            ValueToken::ParentLabelRef(itk!(37, 47, "foo::field"), 1)
        ]);
    }

    // TODO fill in test data
    // #[test]
    // fn test_namespace_statement_callable_label_parameter() {
    //     let lexer = value_lexer("NAMESPACE foo field: DB ENDNAMESPACE\ncall callable_label(foo::field)\ncallable_label(a):\nret");
    //     assert_eq!(lexer.tokens, vec![
    //         ValueToken::ParentLabelDef(itk!(14, 20, "foo::field"), 1, None),
    //         ValueToken::Reserved(itk!(21, 23, "DB")),
    //         ValueToken::ParentLabelRef(itk!(37, 47, "foo::field"), 1)
    //     ]);
    // }

    #[test]
    fn test_namespace_statement_constants() {
        let lexer = value_lexer("NAMESPACE foo field: DS SIZE ENDNAMESPACE");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ParentLabelDef(itk!(14, 20, "foo::field"), 1, None, false),
            ValueToken::Reserved(itk!(21, 23, "DS")),
            ValueToken::Name(itk!(24, 28, "SIZE"))
        ]);
    }

    #[test]
    fn test_namespace_statement_local_error() {
        let e = value_lexer_child_error("INCLUDE 'child.gb.s'\nNAMESPACE foo field: DB ENDNAMESPACE", "foo::field");
        assert_eq!(e, "In file \"child.gb.s\" on line 1, column 1: Reference to unknown namespace \"foo\".\n\nfoo::field\n^--- Here\n\nincluded from file \"main.gb.s\" on line 1, column 9".to_string());
    }

    #[test]
    fn test_namespace_statement_global() {
        let lexer = value_lexer_child("INCLUDE 'child.gb.s'\nGLOBAL NAMESPACE foo field: DB ENDNAMESPACE", "foo::field");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ParentLabelRef(itf!(0, 10, "foo::field", 1), 1),
            ValueToken::ParentLabelDef(itk!(42, 48, "foo::field"), 1, None, false),
            ValueToken::Reserved(itk!(49, 51, "DB"))
        ]);
    }

    #[test]
    fn test_namespace_statement_macro() {
        let lexer = value_lexer("MACRO m(@name) NAMESPACE @name field: DB ENDNAMESPACE foo::field ENDMACRO m(foo)");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ParentLabelDef(itkm!(31, 37, "foo::field", 0), 1, None, false),
            ValueToken::Reserved(itkm!(38, 40, "DB", 0)),
            ValueToken::ParentLabelRef(itkm!(54, 64, "foo::field", 0), 1)
        ]);
    }

    #[test]
    fn test_namespace_statement_nested() {
        let lexer = value_lexer("NAMESPACE foo NAMESPACE inner bar: DB\nENDNAMESPACE\nENDNAMESPACE\nfoo::inner::bar");
        assert_eq!(lexer.tokens, vec![
            ValueToken::ParentLabelDef(itk!(30, 34, "foo::inner::bar"), 1, None, false),
            ValueToken::Reserved(itk!(35, 37, "DB")),
            ValueToken::ParentLabelRef(itk!(64, 79, "foo::inner::bar"), 1)
        ]);
    }

    #[test]
    fn test_namespace_statement_error_lookup() {
        assert_eq!(
            value_lexer_error("foo::field"),
            "In file \"main.gb.s\" on line 1, column 1: Reference to unknown namespace \"foo\".\n\nfoo::field\n^--- Here".to_string()
        );
        assert_eq!(
            value_lexer_error("foo::field::bar"),
            "In file \"main.gb.s\" on line 1, column 1: Reference to unknown namespace \"foo\".\n\nfoo::field::bar\n^--- Here".to_string()
        );
    }

    #[test]
    fn test_namespace_statement_error_member_lookup() {
        assert_eq!(
            value_lexer_error("NAMESPACE foo ENDNAMESPACE\nfoo::bar"),
            "In file \"main.gb.s\" on line 2, column 6: Reference to unknown member \"bar\".\n\nfoo::bar\n     ^--- Here\n\nin namespace defined in file \"main.gb.s\" on line 1, column 11:\n\nNAMESPACE foo ENDNAMESPACE\n          ^--- Here".to_string()
        );
    }

    #[test]
    fn test_namespace_statement_error_member_lookup_nested() {
        assert_eq!(
            value_lexer_error("NAMESPACE foo NAMESPACE inner ENDNAMESPACE ENDNAMESPACE\nfoo::inner::bar"),
            "In file \"main.gb.s\" on line 2, column 13: Reference to unknown member \"bar\".\n\nfoo::inner::bar\n            ^--- Here\n\nin namespace defined in file \"main.gb.s\" on line 1, column 15:\n\nNAMESPACE foo NAMESPACE inner ENDNAMESPACE ENDNAMESPACE\n              ^--- Here".to_string()
        );
    }

    #[test]
    fn test_namespace_statement_error_duplicate() {
        assert_eq!(
            value_lexer_error("NAMESPACE foo ENDNAMESPACE NAMESPACE foo ENDNAMESPACE"),
            "In file \"main.gb.s\" on line 1, column 38: A namespace with the name \"foo\" was already defined.\n\nNAMESPACE foo ENDNAMESPACE NAMESPACE foo ENDNAMESPACE\n                                     ^--- Here\n\nOriginal definition of namespace was in file \"main.gb.s\" on line 1, column 11:\n\nNAMESPACE foo ENDNAMESPACE NAMESPACE foo ENDNAMESPACE\n          ^--- Here".to_string()
        );
    }

    #[test]
    fn test_namespace_statement_error_duplicate_member() {
        assert_eq!(
            value_lexer_error("NAMESPACE foo field: DB field: DB ENDNAMESPACE"),
            "In file \"main.gb.s\" on line 1, column 25: A member with the name \"field\" was already defined within the current namespace.\n\nNAMESPACE foo field: DB field: DB ENDNAMESPACE\n                        ^--- Here\n\nOriginal definition of member was in file \"main.gb.s\" on line 1, column 15:\n\nNAMESPACE foo field: DB field: DB ENDNAMESPACE\n              ^--- Here".to_string()
        );
    }

    #[test]
    fn test_namespace_statement_error_child_label() {
        assert_eq!(
            value_lexer_error("NAMESPACE foo field: DB .child: DB ENDNAMESPACE"),
            "In file \"main.gb.s\" on line 1, column 25: Unexpected definition of child label \"child\" without parent.\n\nNAMESPACE foo field: DB .child: DB ENDNAMESPACE\n                        ^--- Here".to_string()
        );
    }

}

