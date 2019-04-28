// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use gbc_cpu::{Flag, Register};
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{MacroStage, TokenValue};
use crate::error::SourceError;
use crate::expression::Operator;
use super::macros::{MacroCall, MacroToken, MacroTokenType, IfStatementBranch, ForStatement, BlockStatement};
use super::super::{LexerStage, InnerToken, TokenIterator, LexerToken};


// Value Specific Tokens ------------------------------------------------------
lexer_token!(ValueToken, ValueTokenType, (Debug, Eq, PartialEq), {
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
    IfStatement((Vec<IfStatementBranch<ValueToken>>)),
    ForStatement((ForStatement<ValueToken>)),
    BlockStatement((BlockStatement<ValueToken>)),
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
        let mut global_labels: HashMap<(TokenValue, Option<usize>), (InnerToken, usize)> = HashMap::new();
        let mut global_labels_names: Vec<TokenValue> = Vec::with_capacity(64);
        let mut unique_label_id = 0;
        Self::convert_local_labels_refs(Self::parse_tokens(
            &mut global_labels,
            &mut global_labels_names,
            &mut unique_label_id,
            false,
            tokens
        )?)
    }

}

impl ValueStage {

    fn parse_tokens(
        global_labels: &mut HashMap<(TokenValue, Option<usize>), (InnerToken, usize)>,
        global_labels_names: &mut Vec<TokenValue>,
        unique_label_id: &mut usize,
        is_argument: bool,
        tokens: Vec<MacroToken>

    ) -> Result<Vec<ValueToken>, SourceError> {

        let mut local_labels: HashMap<TokenValue, InnerToken> = HashMap::new();

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
                MacroToken::CloseBracket(inner) => ValueToken::CloseBracket(inner),

                // Convert Statements
                MacroToken::IfStatement(inner, branches) => {
                    let mut value_branches = Vec::with_capacity(branches.len());
                    for branch in branches {
                        value_branches.push(branch.into_other(|tokens| {
                            Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, tokens)
                        })?);
                    }
                    ValueToken::IfStatement(inner, value_branches)
                }
                MacroToken::ForStatement(inner, for_statement) => {
                    let mut binding = Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, vec![*for_statement.binding])?;
                    ValueToken::ForStatement(inner, ForStatement {
                        binding: Box::new(binding.remove(0)),
                        from: Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, for_statement.from)?,
                        to: Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, for_statement.to)?,
                        body: Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, for_statement.body)?
                    })
                },
                MacroToken::BlockStatement(inner, block) => {
                    ValueToken::BlockStatement(inner, match block {
                        BlockStatement::Using(cmd, body) => BlockStatement::Using(cmd, Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, body)?),
                        BlockStatement::Volatile(body) => BlockStatement::Volatile(Self::parse_tokens(global_labels, global_labels_names, unique_label_id, false, body)?)
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
                    if tokens.peek_is(MacroTokenType::Colon, None) {
                        let colon = tokens.expect(MacroTokenType::Colon, None, "when parsing global label definition")?.into_inner();
                        let label_id = Self::global_label_id(&inner, false);
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
                MacroToken::Point(inner) => Self::parse_local_label(
                    &mut tokens,
                    global_labels,
                    global_labels_names,
                    unique_label_id,
                    is_argument,
                    &mut local_labels,
                    inner
                )?,
                MacroToken::Offset(inner) => ValueToken::Offset {
                    value: Self::parse_integer(&inner, 0, 10)?,
                    inner
                },
                MacroToken::NumberLiteral(inner) => Self::parse_number_literal(inner)?,
                MacroToken::StringLiteral(inner) => ValueToken::String {
                    value: inner.value.to_string(),
                    inner
                },
                MacroToken::Colon(inner) => {
                    return Err(inner.error(format!("Unexpected standalone \"{}\", expected a \"Name\" token to preceed it.", inner.value)))
                },
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

        // Convert name tokens into corresponding global label references
        Ok(Self::convert_global_label_refs(&global_labels, value_tokens))
    }

    fn convert_global_label_refs(
        global_labels: &HashMap<(TokenValue, Option<usize>), (InnerToken, usize)>,
        tokens: Vec<ValueToken>

    ) -> Vec<ValueToken> {
        tokens.into_iter().map(|token| {
            if let ValueToken::Name(inner) = token {

                // Generate references to global labels
                let label_id = Self::global_label_id(&inner, true);
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

    fn global_label_id(inner: &InnerToken, global_only: bool) -> (TokenValue, Option<usize>) {
        let name = if global_only || inner.macro_call_id.is_none() {
            inner.value.to_string()

        } else if let Some(call_id) = inner.macro_call_id() {
            // Postfix labels created by macros calls so they are unique
            format!("{}_from_macro_call_{}", inner.value, call_id)

        } else {
            unreachable!()
        };

        if name.starts_with('_') {
            // Handle file local global labels that are prefixed with _
            (TokenValue::from(format!("{}_file_local_{}", name, inner.file_index)), Some(inner.file_index))

        } else {
            (TokenValue::from(name), None)
        }
    }

    fn parse_local_label(
        tokens: &mut TokenIterator<MacroToken>,
        global_labels: &mut HashMap<(TokenValue, Option<usize>), (InnerToken, usize)>,
        global_labels_names: &mut Vec<TokenValue>,
        unique_label_id: &mut usize,
        is_argument: bool,
        local_labels: &mut HashMap<TokenValue, InnerToken>,
        mut inner: InnerToken

    ) -> Result<ValueToken, SourceError> {
        // For local labels all kinds of names are allowed
        let name_token = if tokens.peek_is(MacroTokenType::Instruction, None) {
            tokens.expect(MacroTokenType::Instruction, None, "when parsing local label")?.into_inner()

        } else if tokens.peek_is(MacroTokenType::Reserved, None) {
            tokens.expect(MacroTokenType::Reserved, None, "when parsing local label")?.into_inner()

        } else {
            tokens.expect(MacroTokenType::Name, None, "when parsing local label")?.into_inner()
        };

        // Postfix labels created by macros calls so they are unique
        let name = if let Some(call_id) = name_token.macro_call_id() {
            TokenValue::from(format!("{}_from_macro_call_{}", name_token.value, call_id))

        } else {
            TokenValue::from(name_token.value.to_string())
        };

        if tokens.peek_is(MacroTokenType::Colon, None) {
            let colon = tokens.expect(MacroTokenType::Colon, None, "when parsing local label definition")?.into_inner();
            if global_labels.is_empty() {
                Err(inner.error(format!(
                    "Unexpected definition of local label \"{}\" before any global label was defined.",
                    name_token.value
                )))

            } else if let Some(previous) = local_labels.get(&name) {
                Err(inner.error(format!(
                    "Local label \"{}\" was already defined under the current global label \"{}\".",
                    name,
                    global_labels_names.last().unwrap()

                )).with_reference(previous, "Original definition of local label was"))

            } else if is_argument {
                Err(inner.error("Local label cannot be defined inside an argument list".to_string()))

            } else {
                inner.end_index = colon.end_index;
                inner.value = name.clone();
                local_labels.insert(name.clone(), inner.clone());

                *unique_label_id += 1;
                Ok(ValueToken::LocalLabelDef(inner, *unique_label_id))
            }

        } else if global_labels.is_empty() {
            Err(inner.error(format!(
                "Unexpected reference to local label \"{}\" before any global label was defined.",
                name_token.value
            )))

        } else {
            inner.value = name.clone();
            inner.end_index = name_token.end_index;
            Ok(ValueToken::LocalLabelRef(inner, 0))
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


// Local Label Reference Handling ---------------------------------------------
#[derive(Debug, Clone)]
enum LocalLabelRef {
    Global {
        index: usize,
        target: usize
    },
    InsideBranch {
        index: usize,
        arg_index: usize,
        inner_index: usize,
        target: usize
    },
    InsideBody {
        index: usize,
        inner_index: usize,
        target: usize
    }
}

type GlobalLabelEntry = Option<(usize, Vec<(TokenValue, usize)>, Vec<(TokenValue, usize, LocalCallIndex, Option<usize>)>)>;
type LocalCallIndex = Option<(usize, usize)>;
type LocalLabelError = (usize, usize, LocalCallIndex);

impl ValueStage {
    fn convert_local_labels_refs(mut tokens: Vec<ValueToken>) -> Result<Vec<ValueToken>, SourceError> {

        let mut global_label_map = HashMap::new();
        let mut local_label_refs = Vec::with_capacity(64);
        let mut error = Self::inner_assign_and_verify_local_label_refs(
            &mut tokens,
            &mut global_label_map,
            &mut local_label_refs,
            true,
            None,
            None
        );

        // Verify any open global label scopes
        for (_, mut global_label) in global_label_map.drain() {
            if let Some(e) = Self::verify_local_label_refs_under_global(global_label.take(), &mut local_label_refs) {
                error = Some(e);
                break;
            }
        }

        // Handle Errors
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

        // Set target label ID of all local label references
        for r in local_label_refs.drain(0..) {
            match r {
                LocalLabelRef::Global { index, target } => {
                    if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = tokens.get_mut(index) {
                        *id = target;
                        continue;
                    }
                },
                LocalLabelRef::InsideBranch { index, arg_index, inner_index, target} => {
                    if let Some(ValueToken::BuiltinCall(_, ref mut arguments)) = tokens.get_mut(index) {
                        if let Some(arg_tokens) = arguments.get_mut(arg_index) {
                            if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = arg_tokens.get_mut(inner_index) {
                                *id = target;
                                continue;
                            }
                        }

                    } else if let Some(ValueToken::IfStatement(_, ref mut if_branches)) = tokens.get_mut(index) {
                        if let Some(branch) = if_branches.get_mut(arg_index) {
                            if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = branch.body.get_mut(inner_index) {
                                *id = target;
                                continue;
                            }
                        }
                    }
                },
                LocalLabelRef::InsideBody { index, inner_index, target} => {
                    if let Some(ValueToken::ForStatement(_, ref mut for_statement)) = tokens.get_mut(index) {
                        if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = for_statement.body.get_mut(inner_index) {
                            *id = target;
                            continue;
                        }

                    } else if let Some(ValueToken::BlockStatement(_, ref mut block)) = tokens.get_mut(index) {
                        match block {
                            BlockStatement::Using(_, body) | BlockStatement::Volatile(body) => {
                                if let Some(ValueToken::LocalLabelRef(_, ref mut id)) = body.get_mut(inner_index) {
                                    *id = target;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
            unreachable!("Invalid local label ref generated: {:?}", r);
        }

        Ok(tokens)
    }

    fn inner_assign_and_verify_local_label_refs(
        tokens: &mut [ValueToken],
        global_label_map: &mut HashMap<Option<usize>, GlobalLabelEntry>,
        local_label_refs: &mut Vec<LocalLabelRef>,
        global_def_allowed: bool,
        call_parent: LocalCallIndex,
        stmt_parent: Option<usize>

    ) -> Option<LocalLabelError> {
        for (index, token) in tokens.iter_mut().enumerate() {
            match token {
                ValueToken::GlobalLabelDef(inner, _) if global_def_allowed => {
                    let global_label = global_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(error) = Self::verify_local_label_refs_under_global(global_label.take(), local_label_refs) {
                        return Some(error);
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
                        global_label.2.push((inner.value.clone(), index, call_parent, stmt_parent));
                    }
                },
                ValueToken::BuiltinCall(_, arguments) => {
                    for (arg_index, arg_tokens) in arguments.iter_mut().enumerate() {
                        if let Some(error) = Self::inner_assign_and_verify_local_label_refs(
                            arg_tokens,
                            global_label_map,
                            local_label_refs,
                            false,
                            Some((index, arg_index)),
                            None
                        ) {
                            return Some(error);
                        }
                    }
                },
                ValueToken::ForStatement(_, for_statement) => {
                    if let Some(error) = Self::inner_assign_and_verify_local_label_refs(
                        &mut for_statement.body,
                        global_label_map,
                        local_label_refs,
                        true,
                        None,
                        Some(index)
                    ) {
                        return Some(error);
                    }
                },
                ValueToken::IfStatement(_, if_branches) => {
                    for (branch_index, branch) in if_branches.iter_mut().enumerate() {
                        if let Some(error) = Self::inner_assign_and_verify_local_label_refs(
                            &mut branch.body,
                            global_label_map,
                            local_label_refs,
                            true,
                            Some((index, branch_index)),
                            None
                        ) {
                            return Some(error);
                        }
                    }
                },
                ValueToken::BlockStatement(_, block) => {
                    match block {
                        BlockStatement::Using(_, body) | BlockStatement::Volatile(body) => {
                            if let Some(error) = Self::inner_assign_and_verify_local_label_refs(
                                body,
                                global_label_map,
                                local_label_refs,
                                true,
                                None,
                                Some(index)
                            ) {
                                return Some(error);
                            }
                        }
                    }
                },
                _ => {}
            }
        }
        None
    }

    fn verify_local_label_refs_under_global(
        global_label: GlobalLabelEntry,
        local_label_refs: &mut Vec<LocalLabelRef>

    ) -> Option<LocalLabelError> {
        if let Some((previous_index, local_defs, local_refs)) = global_label {
            for (ref_name, token_index, call_parent, stmt_parent) in &local_refs {
                let mut label_exists = false;
                for (def_name, label_id) in &local_defs {
                    if def_name == ref_name {
                        label_exists = true;
                        if let Some((index, arg_index)) = call_parent {
                            local_label_refs.push(LocalLabelRef::InsideBranch {
                                index: *index,
                                arg_index: *arg_index,
                                inner_index: *token_index,
                                target: *label_id
                            });

                        } else if let Some(index) = stmt_parent {
                            local_label_refs.push(LocalLabelRef::InsideBody {
                                index: *index,
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
                    return Some((*token_index, previous_index, *call_parent));
                }
            }
        }
        None
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
    fn test_macro_arg_global_label_ref() {
        assert_eq!(tfv("global:\nMACRO FOO(@a) DB @a ENDMACRO\nFOO(global)"), vec![
            ValueToken::GlobalLabelDef(
                itk!(0, 7, "global"), 1
            ),
            vtkm!(Reserved, 22, 24, "DB", 0),
            ValueToken::GlobalLabelRef(
                itkm!(41, 47, "global", 0),
                1
            )
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

    // If Statements ----------------------------------------------------------
    #[test]
    fn test_if_statment_forwarding() {
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

    // FOR Statements ---------------------------------------------------------
    #[test]
    fn test_for_statment_forwarding() {
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
        let lexer = value_lexer("BLOCK VOLATILE\nglobal:\n.local:\nDB .local\nENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            ValueToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Volatile(
                vec![
                    ValueToken::GlobalLabelDef(itk!(15, 22, "global"), 1),
                    ValueToken::LocalLabelDef(itk!(23, 30, "local"), 2),
                    ValueToken::Reserved(itk!(31, 33, "DB")),
                    ValueToken::LocalLabelRef(itk!(34, 40, "local"), 2)
                ]
            ))
        ]);
    }

    #[test]
    fn test_label_ref_if_statement() {
        let lexer = value_lexer("IF foo THEN\nglobal:\n.local:\nDB .local\nENDIF");
        assert_eq!(lexer.tokens, vec![
            ValueToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![ValueToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![
                        ValueToken::GlobalLabelDef(itk!(12, 19, "global"), 1),
                        ValueToken::LocalLabelDef(itk!(20, 27, "local"), 2),
                        ValueToken::Reserved(itk!(28, 30, "DB")),
                        ValueToken::LocalLabelRef(itk!(31, 37, "local"), 2)
                    ]
                }
            ])
        ]);
    }

    #[test]
    fn test_label_ref_for_statement() {
        let lexer = value_lexer("FOR x IN 0 TO 10 REPEAT\nglobal:\n.local:\nDB .local\nENDFOR");
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
                    ValueToken::GlobalLabelDef(itk!(24, 31, "global"), 1),
                    ValueToken::LocalLabelDef(itk!(32, 39, "local"), 2),
                    ValueToken::Reserved(itk!(40, 42, "DB")),
                    ValueToken::LocalLabelRef(itk!(43, 49, "local"), 2)
                ]
            })
        ]);
    }

}

