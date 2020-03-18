// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::IncludeStage;
use crate::error::SourceError;
use crate::expression::ExpressionArgumenType;
use super::include::{IncludeToken, IncludeType};
use super::super::{LexerStage, InnerToken, TokenIterator, LexerToken, Symbol};


// Statics --------------------------------------------------------------------
const MAX_EXPANSION_DEPTH: usize = 8;

lazy_static! {
    pub static ref BUILTIN_MACRO_DEFS: Vec<MacroDefinition> = vec![
        // Noop
        MacroDefinition::builtin("DBG", vec![]),

        // Math
        MacroDefinition::builtin("MAX", vec![(ExpressionArgumenType::Number, "a"), (ExpressionArgumenType::Number, "b")]),
        MacroDefinition::builtin("MIN", vec![(ExpressionArgumenType::Number, "a"), (ExpressionArgumenType::Number, "b")]),
        MacroDefinition::builtin("FLOOR", vec![(ExpressionArgumenType::Number, "value")]),
        MacroDefinition::builtin("CEIL", vec![(ExpressionArgumenType::Number, "value")]),
        MacroDefinition::builtin("ROUND", vec![(ExpressionArgumenType::Number, "value")]),

        MacroDefinition::builtin("LOG", vec![(ExpressionArgumenType::Number, "value")]),
        MacroDefinition::builtin("EXP", vec![(ExpressionArgumenType::Number, "value")]),
        MacroDefinition::builtin("SQRT", vec![(ExpressionArgumenType::Number, "value")]),
        MacroDefinition::builtin("ABS", vec![(ExpressionArgumenType::Number, "value")]),
        // MacroDefinition::builtin("RAND", vec![(ExpressionArgumenType::Number, "from"), (ExpressionArgumenType::Number, "to")]),

        // String Operations
        MacroDefinition::builtin("STRUPR", vec![(ExpressionArgumenType::String, "text")]),
        MacroDefinition::builtin("STRLWR", vec![(ExpressionArgumenType::String, "text")]),
        MacroDefinition::builtin("STRLEN", vec![(ExpressionArgumenType::String, "text")]),
        MacroDefinition::builtin("STRSUB", vec![(ExpressionArgumenType::String, "text"), (ExpressionArgumenType::Integer, "index"), (ExpressionArgumenType::Integer, "length")]),
        MacroDefinition::builtin("STRIN", vec![(ExpressionArgumenType::String, "text"), (ExpressionArgumenType::String, "search")]),
        MacroDefinition::builtin("STRPADR", vec![(ExpressionArgumenType::String, "text"), (ExpressionArgumenType::String, "padding"), (ExpressionArgumenType::Integer, "length")]),
        MacroDefinition::builtin("STRPADL", vec![(ExpressionArgumenType::String, "text"), (ExpressionArgumenType::String, "padding"), (ExpressionArgumenType::Integer, "length")]),

        // Geometry
        MacroDefinition::builtin("SIN", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("COS", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("TAN", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("ASIN", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("ACOS", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("ATAN", vec![(ExpressionArgumenType::Number, "radians")]),
        MacroDefinition::builtin("ATAN2", vec![(ExpressionArgumenType::Number, "y"), (ExpressionArgumenType::Number, "x")])
    ];
    pub static ref BUILTIN_MACRO_INDEX: HashMap<Symbol, usize> = {
        let mut map = HashMap::with_capacity(BUILTIN_MACRO_DEFS.len());
        for (index, def) in BUILTIN_MACRO_DEFS.iter().enumerate() {
            map.insert(def.name.value.clone(), index);
        }
        map
    };

}


// Parsing Namespaces  --------------------------------------------------------
struct MacroContext<'t> {
    def: &'t MacroDefinition,
    args: Vec<Vec<IncludeToken>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfStatementBranch<T: LexerToken> {
    pub condition: Option<Vec<T>>,
    pub body: Vec<T>
}

impl<T: LexerToken> IfStatementBranch<T> {
    pub fn into_other<U: LexerToken, C: FnMut(Vec<T>) -> Result<Vec<U>, SourceError>>(self, mut convert: C) -> Result<IfStatementBranch<U>, SourceError> {
        Ok(IfStatementBranch {
            condition: if let Some(condition) = self.condition {
                Some(convert(condition)?)

            } else {
                None
            },
            body: convert(self.body)?
        })
    }
}

enum IfBranch {
    None,
    Else,
    If
}

#[derive(Debug, Eq, PartialEq)]
pub struct ForStatement<T: LexerToken> {
    pub binding: Box<T>,
    pub from: Vec<T>,
    pub to: Vec<T>,
    pub body: Vec<T>
}

#[derive(Debug, Eq, PartialEq)]
pub enum BlockStatement<T: LexerToken> {
    Using(String, Vec<T>),
    Volatile(Vec<T>)
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamespaceStatement<T: LexerToken> {
    pub name: Box<T>,
    pub body: Vec<T>
}

// Macro Specific Tokens ------------------------------------------------------
lexer_token!(MacroToken, MacroTokenType, (Debug, Eq, PartialEq), {
    Name(()),
    GlobalName(()),
    Reserved(()),
    Segment(()),
    Register(()),
    Flag(()),
    Instruction(()),
    MetaInstruction(()),
    Offset(()),
    NumberLiteral(()),
    StringLiteral(()),
    BinaryFile((Vec<u8>)),
    BuiltinCall((Vec<Vec<MacroToken>>)),
    ParentLabelCall((Vec<Vec<MacroToken>>)),
    IfStatement((Vec<IfStatementBranch<MacroToken>>)),
    ForStatement((ForStatement<MacroToken>)),
    BlockStatement((BlockStatement<MacroToken>)),
    NamespaceStatement((NamespaceStatement<MacroToken>)),
    Lookup((Vec<MacroToken>)),
    Comma(()),
    Point(()),
    Colon(()),
    Operator(()),
    OpenParen(()),
    CloseParen(()),
    OpenBracket(()),
    CloseBracket(())
});

impl From<IncludeToken> for MacroToken {
    fn from(token: IncludeToken) -> Self {
        match token {
            IncludeToken::Name(inner) => MacroToken::Name(inner),
            IncludeToken::Reserved(inner) => MacroToken::Reserved(inner),
            IncludeToken::Segment(inner) => MacroToken::Segment(inner),
            IncludeToken::Register(inner) => MacroToken::Register(inner),
            IncludeToken::Flag(inner) => MacroToken::Flag(inner),
            IncludeToken::Instruction(inner) => MacroToken::Instruction(inner),
            IncludeToken::MetaInstruction(inner) => MacroToken::MetaInstruction(inner),
            IncludeToken::Offset(inner) => MacroToken::Offset(inner),
            IncludeToken::NumberLiteral(inner) => MacroToken::NumberLiteral(inner),
            IncludeToken::StringLiteral(inner) => MacroToken::StringLiteral(inner),
            IncludeToken::BinaryFile(inner, bytes) => MacroToken::BinaryFile(inner, bytes),
            IncludeToken::BuiltinCall(inner, args) => MacroToken::BuiltinCall(
                inner,
                args.into_iter().map(|tokens| {
                    tokens.into_iter().map(MacroToken::from).collect()

                }).collect()
            ),
            IncludeToken::ParentLabelCall(inner, args) => MacroToken::ParentLabelCall(
                inner,
                args.into_iter().map(|tokens| {
                    tokens.into_iter().map(MacroToken::from).collect()

                }).collect()
            ),
            IncludeToken::Comma(inner) => MacroToken::Comma(inner),
            IncludeToken::Point(inner) => MacroToken::Point(inner),
            IncludeToken::Colon(inner) => MacroToken::Colon(inner),
            IncludeToken::Operator(inner) => MacroToken::Operator(inner),
            IncludeToken::OpenParen(inner) => MacroToken::OpenParen(inner),
            IncludeToken::CloseParen(inner) => MacroToken::CloseParen(inner),
            IncludeToken::OpenBracket(inner) => MacroToken::OpenBracket(inner),
            IncludeToken::CloseBracket(inner) => MacroToken::CloseBracket(inner),
            token => {
                unreachable!("Token {:?} may not be passed through MacroLexer", token)
            }
        }
    }
}


// Macro Definitions ----------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub struct MacroDefinition {
    name: InnerToken,
    pub parameters: Vec<(ExpressionArgumenType, InnerToken)>,
    body: Vec<IncludeToken>,
    file_index: Option<usize>,
    is_builtin: bool,
    is_exported: bool,
    is_label: bool
}

impl MacroDefinition {
    fn builtin(name: &str, parameters: Vec<(ExpressionArgumenType, &str)>) -> Self {
        Self {
            name: InnerToken::new(0, 0, 0, name.into()),
            parameters: parameters.into_iter().map(|(typ, name)| {
                (typ, InnerToken::new(0, 0, 0, name.into()))

            }).collect(),
            body: Vec::with_capacity(64),
            file_index: None,
            is_builtin: true,
            is_exported: true,
            is_label: false
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct MacroCall {
    id: usize,
    name: InnerToken,
    arguments: Vec<Vec<IncludeToken>>
}

impl MacroCall {
    pub fn error(&self, message: String) -> SourceError {
        self.name.error(message)
    }
}


// Macro Level Lexer Implementation -------------------------------------------
pub struct MacroStage;
impl LexerStage for MacroStage {

    type Input = IncludeStage;
    type Output = MacroToken;
    type Data = MacroDefinition;

    fn from_tokens(
        tokens: Vec<<Self::Input as LexerStage>::Output>,
        macro_calls: &mut Vec<MacroCall>,
        data: &mut Vec<Self::Data>,
        _linter_enabled: bool

    ) -> Result<Vec<Self::Output>, SourceError> {
        let (mut macro_defs, tokens) = Self::parse_tokens(tokens, macro_calls)?;
        data.append(&mut macro_defs);
        Ok(tokens)
    }

}

impl MacroStage {

    fn parse_tokens(
        tokens: Vec<IncludeToken>,
        macro_calls: &mut Vec<MacroCall>

    ) -> Result<(Vec<MacroDefinition>, Vec<MacroToken>), SourceError> {

        let mut user_macro_defs = Vec::with_capacity(16);
        let mut tokens_without_macro_defs = Vec::with_capacity(tokens.len());

        // Drop all newlines
        let mut tokens = TokenIterator::new(tokens);

        // Extract Macro Definitions
        while let Some(token) = tokens.next() {
            // ENDMACRO
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDMACRO) {
                return Err(token.error(format!("Unexpected \"{}\" token outside of macro definition.", token.symbol())));

            // MACRO
            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::MACRO) {
                Self::parse_user_macro_definition(&mut tokens, &mut user_macro_defs, false)?;

            // GLOBAL MACRO
            } else if token.is(IncludeType::Reserved)
                   && token.is_symbol(Symbol::GLOBAL)
                   && tokens.peek_is(IncludeType::Reserved, Some(Symbol::MACRO)) {

                tokens.expect(IncludeType::Reserved, Some(Symbol::MACRO), "when parsing macro definition")?;
                Self::parse_user_macro_definition(&mut tokens, &mut user_macro_defs, true)?;

            } else {
                tokens_without_macro_defs.push(token);
            }
        }

        // Recursively expand all Macro Calls
        let mut macro_call_id = 0;
        let tokens_without_macro_calls = Self::expand_macro_calls(
            tokens_without_macro_defs,
            &mut macro_call_id,
            0,
            macro_calls,
            &BUILTIN_MACRO_DEFS,
            &mut user_macro_defs
        )?;

        // Check for any left over parameter or token groups
        for token in &tokens_without_macro_calls {
            if token.is(IncludeType::Parameter) {
                return Err(token.error(format!(
                    "Unexpected parameter \"{}\" outside of macro expansion.",
                    token.symbol()
                )));

            } else if token.is(IncludeType::TokenGroup) {
                return Err(token.error(
                    "Unexpected token group outside of macro expansion.".to_string()
                ));
            }
        }

        let tokens_with_statements = Self::parse_statements(
            tokens_without_macro_calls
        )?;

        Ok((
            user_macro_defs,
            tokens_with_statements
        ))

    }

    fn parse_user_macro_definition(
        tokens: &mut TokenIterator<IncludeToken>,
        user_macro_defs: &mut Vec<MacroDefinition>,
        is_exported: bool

    ) -> Result<(), SourceError> {

        // Verify Macro Name
        let name_token = tokens.expect(IncludeType::Name, None, "when parsing macro definition")?;

        // Check for shadowing of builtin
        if Self::get_macro_by_name(&BUILTIN_MACRO_DEFS, name_token.symbol(), None).is_some() {
            return Err(name_token.error(format!("Re-definition of builtin macro \"{}\".", name_token.symbol())));

        // Check for shadowing of macro in same file
        } else if let Some(user_def) = Self::get_macro_by_name(&user_macro_defs, name_token.symbol(), Some(name_token.inner().file_index)) {
            return Err(name_token.error(
                format!("Re-definition of non-global user macro \"{}\".", name_token.symbol())

            ).with_reference(&user_def.name, "Original definition was"));
        }

        // Check for shadowing of macro global macro
        if is_exported {
            if let Some(user_def) = Self::get_macro_by_name(&user_macro_defs, name_token.symbol(), None) {
                return Err(name_token.error(
                    format!("Re-definition of global user macro \"{}\".", name_token.symbol())

                ).with_reference(&user_def.name, "Original definition was"));
            }
        }

        // Parse Macro Parameter list
        let param_tokens = Self::parse_macro_def_arguments(tokens)?;
        let parameters: Vec<(ExpressionArgumenType, InnerToken)> = param_tokens.into_iter().map(|t| (ExpressionArgumenType::Any, t.into_inner())).collect();

        // Check against duplicate names
        let mut param_names: HashSet<Symbol> = HashSet::new();
        for (_, arg) in &parameters {
            if param_names.contains(&arg.value) {
                return Err(arg.error(format!("Duplicate macro parameter \"{}\", a parameter with the same name was already defined.", arg.value)));

            } else {
                param_names.insert(arg.value.clone());
            }
        }

        // Collect Body Tokens
        let mut body_tokens = Vec::with_capacity(128);
        while !tokens.peek_is(IncludeType::Reserved, Some(Symbol::ENDMACRO)) {
            let token = tokens.get("Unexpected end of input while parsing macro body.")?;
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::MACRO) {
                return Err(token.error("Invalid nested macro definition.".to_string()));

            } else {
                body_tokens.push(token);
            }
        }
        tokens.expect(IncludeType::Reserved, Some(Symbol::ENDMACRO), "when parsing macro definition")?;

        // Add Macro Definition
        let inner = name_token.into_inner();
        user_macro_defs.push(MacroDefinition {
            parameters,
            body: body_tokens,
            file_index: if is_exported {
                None

            } else {
                Some(inner.file_index)
            },
            name: inner,
            is_builtin: false,
            is_exported,
            is_label: false
        });
        Ok(())

    }

    fn parse_statements(tokens: Vec<IncludeToken>) -> Result<Vec<MacroToken>, SourceError> {
        let mut tokens_with_statements = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {

            // Parse namespacxe statements
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDNAMESPACE) {
                return Err(token.error(format!("Unexpected \"{}\" token outside of NAMESPACE statement.", token.symbol())));

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::NAMESPACE) {
                let inner = token.inner().clone();
                let name = MacroToken::from(tokens.expect(IncludeType::Name, None, "when parsing NAMESPACE statement")?);

                let body = Self::parse_namespace_body(&mut tokens)?;
                tokens_with_statements.push(MacroToken::NamespaceStatement(inner, NamespaceStatement {
                    name: Box::new(name),
                    body
                }));

            // Parse IF Statements
            } else if token.is(IncludeType::Reserved) && (token.is_symbol(Symbol::THEN) || token.is_symbol(Symbol::ELSE) || token.is_symbol(Symbol::ENDIF)) {
                return Err(token.error(format!("Unexpected \"{}\" token outside of IF statement.", token.symbol())));

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::IF) {
                let inner = token.inner().clone();
                let mut branches: Vec<IfStatementBranch<MacroToken>> = Vec::with_capacity(2);
                let mut condition_tokens = Some(Self::parse_if_condition(&mut tokens, token)?);
                loop {
                    let (body_tokens, branch) = Self::parse_if_body(&mut tokens)?;
                    branches.push(IfStatementBranch {
                        condition: condition_tokens.take(),
                        body: body_tokens
                    });
                    match branch {
                        IfBranch::None => break,
                        IfBranch::Else => continue,
                        IfBranch::If => {
                            let token = tokens.expect(IncludeType::Reserved, Some(Symbol::IF), "when parsing IF statement")?;
                            condition_tokens = Some(Self::parse_if_condition(&mut tokens, token)?);
                        }
                    }
                }
                tokens_with_statements.push(MacroToken::IfStatement(inner, branches));

            // Parse REPEAT Statements
            } else if token.is(IncludeType::Reserved) && (token.is_symbol(Symbol::IN) || token.is_symbol(Symbol::TO) || token.is_symbol(Symbol::REPEAT) || token.is_symbol(Symbol::ENDFOR)) {
                return Err(token.error(format!("Unexpected \"{}\" token outside of FOR statement.", token.symbol())));

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::FOR) {
                let inner = token.into_inner();
                let binding = MacroToken::from(tokens.expect(IncludeType::Name, None, "when parsing FOR statement")?);

                let token = tokens.expect(IncludeType::Reserved, Some(Symbol::IN), "when parsing FOR statement")?;
                let from = Self::parse_for_range(&mut tokens, token, Symbol::TO)?;

                let token = tokens.expect(IncludeType::Reserved, Some(Symbol::TO), "when parsing FOR statement range")?;
                let to = Self::parse_for_range(&mut tokens, token, Symbol::REPEAT)?;

                tokens.expect(IncludeType::Reserved, Some(Symbol::REPEAT), "when parsing FOR statement range")?;
                let body = Self::parse_for_body(&mut tokens)?;
                tokens_with_statements.push(MacroToken::ForStatement(inner, ForStatement {
                    binding: Box::new(binding),
                    from,
                    to,
                    body,
                }));

            // Parse BLOCKS
            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDBLOCK) {
                return Err(token.error(format!("Unexpected \"{}\" token outside of BLOCK statement.", token.symbol())));

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::BLOCK) {
                if tokens.peek_is(IncludeType::Reserved, Some(Symbol::USING)) {
                    tokens.expect(IncludeType::Reserved, Some(Symbol::USING), "when parsing USING BLOCK")?;
                    let command = tokens.expect(IncludeType::StringLiteral, None, "when parsing USING BLOCK")?;
                    let body = Self::parse_block_body(&mut tokens)?;
                    tokens_with_statements.push(MacroToken::BlockStatement(
                        token.into_inner(),
                        BlockStatement::Using(command.into_inner().value.to_string(), body)
                    ));

                } else if tokens.peek_is(IncludeType::Reserved, Some(Symbol::VOLATILE)) {
                    tokens.expect(IncludeType::Reserved, Some(Symbol::VOLATILE), "when parsing VOLATILE BLOCK")?;
                    let body = Self::parse_block_body(&mut tokens)?;
                    tokens_with_statements.push(MacroToken::BlockStatement(
                        token.into_inner(),
                        BlockStatement::Volatile(body)
                    ));

                } else {
                    return Err(token.error("Expected either a USING or VOLATILE keyword to BLOCK directive.".to_string()));
                }

            // Parse Namespace Access
            } else if token.is(IncludeType::Name) && tokens.peek_is(IncludeType::Member, None) {
                let mut members = Vec::with_capacity(4);
                let mut member = token;
                let inner = member.inner().clone();
                while tokens.peek_is(IncludeType::Member, None) {
                    members.push(MacroToken::from(member));
                    tokens.expect(IncludeType::Member, None, "when parsing namespace member access")?;

                    if tokens.peek_is(IncludeType::ParentLabelCall, None) {
                        member = tokens.expect(IncludeType::ParentLabelCall, None, "when parsing namespace member access")?;
                        break;

                    } else {
                        member = tokens.expect(IncludeType::Name, None, "when parsing namespace member access")?;
                    }
                }
                members.push(MacroToken::from(member));
                tokens_with_statements.push(MacroToken::Lookup(inner, members));

            } else if token.is(IncludeType::Member) {
                return Err(token.error("Incomplete namespace member access.".to_string()));

            } else {
                tokens_with_statements.push(MacroToken::from(token));
            }
        }
        Ok(tokens_with_statements)
    }

    fn expand_macro_calls(
        tokens: Vec<IncludeToken>,
        macro_call_id: &mut usize,
        expansion_depth: usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &mut Vec<MacroDefinition>,

    ) -> Result<Vec<IncludeToken>, SourceError> {

        let mut tokens_without_macro_calls = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        let mut has_global_prefix = false;
        while let Some(token) = tokens.next() {

            if token.is(IncludeType::Name) && tokens.peek_is(IncludeType::OpenParen, None) {

                // Builtin
                let macro_def = if let Some(def) = Self::get_macro_by_name(&builtin_macro_defs, token.symbol(), None) {
                    def

                // File local lookup
                } else if let Some(def) = Self::get_macro_by_name(&user_macro_defs, token.symbol(), Some(token.inner().file_index)) {
                    def

                // Global lookup
                } else if let Some(def) = Self::get_macro_by_name(&user_macro_defs, token.symbol(), None) {
                    def

                // Parent Label with Optional Arguments or undefined Macro
                } else {

                    // Collect raw argument tokens
                    let mut argument_tokens = Vec::with_capacity(8);
                    while let Some(token) = tokens.next() {
                        if token.is(IncludeType::CloseParen) {
                            argument_tokens.push(token);
                            break;

                        } else {
                            argument_tokens.push(token);
                        }
                    }

                    // Check for label definition and re-insert arguments tokens into stream
                    if tokens.peek_is(IncludeType::Colon, None) {
                        tokens_without_macro_calls.push(token.clone());
                        tokens_without_macro_calls.append(&mut argument_tokens);

                        // Define callable label
                        let inner = token.into_inner();
                        user_macro_defs.push(MacroDefinition {
                            parameters: Vec::new(),
                            body: Vec::new(),
                            file_index: if has_global_prefix {
                                None

                            } else {
                                Some(inner.file_index)
                            },
                            name: inner,
                            is_builtin: false,
                            is_exported: has_global_prefix,
                            is_label: true
                        });
                        continue;
                    }

                    // Not found
                    let error = token.error(format!("Invocation of undefined macro \"{}\"", token.symbol()));
                    if let Some(def) = Self::get_macro_by_name_all(&user_macro_defs, token.symbol()) {
                        return Err(error.with_reference(&def.name, "A non-global macro with the same name is defined"));

                    } else {
                        return Err(error);
                    }
                };

                let arg_tokens = Self::parse_macro_call_arguments(&mut tokens)?;
                if !macro_def.is_label && arg_tokens.len() != macro_def.parameters.len() {
                    return Err(token.error(format!(
                        "Incorrect number of parameters for invocation of macro \"{}\", expected {} parameter(s) but got {}.",
                        token.symbol(),
                        macro_def.parameters.len(),
                        arg_tokens.len()
                    )));
                }

                // Add Macro Call
                macro_calls.push(MacroCall {
                    id: *macro_call_id,
                    name: token.clone().into_inner(),
                    arguments: arg_tokens.clone()
                });

                // Replace callable labels
                if macro_def.is_label {
                    tokens_without_macro_calls.push(
                        Self::parse_label_call(
                            token.into_inner(),
                            arg_tokens,
                            macro_call_id,
                            expansion_depth,
                            macro_calls,
                            builtin_macro_defs,
                            user_macro_defs
                        )?
                    );
                    *macro_call_id += 1;

                // Replace builtin calls
                } else if macro_def.is_builtin {
                    tokens_without_macro_calls.push(
                        Self::parse_builtin_call(
                            token.into_inner(),
                            arg_tokens,
                            macro_call_id,
                            expansion_depth,
                            macro_calls,
                            builtin_macro_defs,
                            user_macro_defs
                        )?
                    );
                    *macro_call_id += 1;

                // Expand user calls
                } else {
                    let context = MacroContext {
                        def: &macro_def,
                        args: arg_tokens
                    };

                    // Recursively expand all body tokens
                    let mut expanded_macro_tokens = Vec::with_capacity(macro_def.body.len());
                    for token in macro_def.body.clone() {
                        Self::expand_macro_token(
                            &context,
                            token,
                            macro_call_id,
                            &mut expanded_macro_tokens
                        )?
                    }

                    // Prevent stack overflow with deep recursive macros
                    if expansion_depth >= MAX_EXPANSION_DEPTH {
                        let last_macro_call = macro_calls.last().unwrap();
                        return Err(last_macro_call.name.error(format!(
                            "Maximum recursion limit of {} reached during expansion of macro \"{}\".",
                            MAX_EXPANSION_DEPTH,
                            last_macro_call.name.value
                        )));
                    }

                    *macro_call_id += 1;

                    // Recursively expand any macros calls generate by the previous user macro call
                    tokens_without_macro_calls.append(&mut Self::expand_macro_calls(
                        expanded_macro_tokens,
                        macro_call_id,
                        expansion_depth + 1,
                        macro_calls,
                        builtin_macro_defs,
                        user_macro_defs
                    )?);
                }

            } else {
                has_global_prefix = token.is_symbol(Symbol::GLOBAL);
                tokens_without_macro_calls.push(token);
            }
        }
        Ok(tokens_without_macro_calls)
    }

    fn parse_label_call(
        inner: InnerToken,
        arg_tokens: Vec<Vec<IncludeToken>>,
        macro_call_id: &mut usize,
        expansion_depth: usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &mut Vec<MacroDefinition>,

    ) -> Result<IncludeToken, SourceError> {

        let mut arguments = Vec::with_capacity(arg_tokens.len());
        for tokens in arg_tokens {

            let mut expanded = Vec::with_capacity(tokens.len() * 2);
            for t in tokens {
                // Expand token groups when calling builtin macros
                // This allows us to have a "BYTESIZE" / "CYCLES" builtin
                // macros
                if let IncludeToken::TokenGroup(_, mut inner) = t {
                    expanded.append(&mut inner);

                } else {
                    expanded.push(t);
                }
            }

            // Recursively expand and parse all arguments
            // so that MAX(MIN(FLOOR())) results in 3 BuiltinCalls
            arguments.push(Self::expand_macro_calls(
                expanded,
                macro_call_id,
                expansion_depth,
                macro_calls,
                builtin_macro_defs,
                user_macro_defs
            )?);
        }

        Ok(IncludeToken::ParentLabelCall(inner, arguments))
    }

    fn parse_builtin_call(
        inner: InnerToken,
        arg_tokens: Vec<Vec<IncludeToken>>,
        macro_call_id: &mut usize,
        expansion_depth: usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &mut Vec<MacroDefinition>,

    ) -> Result<IncludeToken, SourceError> {

        let mut arguments = Vec::with_capacity(arg_tokens.len());
        for tokens in arg_tokens {

            let mut expanded = Vec::with_capacity(tokens.len() * 2);
            for t in tokens {
                // Expand token groups when calling builtin macros
                // This allows us to have a "BYTESIZE" / "CYCLES" builtin
                // macros
                if let IncludeToken::TokenGroup(_, mut inner) = t {
                    expanded.append(&mut inner);

                } else {
                    expanded.push(t);
                }
            }

            // Recursively expand and parse all arguments
            // so that MAX(MIN(FLOOR())) results in 3 BuiltinCalls
            arguments.push(Self::expand_macro_calls(
                expanded,
                macro_call_id,
                expansion_depth,
                macro_calls,
                builtin_macro_defs,
                user_macro_defs
            )?);
        }

        Ok(IncludeToken::BuiltinCall(inner, arguments))
    }

    fn expand_macro_token(
        context: &MacroContext,
        mut token: IncludeToken,
        macro_call_id: &mut usize,
        expanding_tokens: &mut Vec<IncludeToken>

    ) -> Result<(), SourceError> {
        // TODO add EXPOSED macros which do not set the macro id
        // this way they will leak into their surrounding if desired
        token.inner_mut().set_macro_call_id(*macro_call_id);
        if token.is(IncludeType::Parameter) {
            if let Some((index, _)) = Self::get_macro_def_param_by_name(context.def, token.symbol()) {

                // Insert and expand the argument's tokens
                for token in context.args[index].clone() {
                    if let IncludeToken::TokenGroup(_, group) = token {
                        for token in group {
                            Self::expand_macro_token(
                                context,
                                token,
                                macro_call_id,
                                expanding_tokens
                            )?;
                        }

                    } else {
                        Self::expand_macro_token(
                            context,
                            token,
                            macro_call_id,
                            expanding_tokens
                        )?;
                    }
                }

            } else {
                return Err(token.error(format!(
                    "Unknown parameter in expansion of macro \"{}\", parameter \"{}\" is not defined in list of macro parameters.",
                    context.def.name.value,
                    token.symbol()
                )));
            }

        } else {
            expanding_tokens.push(token);
        }
        Ok(())

    }

    fn get_macro_def_param_by_name<'a>(def: &'a MacroDefinition, name: &Symbol) -> Option<(usize, &'a InnerToken)> {
        for (index, (_, arg)) in def.parameters.iter().enumerate() {
            if &arg.value == name {
                return Some((index, arg));
            }
        }
        None
    }

    fn get_macro_by_name<'a>(defs: &'a [MacroDefinition], name: &Symbol, file_index: Option<usize>) -> Option<&'a MacroDefinition> {
        for def in defs {
            if &def.name.value == name && def.file_index == file_index {
                return Some(def);
            }
        }
        None
    }

    fn get_macro_by_name_all<'a>(defs: &'a [MacroDefinition], name: &Symbol) -> Option<&'a MacroDefinition> {
        for def in defs {
            if &def.name.value == name {
                return Some(def);
            }
        }
        None
    }

    fn parse_macro_def_arguments(tokens: &mut TokenIterator<IncludeToken>) -> Result<Vec<IncludeToken>, SourceError> {

        let mut arguments = Vec::with_capacity(2);
        tokens.expect(IncludeType::OpenParen, None, "when parsing arguments list")?;

        while !tokens.peek_is(IncludeType::CloseParen, None) {
            let next = tokens.expect(IncludeType::Parameter, None, "while parsing macro arguments list")?;
            if tokens.peek_is(IncludeType::Comma, None) {
                tokens.expect(IncludeType::Comma, None, "")?;
            }
            arguments.push(next);
        }
        tokens.expect(IncludeType::CloseParen, None, "when parsing arguments list")?;

        Ok(arguments)

    }

    fn parse_macro_call_arguments(tokens: &mut TokenIterator<IncludeToken>) -> Result<Vec<Vec<IncludeToken>>, SourceError> {

        let mut arguments = Vec::with_capacity(2);
        tokens.expect(IncludeType::OpenParen, None, "when parsing argument list")?;

        let mut paren_depth = 1;
        let mut arg_tokens = Vec::with_capacity(2);
        while !tokens.peek_is(IncludeType::CloseParen, None) || paren_depth > 1 {

            let next = tokens.get("Unexpected end of input while parsing macro arguments list.")?;
            if next.is(IncludeType::OpenParen) {
                paren_depth += 1 ;

            } else if next.is(IncludeType::CloseParen) {
                paren_depth -= 1;
            }

            if tokens.peek_is(IncludeType::Comma, None) && paren_depth == 1 {
                arg_tokens.push(next);
                tokens.expect(IncludeType::Comma, None, "")?;
                arguments.push(mem::replace(&mut arg_tokens, Vec::new()));

            } else {
                arg_tokens.push(next);
            }
        }

        if !arg_tokens.is_empty() {
            arguments.push(arg_tokens);
        }

        tokens.expect(IncludeType::CloseParen, None, "when parsing argument list")?;
        Ok(arguments)

    }

    fn parse_if_condition(
        tokens: &mut TokenIterator<IncludeToken>,
        token: IncludeToken

    ) -> Result<Vec<MacroToken>, SourceError> {

        let mut condition_tokens = Vec::with_capacity(2);
        while !tokens.peek_is(IncludeType::Reserved, Some(Symbol::THEN)) {
            let token = tokens.get("Unexpected end of input while parsing IF statement condition.")?;
            if token.is(IncludeType::Reserved) {
                return Err(token.error(format!("Unexpected \"{}\" token inside of IF statement condition.", token.symbol())));

            } else {
                condition_tokens.push(MacroToken::from(token));
            }
        }
        tokens.expect(IncludeType::Reserved, Some(Symbol::THEN), "when parsing IF statement condition")?;

        if condition_tokens.is_empty() {
            return Err(token.error("Empty IF statement condition.".to_string()));
        }

        Ok(condition_tokens)
    }

    fn parse_if_body(
        tokens: &mut TokenIterator<IncludeToken>

    ) -> Result<(Vec<MacroToken>, IfBranch), SourceError> {

        let mut body_tokens = Vec::with_capacity(16);
        let mut if_depth = 0;
        while if_depth > 0 || !(tokens.peek_is(IncludeType::Reserved, Some(Symbol::ELSE)) || tokens.peek_is(IncludeType::Reserved, Some(Symbol::ENDIF))) {

            let token = tokens.get("Unexpected end of input while parsing IF statement body.")?;

            // Check for nested statements
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::IF) {
                if_depth += 1;

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDIF) {
                if_depth -= 1;
            }

            body_tokens.push(token);

        }

        // Parse nested statements
        let body_tokens = Self::parse_statements(body_tokens)?;

        // Check for branches
        if tokens.peek_is(IncludeType::Reserved, Some(Symbol::ELSE)) {
            tokens.expect(IncludeType::Reserved, Some(Symbol::ELSE), "when parsing IF statement body")?;
            if tokens.peek_is(IncludeType::Reserved, Some(Symbol::IF)) {
                Ok((body_tokens, IfBranch::If))

            } else {
                Ok((body_tokens, IfBranch::Else))
            }

        } else {
            tokens.expect(IncludeType::Reserved, Some(Symbol::ENDIF), "when parsing IF statement body")?;
            Ok((body_tokens, IfBranch::None))
        }

    }

    fn parse_for_range(
        tokens: &mut TokenIterator<IncludeToken>,
        token: IncludeToken,
        delimiter: Symbol

    ) -> Result<Vec<MacroToken>, SourceError> {
        let mut range_tokens = Vec::with_capacity(2);
        while !tokens.peek_is(IncludeType::Reserved, Some(delimiter.clone())) {
            let token = tokens.get("Unexpected end of input while parsing FOR statement range argument.")?;
            if token.is(IncludeType::Reserved) {
                return Err(token.error(format!("Unexpected \"{}\" token inside of FOR statement range argument.", token.symbol())));

            } else {
                range_tokens.push(MacroToken::from(token));
            }
        }

        if range_tokens.is_empty() {
            return Err(token.error("Empty FOR statement range argument.".to_string()));
        }

        Ok(range_tokens)
    }

    fn parse_for_body(
        tokens: &mut TokenIterator<IncludeToken>

    ) -> Result<Vec<MacroToken>, SourceError> {
        let mut body_tokens = Vec::with_capacity(16);
        let mut for_depth = 0;
        while for_depth > 0 || !tokens.peek_is(IncludeType::Reserved, Some(Symbol::ENDFOR)) {

            let token = tokens.get("Unexpected end of input while parsing FOR statement body.")?;

            // Check for nested statements
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::FOR) {
                for_depth += 1;

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDFOR) {
                for_depth -= 1;
            }

            body_tokens.push(token);

        }
        tokens.expect(IncludeType::Reserved, Some(Symbol::ENDFOR), "when parsing FOR statement body")?;

        // Parse nested statements
        Self::parse_statements(body_tokens)
    }

    fn parse_block_body(
        tokens: &mut TokenIterator<IncludeToken>

    ) -> Result<Vec<MacroToken>, SourceError> {
        let mut body_tokens = Vec::with_capacity(16);
        let mut block_depth = 0;
        while block_depth > 0 || !tokens.peek_is(IncludeType::Reserved, Some(Symbol::ENDBLOCK)) {

            let token = tokens.get("Unexpected end of input while parsing BLOCK statement body.")?;

            // Check for nested statements
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::BLOCK) {
                block_depth += 1;

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDBLOCK) {
                block_depth -= 1;
            }

            body_tokens.push(token);

        }
        tokens.expect(IncludeType::Reserved, Some(Symbol::ENDBLOCK), "when parsing BLOCK statement body")?;

        // Parse nested statements
        Self::parse_statements(body_tokens)
    }


    fn parse_namespace_body(
        tokens: &mut TokenIterator<IncludeToken>

    ) -> Result<Vec<MacroToken>, SourceError> {
        let mut body_tokens = Vec::with_capacity(16);
        let mut namespace_depth = 0;
        while namespace_depth > 0 || !tokens.peek_is(IncludeType::Reserved, Some(Symbol::ENDNAMESPACE)) {

            let token = tokens.get("Unexpected end of input while parsing NAMESPACE statement body.")?;
            if token.is_symbol(Symbol::GLOBAL) {
                return Err(token.error("Namespace members cannot be declared GLOBAL individually.".to_string()));
            }

            // Check for nested statements
            if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::NAMESPACE) {
                namespace_depth += 1;

            } else if token.is(IncludeType::Reserved) && token.is_symbol(Symbol::ENDNAMESPACE) {
                namespace_depth -= 1;
            }

            body_tokens.push(token);

        }
        tokens.expect(IncludeType::Reserved, Some(Symbol::ENDNAMESPACE), "when parsing NAMESPACE statement body")?;

        // Parse nested statements
        Self::parse_statements(body_tokens)
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::mocks::{include_lex, macro_lex, macro_lex_child, macro_lex_child_error};
    use crate::expression::ExpressionArgumenType;
    use super::{
        MacroStage,
        MacroToken,
        MacroDefinition,
        MacroCall,
        InnerToken,
        IncludeToken,
        IfStatementBranch,
        ForStatement,
        BlockStatement,
        NamespaceStatement
    };

    fn macro_lexer_error<S: Into<String>>(s: S) -> String {
        colored::control::set_override(false);
        Lexer::<MacroStage>::from_lexer(include_lex(s), false).err().unwrap().to_string()
    }

    fn macro_lexer_error_child<S: Into<String>>(s: S, c: S) -> String {
        colored::control::set_override(false);
        macro_lex_child_error(s, c)
    }

    fn tfm<S: Into<String>>(s: S) -> Vec<MacroToken> {
        macro_lex(s).tokens
    }

    macro_rules! mdef {
        ($name:expr, $args:expr, $body:expr, $file:expr) => {
            MacroDefinition {
                name: $name,
                parameters: $args,
                body: $body,
                file_index: Some($file),
                is_builtin: false,
                is_exported: false,
                is_label: false
            }
        }
    }

    macro_rules! mgdef {
        ($name:expr, $args:expr, $body:expr) => {
            MacroDefinition {
                name: $name,
                parameters: $args,
                body: $body,
                file_index: None,
                is_builtin: false,
                is_exported: true,
                is_label: false
            }
        }
    }

    macro_rules! mcall {
        ($id:expr, $name:expr, $args:expr) => {
            MacroCall {
                id: $id,
                name: $name,
                arguments: $args
            }
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

    macro_rules! itke {
        ($start:expr, $end:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = InnerToken::new(0, $start, $end, $parsed.into());
                t.set_macro_call_id($id);
                t
            }
        }
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            IncludeToken::$tok(itk!($start, $end, $parsed))
        }
    }

    macro_rules! tkf {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr, $file:expr) => {
            IncludeToken::$tok(itkf!($start, $end, $parsed, $file))
        }
    }

    macro_rules! mtk {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            MacroToken::$tok(itk!($start, $end, $parsed))
        }
    }

    macro_rules! mtke {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $parsed);
                t.set_macro_call_id($id);
                MacroToken::$tok(t)
            }
        }
    }

    macro_rules! mtkef {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr, $id:expr, $file:expr) => {
            {
                let mut t = itkf!($start, $end, $parsed, $file);
                t.set_macro_call_id($id);
                MacroToken::$tok(t)
            }
        }
    }

    #[test]
    fn test_empty() {
        assert_eq!(tfm(""), vec![]);
    }

    #[test]
    fn test_passthrough() {
        assert_eq!(tfm("4\n@-2\nhl,.:"), vec![
            mtk!(NumberLiteral, 0, 1, "4"),
            mtk!(Offset, 2, 5, "-2"),
            mtk!(Register, 6, 8, "hl"),
            mtk!(Comma, 8, 9, ","),
            mtk!(Point, 9, 10, "."),
            mtk!(Colon, 10, 11, ":"),
        ]);
    }

    #[test]
    fn test_strip_newlines() {
        assert_eq!(tfm("\n\r"), vec![]);
    }

    // Macro Definition -------------------------------------------------------

    #[test]
    fn test_macro_def_no_args_no_body() {
        let mut lexer = macro_lex("MACRO FOO() ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mdef!(itk!(6, 9, "FOO"), vec![], vec![], 0)
        ]);
    }

    #[test]
    fn test_macro_def_one_arg() {
        let mut lexer = macro_lex("MACRO FOO(@a) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mdef!(itk!(6, 9, "FOO"), vec![
                (ExpressionArgumenType::Any, itk!(10, 12, "a"))

            ], vec![], 0)
        ]);
    }

    #[test]
    fn test_macro_def_multiple_args() {
        let mut lexer = macro_lex("MACRO FOO(@a, @b, @c) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mdef!(itk!(6, 9, "FOO"), vec![
                (ExpressionArgumenType::Any, itk!(10, 12, "a")),
                (ExpressionArgumenType::Any, itk!(14, 16, "b")),
                (ExpressionArgumenType::Any, itk!(18, 20, "c"))

            ], vec![], 0)
        ]);
    }

    #[test]
    fn test_macro_def_body() {
        let mut lexer = macro_lex("MACRO FOO() hl,a ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mdef!(itk!(6, 9, "FOO"), vec![], vec![
                tk!(Register, 12, 14, "hl"),
                tk!(Comma, 14, 15, ","),
                tk!(Register, 15, 16, "a")
            ], 0)
        ]);
    }

    #[test]
    fn test_macro_def_multiple_files() {
        let mut lexer = macro_lex_child(
            "INCLUDE 'child.gb.s'\nMACRO FOO() hl,a ENDMACRO" ,
            "MACRO FOO() hl,b ENDMACRO"
        );
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mdef!(itkf!(6, 9, "FOO", 1), vec![], vec![
                tkf!(Register, 12, 14, "hl", 1),
                tkf!(Comma, 14, 15, ",", 1),
                tkf!(Register, 15, 16, "b", 1)
            ], 1),
            mdef!(itkf!(27, 30, "FOO", 0), vec![], vec![
                tkf!(Register, 33, 35, "hl", 0),
                tkf!(Comma, 35, 36, ",", 0),
                tkf!(Register, 36, 37, "a", 0)
            ], 0)
        ]);
    }

    #[test]
    fn test_macro_def_multiple_files_global_local() {
        let mut lexer = macro_lex_child(
            "INCLUDE 'child.gb.s'\nMACRO FOO() hl,a ENDMACRO" ,
            "GLOBAL MACRO FOO() hl,b ENDMACRO"
        );
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.data(), vec![
            mgdef!(itkf!(13, 16, "FOO", 1), vec![], vec![
                tkf!(Register, 19, 21, "hl", 1),
                tkf!(Comma, 21, 22, ",", 1),
                tkf!(Register, 22, 23, "b", 1)
            ]),
            mdef!(itkf!(27, 30, "FOO", 0), vec![], vec![
                tkf!(Register, 33, 35, "hl", 0),
                tkf!(Comma, 35, 36, ",", 0),
                tkf!(Register, 36, 37, "a", 0)
            ], 0)
        ]);
    }

    #[test]
    fn test_macro_def_outside() {
        assert_eq!(
            macro_lexer_error("ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ENDMACRO\" token outside of macro definition.\n\nENDMACRO\n^--- Here"
        );
    }

    #[test]
    fn test_macro_def_re_builtin() {
        assert_eq!(
            macro_lexer_error("MACRO DBG() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 7: Re-definition of builtin macro \"DBG\".\n\nMACRO DBG() ENDMACRO\n      ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_re_user_local() {
        assert_eq!(
            macro_lexer_error("MACRO FOO() ENDMACRO MACRO FOO() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 28: Re-definition of non-global user macro \"FOO\".\n\nMACRO FOO() ENDMACRO MACRO FOO() ENDMACRO\n                           ^--- Here\n\nOriginal definition was in file \"main.gb.s\" on line 1, column 7:\n\nMACRO FOO() ENDMACRO MACRO FOO() ENDMACRO\n      ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_re_user_mixed() {
        assert_eq!(
            macro_lexer_error("MACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 35: Re-definition of non-global user macro \"FOO\".\n\nMACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO\n                                  ^--- Here\n\nOriginal definition was in file \"main.gb.s\" on line 1, column 7:\n\nMACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO\n      ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_re_user_global_same_file() {
        assert_eq!(
            macro_lexer_error("GLOBAL MACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 42: Re-definition of global user macro \"FOO\".\n\nGLOBAL MACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO\n                                         ^--- Here\n\nOriginal definition was in file \"main.gb.s\" on line 1, column 14:\n\nGLOBAL MACRO FOO() ENDMACRO GLOBAL MACRO FOO() ENDMACRO\n             ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_re_user_global_other_file_global() {
        assert_eq!(
            macro_lexer_error_child(
                "INCLUDE 'child.gb.s'\nGLOBAL MACRO FOO() ENDMACRO",
                "GLOBAL MACRO FOO() ENDMACRO"
            ),
            "In file \"main.gb.s\" on line 2, column 14: Re-definition of global user macro \"FOO\".\n\nGLOBAL MACRO FOO() ENDMACRO\n             ^--- Here\n\nOriginal definition was in file \"child.gb.s\" on line 1, column 14:\n\nGLOBAL MACRO FOO() ENDMACRO\n             ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_nested() {
        assert_eq!(
            macro_lexer_error("MACRO FOO() MACRO DBG() ENDMACRO ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 13: Invalid nested macro definition.\n\nMACRO FOO() MACRO DBG() ENDMACRO ENDMACRO\n            ^--- Here"
        );
    }

    #[test]
    fn test_macro_def_repeated_names() {
        assert_eq!(
            macro_lexer_error("MACRO FOO(@a, @b, @a) ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 19: Duplicate macro parameter \"a\", a parameter with the same name was already defined.\n\nMACRO FOO(@a, @b, @a) ENDMACRO\n                  ^--- Here"
        );
    }

    #[test]
    fn test_macro_extract() {
        let mut lexer = macro_lex("2 MACRO FOO() ENDMACRO 4");
        assert_eq!(lexer.tokens, vec![
            mtk!(NumberLiteral, 0, 1, "2"),
            mtk!(NumberLiteral, 23, 24, "4"),
        ]);
        assert_eq!(lexer.data(), vec![
            mdef!(itk!(8, 11, "FOO"), vec![], vec![], 0)
        ]);
    }

    #[test]
    fn test_macro_extract_global() {
        let mut lexer = macro_lex("2 GLOBAL MACRO FOO() ENDMACRO 4");
        assert_eq!(lexer.tokens, vec![
            mtk!(NumberLiteral, 0, 1, "2"),
            mtk!(NumberLiteral, 30, 31, "4"),
        ]);
        assert_eq!(lexer.data(), vec![
            mgdef!(itk!(15, 18, "FOO"), vec![], vec![])
        ]);
    }

    // Optional Label Arguments -----------------------------------------------
    #[test]
    fn test_macro_passthrough_parent_label_with_arguments() {
        let lexer = macro_lex("parent_label(bc, de, hl):\nparent_label(1, 2, 3)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::Name(
                itk!(0, 12, "parent_label")
           ),
           MacroToken::OpenParen(
                itk!(12, 13, "(")
           ),
           MacroToken::Register(
                itk!(13, 15, "bc")
           ),
           MacroToken::Comma(
                itk!(15, 16, ",")
           ),
           MacroToken::Register(
                itk!(17, 19, "de")
           ),
           MacroToken::Comma(
                itk!(19, 20, ",")
           ),
           MacroToken::Register(
                itk!(21, 23, "hl")
           ),
           MacroToken::CloseParen(
                itk!(23, 24, ")")
           ),
           MacroToken::Colon(
                itk!(24, 25, ":")
           ),
           MacroToken::ParentLabelCall(
                itk!(26, 38, "parent_label"),
                vec![
                    vec![MacroToken::NumberLiteral(
                       itk!(39, 40, "1")
                    )],
                    vec![MacroToken::NumberLiteral(
                       itk!(42, 43, "2")
                    )],
                    vec![MacroToken::NumberLiteral(
                       itk!(45, 46, "3")
                    )]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
           mcall!(0, itk!(26, 38, "parent_label"), vec![
                vec![IncludeToken::NumberLiteral(
                   itk!(39, 40, "1")
                )],
                vec![IncludeToken::NumberLiteral(
                   itk!(42, 43, "2")
                )],
                vec![IncludeToken::NumberLiteral(
                   itk!(45, 46, "3")
                )]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    // Builtin Macro Calls ----------------------------------------------------
    #[test]
    fn test_macro_call_no_args() {
        let lexer = macro_lex("DBG()");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "DBG"),
                vec![]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "DBG"), vec![])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_one_arg() {
        let lexer = macro_lex("ABS(4)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "ABS"),
                vec![
                    vec![mtk!(NumberLiteral, 4, 5, "4")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "ABS"), vec![
                vec![tk!(NumberLiteral, 4, 5, "4")]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_unwrap_token_group_args() {
        let lexer = macro_lex("ABS(`i k`)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "ABS"),
                vec![
                    vec![mtk!(Name, 5, 6, "i"), mtk!(Name, 7, 8, "k")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "ABS"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`"),
                    vec![
                        tk!(Name, 5, 6, "i"),
                        tk!(Name, 7, 8, "k")
                    ]
                )]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_multiple_args() {
        let lexer = macro_lex("MAX(4, 2)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "MAX"),
                vec![
                    vec![mtk!(NumberLiteral, 4, 5, "4")],
                    vec![mtk!(NumberLiteral, 7, 8, "2")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "MAX"), vec![
                vec![tk!(NumberLiteral, 4, 5, "4")],
                vec![tk!(NumberLiteral, 7, 8, "2")]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_expression_args() {
        let lexer = macro_lex("MAX((4, 2) + (2 - 1), 2)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "MAX"),
                vec![
                    vec![
                        mtk!(OpenParen, 4, 5, "("),
                        mtk!(NumberLiteral, 5, 6, "4"),
                        mtk!(Comma, 6, 7, ","),
                        mtk!(NumberLiteral, 8, 9, "2"),
                        mtk!(CloseParen, 9, 10, ")"),
                        mtk!(Operator, 11, 12, "+"),
                        mtk!(OpenParen, 13, 14, "("),
                        mtk!(NumberLiteral, 14, 15, "2"),
                        mtk!(Operator, 16, 17, "-"),
                        mtk!(NumberLiteral, 18, 19, "1"),
                        mtk!(CloseParen, 19, 20, ")"),
                    ],
                    vec![mtk!(NumberLiteral, 22, 23, "2")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "MAX"), vec![
                vec![
                    tk!(OpenParen, 4, 5, "("),
                    tk!(NumberLiteral, 5, 6, "4"),
                    tk!(Comma, 6, 7, ","),
                    tk!(NumberLiteral, 8, 9, "2"),
                    tk!(CloseParen, 9, 10, ")"),
                    tk!(Operator, 11, 12, "+"),
                    tk!(OpenParen, 13, 14, "("),
                    tk!(NumberLiteral, 14, 15, "2"),
                    tk!(Operator, 16, 17, "-"),
                    tk!(NumberLiteral, 18, 19, "1"),
                    tk!(CloseParen, 19, 20, ")"),
                ],
                vec![tk!(NumberLiteral, 22, 23, "2")]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_expression_args_recursive() {
        let lexer = macro_lex("MAX(MIN(4, FLOOR(2) + CEIL(1)), 2)");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 3, "MAX"), vec![
                vec![MacroToken::BuiltinCall(itk!(4, 7, "MIN"), vec![
                    vec![mtk!(NumberLiteral, 8, 9, "4")],
                    vec![
                        MacroToken::BuiltinCall(itk!(11, 16, "FLOOR"), vec![
                            vec![mtk!(NumberLiteral, 17, 18, "2")]
                        ]),
                        mtk!(Operator, 20, 21, "+"),
                        MacroToken::BuiltinCall(itk!(22, 26, "CEIL"), vec![
                            vec![mtk!(NumberLiteral, 27, 28, "1")]
                        ])
                    ]]
                )],
                vec![mtk!(NumberLiteral, 32, 33, "2")]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 4);
    }

    #[test]
    fn test_macro_call_expression_args_recursive_with_user() {
        let lexer = macro_lex("CEIL(BAR()) MACRO BAR() 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 24, 25, "4", 0)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_call_expression_args_recursive_with_user_unused_param() {
        let lexer = macro_lex("CEIL(BAR(BAR(2))) MACRO BAR(@a) 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 32, 33, "4", 0)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_call_expression_args_recursive_with_user_used_param() {
        let lexer = macro_lex("CEIL(BAR(BAR(4))) MACRO BAR(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 13, 14, "4", 1)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 3);
    }

    #[test]
    fn test_error_macro_call_too_few_parameters() {
        assert_eq!(
            macro_lexer_error("MAX(1)"),
            "In file \"main.gb.s\" on line 1, column 1: Incorrect number of parameters for invocation of macro \"MAX\", expected 2 parameter(s) but got 1.\n\nMAX(1)\n^--- Here"
        );
    }

    #[test]
    fn test_error_macro_call_too_many_parameters() {
        assert_eq!(
            macro_lexer_error("MAX(1, 2, 3)"),
            "In file \"main.gb.s\" on line 1, column 1: Incorrect number of parameters for invocation of macro \"MAX\", expected 2 parameter(s) but got 3.\n\nMAX(1, 2, 3)\n^--- Here"
        );
    }

    // User Macro Calls -------------------------------------------------------

    #[test]
    fn test_macro_user_call_no_args() {
        let lexer = macro_lex("FOO() MACRO FOO() op 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 18, 20, "op", 0),
            mtke!(NumberLiteral, 21, 22, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_one_arg() {
        let lexer = macro_lex("FOO(4) MACRO FOO(@a) op @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 21, 23, "op", 0),
            mtke!(NumberLiteral, 4, 5, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![
                    tk!(NumberLiteral, 4, 5, "4")
                ]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_expression() {
        let lexer = macro_lex("FOO((4 + 2)) MACRO FOO(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(OpenParen, 4, 5, "(", 0),
            mtke!(NumberLiteral, 5, 6, "4", 0),
            mtke!(Operator, 7, 8, "+", 0),
            mtke!(NumberLiteral, 9, 10, "2", 0),
            mtke!(CloseParen, 10, 11, ")", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![
                    tk!(OpenParen, 4, 5, "("),
                    tk!(NumberLiteral, 5, 6, "4"),
                    tk!(Operator, 7, 8, "+"),
                    tk!(NumberLiteral, 9, 10, "2"),
                    tk!(CloseParen, 10, 11, ")"),
                ]
            ])
        ]);
    }

    #[test]
    fn test_macro_user_call_token_group_arg() {
        let lexer = macro_lex("FOO(`a b`) MACRO FOO(@a) op @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 25, 27, "op", 0),
            mtke!(Register, 5, 6, "a", 0),
            mtke!(Register, 7, 8, "b", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`"),
                    vec![
                        tk!(Register, 5, 6, "a"),
                        tk!(Register, 7, 8, "b")
                    ]
                )]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_multiple_args() {
        let lexer = macro_lex("FOO(4, 8) MACRO FOO(@a, @b) op @b @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 28, 30, "op", 0),
            mtke!(NumberLiteral, 7, 8, "8", 0),
            mtke!(NumberLiteral, 4, 5, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![
                    tk!(NumberLiteral, 4, 5, "4")
                ],
                vec![
                    tk!(NumberLiteral, 7, 8, "8")
                ]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_recursive() {
        let lexer = macro_lex("FOO() MACRO FOO() 4 BAR() ENDMACRO MACRO BAR() 8 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 18, 19, "4", 0),
            mtke!(NumberLiteral, 47, 48, "8", 1),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![]),
            mcall!(1, itke!(20, 23, "BAR", 0), vec![])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_user_recursive_token_group() {
        let lexer = macro_lex("FOO(`BAR()`) MACRO FOO(@a) 4 @a ENDMACRO MACRO BAR() 8 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 27, 28, "4", 0),
            mtke!(NumberLiteral, 53, 54, "8", 1),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`"),
                    vec![
                        tk!(Name, 5, 8, "BAR"),
                        tk!(OpenParen, 8, 9, "("),
                        tk!(CloseParen, 9, 10, ")")
                    ]
                )]
            ]),
            mcall!(1, itke!(5, 8, "BAR", 0), vec![])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_user_recursive_token_group_parameters() {
        let lexer = macro_lex("FOO(`@b`, `@c`, 4) MACRO FOO(@a, @b, @c) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 16, 17, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_global() {
        let lexer = macro_lex_child(
            "INCLUDE 'child.gb.s'\nFOO()",
            "GLOBAL MACRO FOO() 8 ENDMACRO"
        );
        assert_eq!(lexer.tokens, vec![
            mtkef!(NumberLiteral, 19, 20, "8", 0, 1),
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_local_over_global() {
        let lexer = macro_lex_child(
            "INCLUDE 'child.gb.s'\nMACRO FOO() 4 ENDMACRO\nFOO()",
            "GLOBAL MACRO FOO() 8 ENDMACRO"
        );
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 33, 34, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_error_user_call_defined_non_global_other_file() {
        assert_eq!(
            macro_lexer_error_child(
                "INCLUDE 'child.gb.s'\nFOO()",
                "MACRO FOO() ENDMACRO"
            ),
            "In file \"main.gb.s\" on line 2, column 1: Invocation of undefined macro \"FOO\"\n\nFOO()\n^--- Here\n\nA non-global macro with the same name is defined in file \"child.gb.s\" on line 1, column 7:\n\nMACRO FOO() ENDMACRO\n      ^--- Here"
        );
    }

    #[test]
    fn test_macro_user_recursion_limit() {
        assert_eq!(
            macro_lexer_error("FOO() MACRO FOO() FOO() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 19: Maximum recursion limit of 8 reached during expansion of macro \"FOO\".\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 19: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 1: Triggered by previous macro invocation\n\nFOO() MACRO FOO() FOO() ENDMACRO\n^--- Here"
        );
    }

    #[test]
    fn test_macro_user_expansion_stack() {
        assert_eq!(
            macro_lexer_error("FOO() MACRO FOO() @b ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 19: Unknown parameter in expansion of macro \"FOO\", parameter \"b\" is not defined in list of macro parameters.\n\nFOO() MACRO FOO() @b ENDMACRO\n                  ^--- Here\n\nIn file \"main.gb.s\" on line 1, column 1: Triggered by previous macro invocation\n\nFOO() MACRO FOO() @b ENDMACRO\n^--- Here"
        );
    }

    #[test]
    fn test_macro_user_expansion_parallel() {
        assert_eq!(
            macro_lex("FOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\n MACRO FOO() ENDMACRO").macro_calls_count(),
            9
        );
    }

    #[test]
    fn test_error_macro_user_too_few_parameters() {
        assert_eq!(
            macro_lexer_error("FOO() MACRO FOO(@b) b ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 1: Incorrect number of parameters for invocation of macro \"FOO\", expected 1 parameter(s) but got 0.\n\nFOO() MACRO FOO(@b) b ENDMACRO\n^--- Here"
        );
    }

    #[test]
    fn test_error_macro_user_too_many_parameters() {
        assert_eq!(
            macro_lexer_error("FOO(1, 2) MACRO FOO(@b) b ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 1: Incorrect number of parameters for invocation of macro \"FOO\", expected 1 parameter(s) but got 2.\n\nFOO(1, 2) MACRO FOO(@b) b ENDMACRO\n^--- Here"
        );
    }

    #[test]
    fn test_macro_user_recursive_params() {
        let lexer = macro_lex("FOO(FOO(FOO(4))) MACRO FOO(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 12, 13, "4", 2),
        ]);
        assert_eq!(lexer.macro_calls_count(), 3);
    }

    // IF Statements ----------------------------------------------------------
    #[test]
    fn test_if_statement_endif() {
        let lexer = macro_lex("IF foo THEN bar ENDIF");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![MacroToken::Name(itk!(12, 15, "bar"))]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_endif_nested() {
        let lexer = macro_lex("IF foo THEN IF bar THEN baz ENDIF ENDIF");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![
                        MacroToken::IfStatement(itk!(12, 14, "IF"), vec![
                            IfStatementBranch {
                                condition: Some(vec![MacroToken::Name(itk!(15, 18, "bar"))]),
                                body: vec![
                                    MacroToken::Name(itk!(24, 27, "baz"))
                                ]
                            }
                        ])
                    ]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_else() {
        let lexer = macro_lex("IF foo THEN bar ELSE baz ENDIF");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![MacroToken::Name(itk!(12, 15, "bar"))]
                },
                IfStatementBranch {
                    condition: None,
                    body: vec![MacroToken::Name(itk!(21, 24, "baz"))]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_else_if_endif() {
        let lexer = macro_lex("IF foo THEN bar ELSE IF fuz THEN baz ENDIF");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![MacroToken::Name(itk!(12, 15, "bar"))]
                },
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(24, 27, "fuz"))]),
                    body: vec![MacroToken::Name(itk!(33, 36, "baz"))]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_else_if_else_endif() {
        let lexer = macro_lex("IF foo THEN bar ELSE IF fuz THEN baz ELSE fub ENDIF");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itk!(0, 2, "IF"), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(3, 6, "foo"))]),
                    body: vec![MacroToken::Name(itk!(12, 15, "bar"))]
                },
                IfStatementBranch {
                    condition: Some(vec![MacroToken::Name(itk!(24, 27, "fuz"))]),
                    body: vec![MacroToken::Name(itk!(33, 36, "baz"))]
                },
                IfStatementBranch {
                    condition: None,
                    body: vec![MacroToken::Name(itk!(42, 45, "fub"))]
                }
            ])
        ]);
    }

    #[test]
    fn test_if_statement_macro() {
        let lexer = macro_lex("MACRO BAR(@a) IF @a THEN bar ENDIF ENDMACRO\nBAR(1)");
        assert_eq!(lexer.tokens, vec![
            MacroToken::IfStatement(itke!(14, 16, "IF", 0), vec![
                IfStatementBranch {
                    condition: Some(vec![MacroToken::NumberLiteral(itke!(48, 49, "1", 0))]),
                    body: vec![MacroToken::Name(itke!(25, 28, "bar", 0))]
                }
            ])
        ]);
    }

    #[test]
    fn test_error_if_keywords() {
        assert_eq!(macro_lexer_error("THEN"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"THEN\" token outside of IF statement.\n\nTHEN\n^--- Here");
        assert_eq!(macro_lexer_error("ELSE"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ELSE\" token outside of IF statement.\n\nELSE\n^--- Here");
        assert_eq!(macro_lexer_error("ENDIF"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ENDIF\" token outside of IF statement.\n\nENDIF\n^--- Here");
    }

    #[test]
    fn test_error_if_condition() {
        assert_eq!(macro_lexer_error("IF"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing IF statement condition.\n\nIF\n^--- Here");
        assert_eq!(macro_lexer_error("IF IF"), "In file \"main.gb.s\" on line 1, column 4: Unexpected \"IF\" token inside of IF statement condition.\n\nIF IF\n   ^--- Here");
        assert_eq!(macro_lexer_error("IF ELSE"), "In file \"main.gb.s\" on line 1, column 4: Unexpected \"ELSE\" token inside of IF statement condition.\n\nIF ELSE\n   ^--- Here");
        assert_eq!(macro_lexer_error("IF ENDIF"), "In file \"main.gb.s\" on line 1, column 4: Unexpected \"ENDIF\" token inside of IF statement condition.\n\nIF ENDIF\n   ^--- Here");
        assert_eq!(macro_lexer_error("IF THEN"), "In file \"main.gb.s\" on line 1, column 1: Empty IF statement condition.\n\nIF THEN\n^--- Here");
    }

    #[test]
    fn test_error_if_body() {
        assert_eq!(macro_lexer_error("IF foo THEN"), "In file \"main.gb.s\" on line 1, column 8: Unexpected end of input while parsing IF statement body.\n\nIF foo THEN\n       ^--- Here");
        assert_eq!(macro_lexer_error("IF foo THEN ELSE"), "In file \"main.gb.s\" on line 1, column 13: Unexpected end of input while parsing IF statement body.\n\nIF foo THEN ELSE\n            ^--- Here");
        assert_eq!(macro_lexer_error("IF foo THEN ELSE IF bar THEN "), "In file \"main.gb.s\" on line 1, column 25: Unexpected end of input while parsing IF statement body.\n\nIF foo THEN ELSE IF bar THEN \n                        ^--- Here");
    }

    #[test]
    fn test_error_if_nested() {
        assert_eq!(macro_lexer_error("IF foo THEN IF bar THEN ENDIF"), "In file \"main.gb.s\" on line 1, column 25: Unexpected end of input while parsing IF statement body.\n\nIF foo THEN IF bar THEN ENDIF\n                        ^--- Here");
    }

    // FOR Statements ---------------------------------------------------------
    #[test]
    fn test_for_statement() {
        let lexer = macro_lex("FOR x IN 0 TO 10 REPEAT bar ENDFOR");
        assert_eq!(lexer.tokens, vec![
            MacroToken::ForStatement(itk!(0, 3, "FOR"), ForStatement {
                binding: Box::new(MacroToken::Name(itk!(4, 5, "x"))),
                from: vec![MacroToken::NumberLiteral(itk!(9, 10, "0"))],
                to: vec![MacroToken::NumberLiteral(itk!(14, 16, "10"))],
                body: vec![MacroToken::Name(itk!(24, 27, "bar"))]
            })
        ]);
    }

    #[test]
    fn test_error_for_statement() {
        assert_eq!(macro_lexer_error("FOR"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing FOR statement, expected a \"Name\" token instead.\n\nFOR\n^--- Here");
        assert_eq!(macro_lexer_error("FOR foo"), "In file \"main.gb.s\" on line 1, column 5: Unexpected end of input when parsing FOR statement, expected \"IN\" instead.\n\nFOR foo\n    ^--- Here");
        assert_eq!(macro_lexer_error("FOR foo IN"), "In file \"main.gb.s\" on line 1, column 9: Unexpected end of input while parsing FOR statement range argument.\n\nFOR foo IN\n        ^--- Here");
        assert_eq!(macro_lexer_error("FOR foo IN 0"), "In file \"main.gb.s\" on line 1, column 12: Unexpected end of input while parsing FOR statement range argument.\n\nFOR foo IN 0\n           ^--- Here");
        assert_eq!(macro_lexer_error("FOR foo IN 0 TO"), "In file \"main.gb.s\" on line 1, column 14: Unexpected end of input while parsing FOR statement range argument.\n\nFOR foo IN 0 TO\n             ^--- Here");
        assert_eq!(macro_lexer_error("FOR foo IN 0 TO 10"), "In file \"main.gb.s\" on line 1, column 17: Unexpected end of input while parsing FOR statement range argument.\n\nFOR foo IN 0 TO 10\n                ^--- Here");
        assert_eq!(macro_lexer_error("FOR foo IN 0 TO 10 REPEAT"), "In file \"main.gb.s\" on line 1, column 20: Unexpected end of input while parsing FOR statement body.\n\nFOR foo IN 0 TO 10 REPEAT\n                   ^--- Here");
    }

    #[test]
    fn test_error_for_keywords() {
        assert_eq!(macro_lexer_error("IN"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"IN\" token outside of FOR statement.\n\nIN\n^--- Here");
        assert_eq!(macro_lexer_error("TO"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"TO\" token outside of FOR statement.\n\nTO\n^--- Here");
        assert_eq!(macro_lexer_error("REPEAT"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"REPEAT\" token outside of FOR statement.\n\nREPEAT\n^--- Here");
        assert_eq!(macro_lexer_error("ENDFOR"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ENDFOR\" token outside of FOR statement.\n\nENDFOR\n^--- Here");
    }

    // Blocks -----------------------------------------------------------------
    #[test]
    fn test_block_using() {
        let lexer = macro_lex("BLOCK USING 'cmd' DB 1 ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Using(
                "cmd".to_string(),
                vec![
                    MacroToken::Reserved(itk!(18, 20, "DB")),
                    MacroToken::NumberLiteral(itk!(21, 22, "1"))
                ])
            )
        ]);
    }

    #[test]
    fn test_block_volatile() {
        let lexer = macro_lex("BLOCK VOLATILE nop ENDBLOCK");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BlockStatement(itk!(0, 5, "BLOCK"), BlockStatement::Volatile(
                vec![
                    MacroToken::Instruction(itk!(15, 18, "nop"))
                ])
            )
        ]);
    }

    #[test]
    fn test_error_block() {
        assert_eq!(macro_lexer_error("BLOCK"), "In file \"main.gb.s\" on line 1, column 1: Expected either a USING or VOLATILE keyword to BLOCK directive.\n\nBLOCK\n^--- Here");
        assert_eq!(macro_lexer_error("ENDBLOCK"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ENDBLOCK\" token outside of BLOCK statement.\n\nENDBLOCK\n^--- Here");
    }

    // Namespace Statements ---------------------------------------------------
    #[test]
    fn test_namespace_statement() {
        let lexer = macro_lex("NAMESPACE foo ENDNAMESPACE");
        assert_eq!(lexer.tokens, vec![
            MacroToken::NamespaceStatement(itk!(0, 9, "NAMESPACE"), NamespaceStatement {
                name: Box::new(MacroToken::Name(itk!(10, 13, "foo"))),
                body: vec![]
            })
        ]);
        let lexer = macro_lex("NAMESPACE foo\nfoo: DB\nbar: DB\nENDNAMESPACE");
        assert_eq!(lexer.tokens, vec![
            MacroToken::NamespaceStatement(itk!(0, 9, "NAMESPACE"), NamespaceStatement {
                name: Box::new(MacroToken::Name(itk!(10, 13, "foo"))),
                body: vec![
                    MacroToken::Name(itk!(14, 17, "foo")),
                    MacroToken::Colon(itk!(17, 18, ":")),
                    MacroToken::Reserved(itk!(19, 21, "DB")),
                    MacroToken::Name(itk!(22, 25, "bar")),
                    MacroToken::Colon(itk!(25, 26, ":")),
                    MacroToken::Reserved(itk!(27, 29, "DB"))
                ]
            })
        ]);
    }

    #[test]
    fn test_namespace_statement_nested() {
        let lexer = macro_lex("NAMESPACE foo prefix: DB NAMESPACE bar inner: DB ENDNAMESPACE outer: DB ENDNAMESPACE");
        assert_eq!(lexer.tokens, vec![
            MacroToken::NamespaceStatement(itk!(0, 9, "NAMESPACE"), NamespaceStatement {
                name: Box::new(MacroToken::Name(itk!(10, 13, "foo"))),
                body: vec![
                    MacroToken::Name(itk!(14, 20, "prefix")),
                    MacroToken::Colon(itk!(20, 21, ":")),
                    MacroToken::Reserved(itk!(22, 24, "DB")),
                    MacroToken::NamespaceStatement(itk!(25, 34, "NAMESPACE"), NamespaceStatement {
                        name: Box::new(MacroToken::Name(itk!(35, 38, "bar"))),
                        body: vec![
                            MacroToken::Name(itk!(39, 44, "inner")),
                            MacroToken::Colon(itk!(44, 45, ":")),
                            MacroToken::Reserved(itk!(46, 48, "DB")),
                        ]
                    }),
                    MacroToken::Name(itk!(62, 67, "outer")),
                    MacroToken::Colon(itk!(67, 68, ":")),
                    MacroToken::Reserved(itk!(69, 71, "DB"))
                ]
            })
        ]);
    }

    #[test]
    fn test_error_namespace_statement() {
        assert_eq!(macro_lexer_error("NAMESPACE"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing NAMESPACE statement, expected a \"Name\" token instead.\n\nNAMESPACE\n^--- Here");
        assert_eq!(macro_lexer_error("ENDNAMESPACE"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ENDNAMESPACE\" token outside of NAMESPACE statement.\n\nENDNAMESPACE\n^--- Here");
        assert_eq!(macro_lexer_error("NAMESPACE foo GLOBAL ENDNAMESPACE"), "In file \"main.gb.s\" on line 1, column 15: Namespace members cannot be declared GLOBAL individually.\n\nNAMESPACE foo GLOBAL ENDNAMESPACE\n              ^--- Here");
        assert_eq!(macro_lexer_error("::"), "In file \"main.gb.s\" on line 1, column 1: Incomplete namespace member access.\n\n::\n^--- Here");
        assert_eq!(macro_lexer_error("foo::"), "In file \"main.gb.s\" on line 1, column 4: Unexpected end of input when parsing namespace member access, expected a \"Name\" token instead.\n\nfoo::\n   ^--- Here");
        assert_eq!(macro_lexer_error("foo::DB"), "In file \"main.gb.s\" on line 1, column 6: Unexpected token \"Reserved\" when parsing namespace member access, expected a \"Name\" token instead.\n\nfoo::DB\n     ^--- Here");
    }

}

