// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::error::Error;
use std::collections::HashSet;


// Internal Dependencies ------------------------------------------------------
use super::{IncludeLexer, InnerToken, TokenIterator, LexerFile, LexerToken, LexerError, TokenType};
use super::include::IncludeToken;


// Statics --------------------------------------------------------------------
const MAX_EXPANSION_DEPTH: usize = 8;


// Macro Specific Tokens ------------------------------------------------------
lexer_token!(MacroToken, (Debug, Eq, PartialEq), {
    Name(()),
    Reserved(()),
    Instruction(()),
    Offset(()),
    NumberLiteral(()),
    StringLiteral(()),
    BinaryFile((Vec<u8>)),
    BuiltinCall((Vec<Vec<MacroToken>>)),
    Comma(()),
    Point(()),
    Colon(()),
    Operator(()),
    Comment(()),
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
            IncludeToken::Instruction(inner) => MacroToken::Instruction(inner),
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
            IncludeToken::Comma(inner) => MacroToken::Comma(inner),
            IncludeToken::Point(inner) => MacroToken::Point(inner),
            IncludeToken::Colon(inner) => MacroToken::Colon(inner),
            IncludeToken::Operator(inner) => MacroToken::Operator(inner),
            IncludeToken::Comment(inner) => MacroToken::Comment(inner),
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
enum MacroArgumenType {
    Any,
    Tokens,
    String,
    Number,
    Integer
}

#[derive(Debug, Eq, PartialEq)]
enum MacroReturnType {
    None,
    String,
    Number,
    Float,
    Integer,
}

#[derive(Debug, Eq, PartialEq)]
struct MacroDefinition {
    name: InnerToken,
    parameters: Vec<(MacroArgumenType, InnerToken)>,
    return_type: MacroReturnType,
    body: Vec<IncludeToken>,
    builtin: bool
}

impl MacroDefinition {
    fn builtin(name: &str, parameters: Vec<(MacroArgumenType, &str)>, return_type: MacroReturnType) -> Self {
        Self {
            name: InnerToken::new(0, 0, 0, name.into()),
            parameters: parameters.into_iter().map(|(typ, name)| {
                (typ, InnerToken::new(0, 0, 0, name.into()))

            }).collect(),
            return_type,
            body: Vec::new(),
            builtin: true
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
    pub fn error(&self, message: String) -> LexerError {
        self.name.error(message)
    }
}


// Macro Level Lexer Implementation -------------------------------------------
pub struct MacroLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<MacroToken>,
    macro_defs: Vec<MacroDefinition>,
    pub macro_calls: Vec<MacroCall>
}

impl MacroLexer {

    // TODO use a trait for the boiler plate stuff
    pub fn try_from(lexer: IncludeLexer) -> Result<MacroLexer, Box<dyn Error>> {

        let files = lexer.files;
        let mut macro_calls = Vec::new();
        let (macro_defs, tokens) = Self::from_tokens(lexer.tokens, &mut macro_calls).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;

        Ok(Self {
            files,
            macro_defs,
            macro_calls,
            tokens
        })

    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn macro_defs_count(&self) -> usize {
        self.macro_defs.len()
    }

    pub fn macro_calls_count(&self) -> usize {
        self.macro_calls.len()
    }

    fn from_tokens(
        tokens: Vec<IncludeToken>,
        macro_calls: &mut Vec<MacroCall>

    ) -> Result<(Vec<MacroDefinition>, Vec<MacroToken>), LexerError> {

        let builtin_macro_defs = Self::builtin_macro_defs();
        let mut user_macro_defs = Vec::new();
        let mut tokens_without_macro_defs = Vec::new();

        // Drop all newlines
        let mut tokens = TokenIterator::new(tokens.into_iter().filter(|t| !t.is(TokenType::Newline)).collect());

        // Extract Macro Definitions
        while let Some(token) = tokens.next() {
            if token.is(TokenType::Reserved) && token.has_value("ENDMACRO") {
                return Err(token.error(format!("Unexpected \"{}\" token outside of macro definition.", token.value())));

            } else if token.is(TokenType::Reserved) && token.has_value("MACRO") {

                // Verify Macro Name
                let name_token = tokens.expect(TokenType::Name, None, "when parsing macro definition")?;
                if let Some(_) = Self::get_macro_by_name(&builtin_macro_defs, name_token.value()) {
                    return Err(name_token.error(format!("Re-definition of builtin macro \"{}\".", name_token.value())));

                } else if let Some(user_def) = Self::get_macro_by_name(&user_macro_defs, name_token.value()) {
                    return Err(name_token.error(
                        format!("Re-definition of user macro \"{}\".", name_token.value())

                    ).with_reference(&user_def.name, "Original definition was"));
                }

                // Parse Macro Parameter list
                let param_tokens = Self::parse_macro_def_arguments(&mut tokens)?;
                let parameters: Vec<(MacroArgumenType, InnerToken)> = param_tokens.into_iter().map(|t| (MacroArgumenType::Any, t.into_inner())).collect();

                // Check against duplicate names
                let mut param_names: HashSet<String> = HashSet::new();
                for (_, arg) in &parameters {
                    if param_names.contains(&arg.value) {
                        return Err(arg.error(format!("Duplicate macro parameter \"{}\", a parameter with the same name was already defined.", arg.value)));

                    } else {
                        param_names.insert(arg.value.clone());
                    }
                }

                // Collect Body Tokens
                let mut body_tokens = Vec::new();
                while !tokens.peek_is(TokenType::Reserved, Some("ENDMACRO")) {
                    let token = tokens.get("Unexpected end of input while parsing macro body.")?;
                    if token.is(TokenType::Reserved) && token.has_value("MACRO") {
                        return Err(token.error(format!("Invalid nested macro definition.")));

                    } else {
                        body_tokens.push(token);
                    }
                }
                tokens.expect(TokenType::Reserved, Some("ENDMACRO"), "when parsing macro definition")?;

                // Add Macro Definition
                user_macro_defs.push(MacroDefinition {
                    name: name_token.into_inner(),
                    parameters,
                    body: body_tokens,
                    return_type: MacroReturnType::None,
                    builtin: false
                });

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
            &builtin_macro_defs,
            &user_macro_defs
        )?;

        // Check for any left over parameter or token groups
        for token in &tokens_without_macro_calls {
            if token.is(TokenType::Parameter) {
                return Err(token.error(format!(
                    "Unexpected parameter \"{}\" outside of macro expansion.",
                    token.value()
                )));

            } else if token.is(TokenType::TokenGroup) {
                return Err(token.error(format!(
                    "Unexpected token group outside of macro expansion."
                )));
            }
        }

        Ok((
            user_macro_defs,
            tokens_without_macro_calls.into_iter().map(MacroToken::from).collect()
        ))

    }

    fn expand_macro_calls(
        tokens: Vec<IncludeToken>,
        macro_call_id: &mut usize,
        expansion_depth: usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &[MacroDefinition],

    ) -> Result<Vec<IncludeToken>, LexerError> {

        let mut tokens_without_macro_calls = Vec::new();
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {

            if token.is(TokenType::Name) && tokens.peek_is(TokenType::OpenParen, None) {
                let macro_def = if let Some(def) = Self::get_macro_by_name(&builtin_macro_defs, token.value()) {
                    def

                } else if let Some(def) = Self::get_macro_by_name(&user_macro_defs, token.value()) {
                    def

                } else {
                    return Err(token.error(format!("Invocation of undefined macro \"{}\"", token.value())));
                };

                let arg_tokens = Self::parse_macro_call_arguments(&mut tokens)?;
                if arg_tokens.len() != macro_def.parameters.len() {
                    return Err(token.error(format!(
                        "Incorrect number of parameters for invocation of macro \"{}\", expected {} parameter(s) but got {}.",
                        token.value(),
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

                // Replace builtin calls
                if macro_def.builtin {
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
                    let mut expanded_macro_tokens = Vec::new();

                    // Recursively expand all body tokens
                    for token in macro_def.body.clone() {
                        expanded_macro_tokens.append(&mut Self::expand_macro_token(
                            &macro_def,
                            &arg_tokens,
                            token,
                            macro_call_id,
                            macro_calls,
                            builtin_macro_defs,
                            user_macro_defs
                        )?);
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
                        expanded_macro_tokens.into_iter().filter(|t| !t.is(TokenType::Newline)).collect(),
                        macro_call_id,
                        expansion_depth + 1,
                        macro_calls,
                        builtin_macro_defs,
                        user_macro_defs
                    )?);
                }

            } else {
                tokens_without_macro_calls.push(token);
            }
        }
        Ok(tokens_without_macro_calls)
    }

    fn parse_builtin_call(
        inner: InnerToken,
        arg_tokens: Vec<Vec<IncludeToken>>,
        macro_call_id: &mut usize,
        expansion_depth: usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &[MacroDefinition],

    ) -> Result<IncludeToken, LexerError> {

        let mut arguments = Vec::with_capacity(arg_tokens.len());
        for tokens in arg_tokens {

            let mut expanded = Vec::new();
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
                expanded.into_iter().filter(|t| !t.is(TokenType::Newline)).collect(),
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
        macro_def: &MacroDefinition,
        arg_tokens: &[Vec<IncludeToken>],
        mut token: IncludeToken,
        macro_call_id: &mut usize,
        macro_calls: &mut Vec<MacroCall>,
        builtin_macro_defs: &[MacroDefinition],
        user_macro_defs: &[MacroDefinition],

    ) -> Result<Vec<IncludeToken>, LexerError> {
        let mut expanded = Vec::new();
        token.inner_mut().set_macro_call_id(*macro_call_id);
        if token.is(TokenType::Parameter) {
            if let Some((index, _)) = Self::get_macro_def_param_by_name(&macro_def, token.value()) {

                // Insert and expand the argument's tokens
                let call_argument = arg_tokens[index].clone();
                for mut token in call_argument {

                    if let IncludeToken::TokenGroup(_, group) = token {
                        for mut token in group {
                            token.inner_mut().set_macro_call_id(*macro_call_id);

                            // Expand any inserted parameters or token groups
                            expanded.append(&mut Self::expand_macro_token(
                                &macro_def,
                                &arg_tokens,
                                token,
                                macro_call_id,
                                macro_calls,
                                builtin_macro_defs,
                                user_macro_defs
                            )?);
                        }

                    } else {
                        token.inner_mut().set_macro_call_id(*macro_call_id);

                        // Expand any inserted parameters or token groups
                        expanded.append(&mut Self::expand_macro_token(
                            &macro_def,
                            &arg_tokens,
                            token,
                            macro_call_id,
                            macro_calls,
                            builtin_macro_defs,
                            user_macro_defs
                        )?);
                    }
                }

            } else {
                return Err(token.error(format!(
                    "Unknown parameter in expansion of macro \"{}\", parameter \"{}\" is not defined in list of macro parameters.",
                    macro_def.name.value,
                    token.value()
                )));
            }

        } else {
            expanded.push(token);
        }

        Ok(expanded)
    }

    fn get_macro_def_param_by_name<'a>(def: &'a MacroDefinition, name: &str) -> Option<(usize, &'a InnerToken)> {
        for (index, (_, arg)) in def.parameters.iter().enumerate() {
            if arg.value == name {
                return Some((index, arg));
            }
        }
        None
    }

    fn get_macro_by_name<'a>(defs: &'a [MacroDefinition], name: &str) -> Option<&'a MacroDefinition> {
        for def in defs {
            if def.name.value == name {
                return Some(def);
            }
        }
        None
    }

    fn parse_macro_def_arguments(tokens: &mut TokenIterator<IncludeToken>) -> Result<Vec<IncludeToken>, LexerError> {

        let mut arguments = Vec::new();
        tokens.expect(TokenType::OpenParen, None, "when parsing arguments list")?;

        while !tokens.peek_is(TokenType::CloseParen, None) {
            let next = tokens.expect(TokenType::Parameter, None, "while parsing macro arguments list")?;
            if tokens.peek_is(TokenType::Comma, None) {
                tokens.expect(TokenType::Comma, None, "")?;
            }
            arguments.push(next);
        }
        tokens.expect(TokenType::CloseParen, None, "when parsing arguments list")?;

        Ok(arguments)

    }

    fn parse_macro_call_arguments(tokens: &mut TokenIterator<IncludeToken>) -> Result<Vec<Vec<IncludeToken>>, LexerError> {

        let mut arguments = Vec::new();
        tokens.expect(TokenType::OpenParen, None, "when parsing argument list")?;

        let mut paren_depth = 1;
        let mut arg_tokens = Vec::new();
        while !tokens.peek_is(TokenType::CloseParen, None) || paren_depth > 1 {

            let next = tokens.get("Unexpected end of input while parsing macro arguments list.")?;
            if next.is(TokenType::OpenParen) {
                paren_depth += 1 ;

            } else if next.is(TokenType::CloseParen) {
                paren_depth -= 1;
            }

            if tokens.peek_is(TokenType::Comma, None) && paren_depth == 1 {
                arg_tokens.push(next);
                tokens.expect(TokenType::Comma, None, "")?;
                arguments.push(mem::replace(&mut arg_tokens, Vec::new()));

            } else {
                arg_tokens.push(next);
            }
        }

        if !arg_tokens.is_empty() {
            arguments.push(arg_tokens);
        }

        tokens.expect(TokenType::CloseParen, None, "when parsing argument list")?;
        Ok(arguments)

    }

    fn builtin_macro_defs() -> Vec<MacroDefinition> {
        vec![
            // Noop
            MacroDefinition::builtin("DBG", vec![], MacroReturnType::None),

            // String Operations
            MacroDefinition::builtin("STRUPR", vec![(MacroArgumenType::String, "text")], MacroReturnType::String),
            MacroDefinition::builtin("STRLWR", vec![(MacroArgumenType::String, "text")], MacroReturnType::String),
            MacroDefinition::builtin("STRSUB", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::Integer, "index"), (MacroArgumenType::Integer, "length")], MacroReturnType::String),
            MacroDefinition::builtin("STRIN", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "search")], MacroReturnType::String),
            MacroDefinition::builtin("STRPADR", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "padding"), (MacroArgumenType::Integer, "length")], MacroReturnType::String),
            MacroDefinition::builtin("STRPADL", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "padding"), (MacroArgumenType::Integer, "length")], MacroReturnType::String),

            // Geometry
            MacroDefinition::builtin("SIN", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("COS", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("TAN", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("ASIN", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("ACOS", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("ATAN", vec![(MacroArgumenType::Number, "radians")], MacroReturnType::Float),
            MacroDefinition::builtin("ATAN2", vec![(MacroArgumenType::Number, "y"), (MacroArgumenType::Number, "x")], MacroReturnType::Float),

            // Math
            MacroDefinition::builtin("LOG", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Float),
            MacroDefinition::builtin("EXP", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Float),
            MacroDefinition::builtin("FLOOR", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Integer),
            MacroDefinition::builtin("CEIL", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Integer),
            MacroDefinition::builtin("ROUND", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Integer),
            MacroDefinition::builtin("SQRT", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Float),
            MacroDefinition::builtin("ABS", vec![(MacroArgumenType::Number, "value")], MacroReturnType::Number),
            MacroDefinition::builtin("MAX", vec![(MacroArgumenType::Number, "a"), (MacroArgumenType::Number, "b")], MacroReturnType::Number),
            MacroDefinition::builtin("MIN", vec![(MacroArgumenType::Number, "a"), (MacroArgumenType::Number, "b")], MacroReturnType::Number),
            MacroDefinition::builtin("ATAN2", vec![(MacroArgumenType::Number, "from"), (MacroArgumenType::Number, "to")], MacroReturnType::Float),

            // Code
            MacroDefinition::builtin("BYTESIZE", vec![(MacroArgumenType::Tokens, "tokens")], MacroReturnType::Integer),
            MacroDefinition::builtin("CYCLES", vec![(MacroArgumenType::Tokens, "tokens")], MacroReturnType::Integer),
        ]
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{MacroLexer, MacroToken, MacroDefinition, MacroCall, InnerToken, IncludeToken, MacroArgumenType, MacroReturnType};
    use crate::lexer::mocks::include_lex;

    fn macro_lexer<S: Into<String>>(s: S) -> MacroLexer {
        MacroLexer::try_from(include_lex(s)).expect("MacroLexer failed")
    }

    fn macro_lexer_error<S: Into<String>>(s: S) -> String {
        MacroLexer::try_from(include_lex(s)).err().unwrap().to_string()
    }

    fn tfm<S: Into<String>>(s: S) -> Vec<MacroToken> {
        macro_lexer(s).tokens
    }

    macro_rules! mdef {
        ($name:expr, $args:expr, $body:expr) => {
            MacroDefinition {
                name: $name,
                parameters: $args,
                body: $body,
                return_type: MacroReturnType::None,
                builtin: false
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

    #[test]
    fn test_empty() {
        assert_eq!(tfm(""), vec![]);
    }

    #[test]
    fn test_passthrough() {
        assert_eq!(tfm("4\n@-2\nhl,.:"), vec![
            // TODO test pass through of all tokens
            mtk!(NumberLiteral, 0, 1, "4"),
            mtk!(Offset, 2, 5, "-2"),
            mtk!(Name, 6, 8, "hl"),
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
        let lexer = macro_lexer("MACRO FOO() ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO"), vec![], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_one_arg() {
        let lexer = macro_lexer("MACRO FOO(@a) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO"), vec![
                (MacroArgumenType::Any, itk!(10, 12, "a"))

            ], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_multiple_args() {
        let lexer = macro_lexer("MACRO FOO(@a, @b, @c) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO"), vec![
                (MacroArgumenType::Any, itk!(10, 12, "a")),
                (MacroArgumenType::Any, itk!(14, 16, "b")),
                (MacroArgumenType::Any, itk!(18, 20, "c"))

            ], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_body() {
        let lexer = macro_lexer("MACRO FOO() hl,a ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO"), vec![], vec![
                tk!(Name, 12, 14, "hl"),
                tk!(Comma, 14, 15, ","),
                tk!(Name, 15, 16, "a")
            ])
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
    fn test_macro_def_re_user() {
        assert_eq!(
            macro_lexer_error("MACRO FOO() ENDMACRO MACRO FOO() ENDMACRO"),
            "In file \"main.gb.s\" on line 1, column 28: Re-definition of user macro \"FOO\".\n\nMACRO FOO() ENDMACRO MACRO FOO() ENDMACRO\n                           ^--- Here\n\nOriginal definition was in file \"main.gb.s\" on line 1, column 7:\n\nMACRO FOO() ENDMACRO MACRO FOO() ENDMACRO\n      ^--- Here"
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
        let lexer = macro_lexer("2 MACRO FOO() ENDMACRO 4");
        assert_eq!(lexer.tokens, vec![
            mtk!(NumberLiteral, 0, 1, "2"),
            mtk!(NumberLiteral, 23, 24, "4"),
        ]);
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(8, 11, "FOO"), vec![], vec![])
        ]);
    }

    // Builtin Macro Calls ----------------------------------------------------

    #[test]
    fn test_macro_call_no_args() {
        let lexer = macro_lexer("DBG()");
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
        let lexer = macro_lexer("ABS(4)");
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
        let lexer = macro_lexer("ABS(`a b`)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "ABS"),
                vec![
                    vec![mtk!(Name, 5, 6, "a"), mtk!(Name, 7, 8, "b")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "ABS"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`"),
                    vec![
                        tk!(Name, 5, 6, "a"),
                        tk!(Name, 7, 8, "b")
                    ]
                )]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_call_multiple_args() {
        let lexer = macro_lexer("MAX(4, 2)");
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
        let lexer = macro_lexer("MAX((4, 2) + (2 - 1), 2)");
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
        let lexer = macro_lexer("MAX(MIN(4, FLOOR(2) + CEIL(1)), 2)");
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
        let lexer = macro_lexer("CEIL(BAR()) MACRO BAR() 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 24, 25, "4", 0)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_call_expression_args_recursive_with_user_unused_param() {
        let lexer = macro_lexer("CEIL(BAR(BAR(2))) MACRO BAR(@a) 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 32, 33, "4", 0)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 2);
    }

    #[test]
    fn test_macro_call_expression_args_recursive_with_user_used_param() {
        let lexer = macro_lexer("CEIL(BAR(BAR(4))) MACRO BAR(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            MacroToken::BuiltinCall(itk!(0, 4, "CEIL"), vec![
                vec![mtke!(NumberLiteral, 13, 14, "4", 1)],
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 3);
    }

    // User Macro Calls -------------------------------------------------------

    // TODO handle user macro call errors
    // TODO handle errors inside of macro expansions and show expanded macro in error message
    #[test]
    fn test_macro_user_call_no_args() {
        let lexer = macro_lexer("FOO() MACRO FOO() op 4 ENDMACRO");
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
        let lexer = macro_lexer("FOO(4) MACRO FOO(@a) op @a ENDMACRO");
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
        let lexer = macro_lexer("FOO((4 + 2)) MACRO FOO(@a) @a ENDMACRO");
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
        let lexer = macro_lexer("FOO(`a b`) MACRO FOO(@a) op @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 25, 27, "op", 0),
            mtke!(Name, 5, 6, "a", 0),
            mtke!(Name, 7, 8, "b", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`"),
                    vec![
                        tk!(Name, 5, 6, "a"),
                        tk!(Name, 7, 8, "b")
                    ]
                )]
            ])
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
    }

    #[test]
    fn test_macro_user_call_multiple_args() {
        let lexer = macro_lexer("FOO(4, 8) MACRO FOO(@a, @b) op @b @a ENDMACRO");
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
        let lexer = macro_lexer("FOO() MACRO FOO() 4 BAR() ENDMACRO MACRO BAR() 8 ENDMACRO");
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
        let lexer = macro_lexer("FOO(`BAR()`) MACRO FOO(@a) 4 @a ENDMACRO MACRO BAR() 8 ENDMACRO");
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
        let lexer = macro_lexer("FOO(`@b`, `@c`, 4) MACRO FOO(@a, @b, @c) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 16, 17, "4", 0),
        ]);
        assert_eq!(lexer.macro_calls_count(), 1);
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
            macro_lexer("FOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\nFOO()\n MACRO FOO() ENDMACRO").macro_calls_count(),
            9
        );
    }

    #[test]
    fn test_macro_user_recursive_params() {
        let lexer = macro_lexer("FOO(FOO(FOO(4))) MACRO FOO(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 12, 13, "4", 2),
        ]);
        assert_eq!(lexer.macro_calls_count(), 3);
    }

}

