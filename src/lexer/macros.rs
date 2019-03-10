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
#[derive(Debug, Eq, PartialEq)]
pub enum MacroToken {
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
    BuiltinCall(InnerToken, Vec<Vec<MacroToken>>),
    Comma(InnerToken),
    Point(InnerToken),
    Colon(InnerToken),
    Operator(InnerToken),
    Comment(InnerToken),
    OpenParen(InnerToken),
    CloseParen(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
}

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
                println!("Token {:?} should not be passed through MacroLexer", token);
                unreachable!()
            }
        }
    }
}

impl LexerToken for MacroToken {

    fn typ(&self) -> TokenType {
        match self {
            MacroToken::Name(_) => TokenType::Name,
            MacroToken::Reserved(_) => TokenType::Reserved,
            MacroToken::Instruction(_) => TokenType::Instruction,
            MacroToken::Offset(_) => TokenType::Offset,
            MacroToken::NumberLiteral(_) => TokenType::NumberLiteral,
            MacroToken::StringLiteral(_) => TokenType::StringLiteral,
            MacroToken::BinaryFile(_, _) => TokenType::BinaryFile,
            MacroToken::BuiltinCall(_, _) => TokenType::BuiltinCall,
            MacroToken::Comma(_) => TokenType::Comma,
            MacroToken::Point(_) => TokenType::Point,
            MacroToken::Colon(_) => TokenType::Colon,
            MacroToken::Operator(_) => TokenType::Operator,
            MacroToken::Comment(_) => TokenType::Comment,
            MacroToken::OpenParen(_) => TokenType::OpenParen,
            MacroToken::CloseParen(_) => TokenType::CloseParen,
            MacroToken::OpenBracket(_) => TokenType::OpenBracket,
            MacroToken::CloseBracket(_) => TokenType::CloseBracket
        }

    }

    fn inner(&self) -> &InnerToken {
        match self {
            MacroToken::Name(inner) | MacroToken::Reserved(inner) | MacroToken::Instruction(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
            | MacroToken::StringLiteral(inner) | MacroToken::BinaryFile(inner, _) | MacroToken::BuiltinCall(inner, _)
            | MacroToken::Comma(inner) | MacroToken::Point(inner) | MacroToken::Colon(inner) | MacroToken::Operator(inner) | MacroToken::Comment(inner)
            | MacroToken::OpenParen(inner) | MacroToken::CloseParen(inner) | MacroToken::OpenBracket(inner) | MacroToken::CloseBracket(inner) => {
                &inner
            }
        }
    }

    fn inner_mut(&mut self) -> &mut InnerToken {
        match self {
            MacroToken::Name(inner) | MacroToken::Reserved(inner) | MacroToken::Instruction(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
            | MacroToken::StringLiteral(inner) | MacroToken::BinaryFile(inner, _) | MacroToken::BuiltinCall(inner, _)
            | MacroToken::Comma(inner) | MacroToken::Point(inner) | MacroToken::Colon(inner) | MacroToken::Operator(inner) | MacroToken::Comment(inner)
            | MacroToken::OpenParen(inner) | MacroToken::CloseParen(inner) | MacroToken::OpenBracket(inner) | MacroToken::CloseBracket(inner) => {
                inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            MacroToken::Name(inner) | MacroToken::Reserved(inner) | MacroToken::Instruction(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
            | MacroToken::StringLiteral(inner) | MacroToken::BinaryFile(inner, _) | MacroToken::BuiltinCall(inner, _)
            | MacroToken::Comma(inner) | MacroToken::Point(inner) | MacroToken::Colon(inner) | MacroToken::Operator(inner) | MacroToken::Comment(inner)
            | MacroToken::OpenParen(inner) | MacroToken::CloseParen(inner) | MacroToken::OpenBracket(inner) | MacroToken::CloseBracket(inner) => {
                inner
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
    Number
}

#[derive(Debug, Eq, PartialEq)]
struct MacroDefinition {
    name: InnerToken,
    parameters: Vec<(MacroArgumenType, InnerToken)>,
    body: Vec<IncludeToken>,
    builtin: bool
}

impl MacroDefinition {
    fn builtin(name: &str, parameters: Vec<(MacroArgumenType, &str)>) -> Self {
        Self {
            name: InnerToken::new(0, 0, 0, name.into(), name.into()),
            parameters: parameters.into_iter().map(|(typ, name)| {
                (typ, InnerToken::new(0, 0, 0, format!("@{}", name), name.into()))

            }).collect(),
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

    fn from_tokens(tokens: Vec<IncludeToken>, macro_calls: &mut Vec<MacroCall>) -> Result<(Vec<MacroDefinition>, Vec<MacroToken>), LexerError> {

        let builtin_macro_defs = Self::builtin_macro_defs();
        let mut user_macro_defs = Vec::new();
        //let mut macro_calls = Vec::new();
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
                    // TODO Include pointer to original definition in error
                    return Err(name_token.error(format!("Re-definition of builtin macro \"{}\".", name_token.value())));

                } else if let Some(_) = Self::get_macro_by_name(&user_macro_defs, name_token.value()) {
                    // TODO Include pointer to original definition in error
                    return Err(name_token.error(format!("Re-definition of user macro \"{}\".", name_token.value())));
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
                while !tokens.peek(TokenType::Reserved, Some("ENDMACRO")) {
                    let token = tokens.get("while parsing macro body")?;
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
                    builtin: false
                });

            } else {
                tokens_without_macro_defs.push(token);
            }
        }

        // Recursively expand Macro Calls
        let mut tokens_without_macro_calls = Vec::new();
        let mut macro_call_id = 0;
        let mut expansion_depth = 0;
        loop {
            let mut expanded_macro_calls = 0;
            let mut tokens = TokenIterator::new(tokens_without_macro_defs);
            while let Some(token) = tokens.next() {
                if token.is(TokenType::Name) && tokens.peek(TokenType::OpenParen, None) {
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
                        id: macro_call_id,
                        name: token.clone().into_inner(),
                        arguments: arg_tokens.clone()
                    });

                    // Replace builtin calls
                    if macro_def.builtin {
                        tokens_without_macro_calls.push(IncludeToken::BuiltinCall(
                            token.into_inner(),
                            arg_tokens.clone().into_iter().map(|tokens| {
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
                                // Filter out new lines
                                expanded.into_iter().filter(|t| !t.is(TokenType::Newline)).collect()

                            }).collect()
                        ));

                    // Expand user calls
                    } else {
                        let mut expanded = Vec::new();
                        for token in macro_def.body.clone() {
                            // Recursively expand all body tokens
                            expanded.append(&mut Self::expand_macro_token(&macro_def, &arg_tokens, token, macro_call_id)?);
                        }
                        tokens_without_macro_calls.append(
                            // Filter out newlines
                            &mut expanded.into_iter().filter(|t| !t.is(TokenType::Newline)).collect()
                        );
                        expanded_macro_calls += 1;
                    }
                    macro_call_id += 1;

                } else {
                    tokens_without_macro_calls.push(token);
                }
            }

            if expanded_macro_calls > 0 {

                // Try to expand any newly introduced calls from the current expansion
                tokens_without_macro_defs = tokens_without_macro_calls.drain(0..).collect();
                expansion_depth += 1;

                if expansion_depth > MAX_EXPANSION_DEPTH {
                    let last_macro_call = macro_calls.last().unwrap();
                    return Err(last_macro_call.name.error(format!(
                        "Maximum recursion limit of {} reached during expansion of macro \"{}\".",
                        MAX_EXPANSION_DEPTH,
                        last_macro_call.name.value
                    )));
                }

            } else {
                break;
            }
        }

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

    fn expand_macro_token(
        macro_def: &MacroDefinition,
        arg_tokens: &[Vec<IncludeToken>],
        mut token: IncludeToken,
        macro_call_id: usize

    ) -> Result<Vec<IncludeToken>, LexerError> {
        // TODO When later errors happen should we show a "Error in file line... during expansion
        // of marco from file ... line..." ?
        // TODO The other lexers need to keep the MacroCalls as a reference and feed it into the
        // LexerError
        let mut expanded = Vec::new();
        token.inner_mut().set_macro_call_id(macro_call_id);
        if token.is(TokenType::Parameter) {
            if let Some((index, _)) = Self::get_macro_def_param_by_name(&macro_def, token.value()) {

                // Insert and expand the argument's tokens
                let call_argument = arg_tokens[index].clone();
                for mut token in call_argument {

                    if let IncludeToken::TokenGroup(_, group) = token {
                        for mut token in group {
                            token.inner_mut().set_macro_call_id(macro_call_id);

                            // Expand any inserted parameters or token groups
                            expanded.append(&mut Self::expand_macro_token(
                                &macro_def,
                                &arg_tokens,
                                token,
                                macro_call_id
                            )?);
                        }

                    } else {
                        token.inner_mut().set_macro_call_id(macro_call_id);

                        // Expand any inserted parameters or token groups
                        expanded.append(&mut Self::expand_macro_token(
                            &macro_def,
                            &arg_tokens,
                            token,
                            macro_call_id
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

        while !tokens.peek(TokenType::CloseParen, None) {
            let next = tokens.expect(TokenType::Parameter, None, "while parsing macro arguments list")?;
            if tokens.peek(TokenType::Comma, None) {
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
        while !tokens.peek(TokenType::CloseParen, None) || paren_depth > 1 {

            let next = tokens.get("while parsing macro arguments list")?;
            if next.is(TokenType::OpenParen) {
                paren_depth += 1 ;

            } else if next.is(TokenType::CloseParen) {
                paren_depth -= 1;
            }

            if tokens.peek(TokenType::Comma, None) && paren_depth == 1 {
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
            MacroDefinition::builtin("DBG", vec![]),

            // String Operations
            MacroDefinition::builtin("STRUPR", vec![(MacroArgumenType::String, "text")]),
            MacroDefinition::builtin("STRLWR", vec![(MacroArgumenType::String, "text")]),
            MacroDefinition::builtin("STRSUB", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::Number, "index"), (MacroArgumenType::Number, "length")]),
            MacroDefinition::builtin("STRIN", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "search")]),
            MacroDefinition::builtin("STRPADR", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "padding"), (MacroArgumenType::Number, "length")]),
            MacroDefinition::builtin("STRPADL", vec![(MacroArgumenType::String, "text"), (MacroArgumenType::String, "padding"), (MacroArgumenType::Number, "length")]),

            // Geometry
            MacroDefinition::builtin("SIN", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("COS", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("TAN", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("ASIN", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("ACOS", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("ATAN", vec![(MacroArgumenType::Number, "radians")]),
            MacroDefinition::builtin("ATAN2", vec![(MacroArgumenType::Number, "y"), (MacroArgumenType::Number, "x")]),

            // Math
            MacroDefinition::builtin("LOG", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("EXP", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("FLOOR", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("CEIL", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("ROUND", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("SQRT", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("ABS", vec![(MacroArgumenType::Number, "value")]),
            MacroDefinition::builtin("MAX", vec![(MacroArgumenType::Number, "a"), (MacroArgumenType::Number, "b")]),
            MacroDefinition::builtin("MIN", vec![(MacroArgumenType::Number, "a"), (MacroArgumenType::Number, "b")]),
            MacroDefinition::builtin("ATAN2", vec![(MacroArgumenType::Number, "from"), (MacroArgumenType::Number, "to")]),

            // Code
            MacroDefinition::builtin("BYTESIZE", vec![(MacroArgumenType::Tokens, "tokens")]),
            MacroDefinition::builtin("CYCLES", vec![(MacroArgumenType::Tokens, "tokens")]),
        ]
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{MacroLexer, MacroToken, MacroDefinition, MacroCall, InnerToken, IncludeToken, MacroArgumenType};
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
        ($start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $raw.into(), $parsed.into())
        }
    }

    macro_rules! itke {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = InnerToken::new(0, $start, $end, $raw.into(), $parsed.into());
                t.set_macro_call_id($id);
                t
            }
        }
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            IncludeToken::$tok(itk!($start, $end, $raw, $parsed))
        }
    }

    macro_rules! mtk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            MacroToken::$tok(itk!($start, $end, $raw, $parsed))
        }
    }

    macro_rules! mtke {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr, $id:expr) => {
            {
                let mut t = itk!($start, $end, $raw, $parsed);
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
            mtk!(NumberLiteral, 0, 1, "4", "4"),
            mtk!(Offset, 2, 5, "@-2", "-2"),
            mtk!(Name, 6, 8, "hl", "hl"),
            mtk!(Comma, 8, 9, ",", ","),
            mtk!(Point, 9, 10, ".", "."),
            mtk!(Colon, 10, 11, ":", ":"),
        ]);
    }

    // Macro Definition -------------------------------------------------------

    // TODO handle macro definition errors
    #[test]
    fn test_macro_def_no_args_no_body() {
        let lexer = macro_lexer("MACRO FOO() ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO", "FOO"), vec![], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_one_arg() {
        let lexer = macro_lexer("MACRO FOO(@a) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO", "FOO"), vec![
                (MacroArgumenType::Any, itk!(10, 12, "@a", "a"))

            ], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_multiple_args() {
        let lexer = macro_lexer("MACRO FOO(@a, @b, @c) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO", "FOO"), vec![
                (MacroArgumenType::Any, itk!(10, 12, "@a", "a")),
                (MacroArgumenType::Any, itk!(14, 16, "@b", "b")),
                (MacroArgumenType::Any, itk!(18, 20, "@c", "c"))

            ], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_body() {
        let lexer = macro_lexer("MACRO FOO() hl,a ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO", "FOO"), vec![], vec![
                tk!(Name, 12, 14, "hl", "hl"),
                tk!(Comma, 14, 15, ",", ","),
                tk!(Name, 15, 16, "a", "a")
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
            "In file \"main.gb.s\" on line 1, column 28: Re-definition of user macro \"FOO\".\n\nMACRO FOO() ENDMACRO MACRO FOO() ENDMACRO\n                           ^--- Here"
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
            mtk!(NumberLiteral, 0, 1, "2", "2"),
            mtk!(NumberLiteral, 23, 24, "4", "4"),
        ]);
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(8, 11, "FOO", "FOO"), vec![], vec![])
        ]);
    }

    // Builtin Macro Calls ----------------------------------------------------

    // TODO handle builtin macro call errors
    #[test]
    fn test_macro_call_no_args() {
        let lexer = macro_lexer("DBG()");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "DBG", "DBG"),
                vec![]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "DBG", "DBG"), vec![])
        ]);
    }

    #[test]
    fn test_macro_call_one_arg() {
        let lexer = macro_lexer("ABS(4)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "ABS", "ABS"),
                vec![
                    vec![mtk!(NumberLiteral, 4, 5, "4", "4")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "ABS", "ABS"), vec![
                vec![tk!(NumberLiteral, 4, 5, "4", "4")]
            ])
        ]);
    }

    #[test]
    fn test_macro_call_unwrap_token_group_args() {
        let lexer = macro_lexer("ABS(`a b`)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "ABS", "ABS"),
                vec![
                    vec![mtk!(Name, 5, 6, "a", "a"), mtk!(Name, 7, 8, "b", "b")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "ABS", "ABS"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`", "`"),
                    vec![
                        tk!(Name, 5, 6, "a", "a"),
                        tk!(Name, 7, 8, "b", "b")
                    ]
                )]
            ])
        ]);
    }

    #[test]
    fn test_macro_call_multiple_args() {
        let lexer = macro_lexer("MAX(4, 2)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "MAX", "MAX"),
                vec![
                    vec![mtk!(NumberLiteral, 4, 5, "4", "4")],
                    vec![mtk!(NumberLiteral, 7, 8, "2", "2")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "MAX", "MAX"), vec![
                vec![tk!(NumberLiteral, 4, 5, "4", "4")],
                vec![tk!(NumberLiteral, 7, 8, "2", "2")]
            ])
        ]);
    }

    #[test]
    fn test_macro_call_expression_args() {
        let lexer = macro_lexer("MAX((4, 2) + (2 - 1), 2)");
        assert_eq!(lexer.tokens, vec![
           MacroToken::BuiltinCall(
                itk!(0, 3, "MAX", "MAX"),
                vec![
                    vec![
                        mtk!(OpenParen, 4, 5, "(", "("),
                        mtk!(NumberLiteral, 5, 6, "4", "4"),
                        mtk!(Comma, 6, 7, ",", ","),
                        mtk!(NumberLiteral, 8, 9, "2", "2"),
                        mtk!(CloseParen, 9, 10, ")", ")"),
                        mtk!(Operator, 11, 12, "+", "+"),
                        mtk!(OpenParen, 13, 14, "(", "("),
                        mtk!(NumberLiteral, 14, 15, "2", "2"),
                        mtk!(Operator, 16, 17, "-", "-"),
                        mtk!(NumberLiteral, 18, 19, "1", "1"),
                        mtk!(CloseParen, 19, 20, ")", ")"),
                    ],
                    vec![mtk!(NumberLiteral, 22, 23, "2", "2")]
                ]
           )
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "MAX", "MAX"), vec![
                vec![
                    tk!(OpenParen, 4, 5, "(", "("),
                    tk!(NumberLiteral, 5, 6, "4", "4"),
                    tk!(Comma, 6, 7, ",", ","),
                    tk!(NumberLiteral, 8, 9, "2", "2"),
                    tk!(CloseParen, 9, 10, ")", ")"),
                    tk!(Operator, 11, 12, "+", "+"),
                    tk!(OpenParen, 13, 14, "(", "("),
                    tk!(NumberLiteral, 14, 15, "2", "2"),
                    tk!(Operator, 16, 17, "-", "-"),
                    tk!(NumberLiteral, 18, 19, "1", "1"),
                    tk!(CloseParen, 19, 20, ")", ")"),
                ],
                vec![tk!(NumberLiteral, 22, 23, "2", "2")]
            ])
        ]);
    }

    // User Macro Calls -------------------------------------------------------

    // TODO handle user macro call errors
    // TODO handle errors inside of macro expansions and show expanded macro in error message
    #[test]
    fn test_macro_user_call_no_args() {
        let lexer = macro_lexer("FOO() MACRO FOO() op 4 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 18, 20, "op", "op", 0),
            mtke!(NumberLiteral, 21, 22, "4", "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![])
        ]);
    }

    #[test]
    fn test_macro_user_call_one_arg() {
        let lexer = macro_lexer("FOO(4) MACRO FOO(@a) op @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 21, 23, "op", "op", 0),
            mtke!(NumberLiteral, 4, 5, "4", "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![
                vec![
                    tk!(NumberLiteral, 4, 5, "4", "4")
                ]
            ])
        ]);
    }

    #[test]
    fn test_macro_user_call_expression() {
        let lexer = macro_lexer("FOO((4 + 2)) MACRO FOO(@a) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(OpenParen, 4, 5, "(", "(", 0),
            mtke!(NumberLiteral, 5, 6, "4", "4", 0),
            mtke!(Operator, 7, 8, "+", "+", 0),
            mtke!(NumberLiteral, 9, 10, "2", "2", 0),
            mtke!(CloseParen, 10, 11, ")", ")", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![
                vec![
                    tk!(OpenParen, 4, 5, "(", "("),
                    tk!(NumberLiteral, 5, 6, "4", "4"),
                    tk!(Operator, 7, 8, "+", "+"),
                    tk!(NumberLiteral, 9, 10, "2", "2"),
                    tk!(CloseParen, 10, 11, ")", ")"),
                ]
            ])
        ]);
    }

    #[test]
    fn test_macro_user_call_token_group_arg() {
        let lexer = macro_lexer("FOO(`a b`) MACRO FOO(@a) op @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 25, 27, "op", "op", 0),
            mtke!(Name, 5, 6, "a", "a", 0),
            mtke!(Name, 7, 8, "b", "b", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`", "`"),
                    vec![
                        tk!(Name, 5, 6, "a", "a"),
                        tk!(Name, 7, 8, "b", "b")
                    ]
                )]
            ])
        ]);
    }

    #[test]
    fn test_macro_user_call_multiple_args() {
        let lexer = macro_lexer("FOO(4, 8) MACRO FOO(@a, @b) op @b @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(Name, 28, 30, "op", "op", 0),
            mtke!(NumberLiteral, 7, 8, "8", "8", 0),
            mtke!(NumberLiteral, 4, 5, "4", "4", 0),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![
                vec![
                    tk!(NumberLiteral, 4, 5, "4", "4")
                ],
                vec![
                    tk!(NumberLiteral, 7, 8, "8", "8")
                ]
            ])
        ]);
    }

    // TODO test recursive call errors
    #[test]
    fn test_macro_user_recursive() {
        let lexer = macro_lexer("FOO() MACRO FOO() 4 BAR() ENDMACRO MACRO BAR() 8 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 18, 19, "4", "4", 0),
            mtke!(NumberLiteral, 47, 48, "8", "8", 1),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![]),
            mcall!(1, itke!(20, 23, "BAR", "BAR", 0), vec![])
        ]);
    }

    #[test]
    fn test_macro_user_recursive_token_group() {
        let lexer = macro_lexer("FOO(`BAR()`) MACRO FOO(@a) 4 @a ENDMACRO MACRO BAR() 8 ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 27, 28, "4", "4", 0),
            mtke!(NumberLiteral, 53, 54, "8", "8", 1),
        ]);
        assert_eq!(lexer.macro_calls, vec![
            mcall!(0, itk!(0, 3, "FOO", "FOO"), vec![
                vec![IncludeToken::TokenGroup(
                    itk!(4, 5, "`", "`"),
                    vec![
                        tk!(Name, 5, 8, "BAR", "BAR"),
                        tk!(OpenParen, 8, 9, "(", "("),
                        tk!(CloseParen, 9, 10, ")", ")")
                    ]
                )]
            ]),
            mcall!(1, itke!(5, 8, "BAR", "BAR", 0), vec![])
        ]);
    }

    #[test]
    fn test_macro_user_recursive_token_group_parameters() {
        let lexer = macro_lexer("FOO(`@b`, `@c`, 4) MACRO FOO(@a, @b, @c) @a ENDMACRO");
        assert_eq!(lexer.tokens, vec![
            mtke!(NumberLiteral, 16, 17, "4", "4", 0),
        ]);
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

}

