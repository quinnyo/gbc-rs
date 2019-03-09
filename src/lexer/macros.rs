// STD Dependencies -----------------------------------------------------------
use std::mem;
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{IncludeLexer, InnerToken, TokenIterator, LexerFile, LexerToken, LexerError, TokenType};
use super::include::IncludeToken;


// Include Specific Tokens ----------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum MacroToken {
    Name(InnerToken),
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
            _ => unreachable!()
        }
    }
}

impl LexerToken for MacroToken {

    fn typ(&self) -> TokenType {
        match self {
            MacroToken::Name(_) => TokenType::Name,
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
            MacroToken::Name(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
            | MacroToken::StringLiteral(inner) | MacroToken::BinaryFile(inner, _) | MacroToken::BuiltinCall(inner, _)
            | MacroToken::Comma(inner) | MacroToken::Point(inner) | MacroToken::Colon(inner) | MacroToken::Operator(inner) | MacroToken::Comment(inner)
            | MacroToken::OpenParen(inner) | MacroToken::CloseParen(inner) | MacroToken::OpenBracket(inner) | MacroToken::CloseBracket(inner) => {
                &inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            MacroToken::Name(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
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
struct MacroDefinition {
    name: InnerToken,
    arguments: Vec<InnerToken>,
    // TODO when expanding set expanded to true for all body tokens
    body: Vec<IncludeToken>,
    builtin: bool
}

impl MacroDefinition {
    fn builtin(name: &str, arguments: Vec<&str>) -> Self {
        Self {
            name: InnerToken::new(0, 0, 0, name.into(), name.into()),
            arguments: arguments.into_iter().map(|name| {
                InnerToken::new(0, 0, 0, format!("@{}", name), name.into())

            }).collect(),
            body: Vec::new(),
            builtin: true
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct MacroCall {
    name: InnerToken,
    arguments: Vec<Vec<IncludeToken>>
}


// Macro Level Lexer Implementation -------------------------------------------
pub struct MacroLexer {
    files: Vec<LexerFile>,
    // TODO setup built-in macros
    macro_defs: Vec<MacroDefinition>,
    macro_calls: Vec<MacroCall>,
    tokens: Vec<MacroToken>
}

impl MacroLexer {

    pub fn try_from(lexer: IncludeLexer) -> Result<MacroLexer, Box<dyn Error>> {

        let files = lexer.files;
        let (macro_defs, macro_calls, tokens) = Self::from_tokens(lexer.tokens).map_err(|err| {
            LexerFile::error(err, &files)
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

    fn from_tokens(tokens: Vec<IncludeToken>) -> Result<(Vec<MacroDefinition>, Vec<MacroCall>, Vec<MacroToken>), LexerError> {

        let mut builtin_macro_defs = Self::builtin_macro_defs();
        let mut user_macro_defs = Vec::new();
        let mut macro_calls = Vec::new();
        let mut tokens_without_macro_defs = Vec::new();

        // Drop all newlines
        let mut tokens = TokenIterator::new(tokens.into_iter().filter(|t| !t.is(TokenType::Newline)).collect());

        // Extract Macro Definitions
        while let Some(token) = tokens.next() {
            if token.is(TokenType::Name) && token.has_value("MACRO") {

                // Grab name and argument tokens
                let name_token = tokens.expect(TokenType::Name, None, "when parsing macro definition")?;
                // TODO check macro re-definition and throw error (both user and builtin)
                // TODO Add support to LexerError to show a previously declared location based on
                // another InnerToken

                let arg_tokens = Self::parse_macro_def_arguments(&mut tokens)?;

                // Collect Body Tokens
                let mut body_tokens = Vec::new();
                while !tokens.peek(TokenType::Name, Some("ENDMACRO")) {
                    // TODO if token type is MACRO then raise error to prevent macro defs inside
                    // other macros
                    body_tokens.push(tokens.get("while parsing macro body")?);
                }
                tokens.expect(TokenType::Name, Some("ENDMACRO"), "when parsing macro definition")?;

                // Add Macro Definition
                user_macro_defs.push(MacroDefinition {
                    name: name_token.into_inner(),
                    arguments: arg_tokens.into_iter().map(|t| t.into_inner()).collect(),
                    body: body_tokens,
                    builtin: false
                });

            } else {
                tokens_without_macro_defs.push(token);
            }
        }

        // Recursively expand Macro Calls
        let mut tokens_without_macro_calls = Vec::new();
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
                    if arg_tokens.len() != macro_def.arguments.len() {
                        return Err(token.error(format!(
                            "Incorrect number of parameters for invocation of macro \"{}\", expected {} parameter(s) but got {}",
                            token.value(),
                            macro_def.arguments.len(),
                            arg_tokens.len()
                        )));
                    }

                    // Add Macro Call
                    macro_calls.push(MacroCall {
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
                                expanded
                            }).collect()
                        ));

                    // Expand user calls
                    } else {
                        let mut expanded = Vec::new();
                        for token in macro_def.body.clone() {
                            if token.is(TokenType::Parameter) {
                                // TODO if a parameter name is not declared in the macro def raise an error
                                // TODO expand all parameters inside the copied tokens with their
                                // corresponding argument tokens
                                // TODO flatten token groups from parameters
                                // TODO append tokens

                            } else {
                                expanded.push(token);
                            }
                        }
                        // TODO append to tokens_without_macro_calls
                        expanded_macro_calls += 1;
                    }

                } else {
                    tokens_without_macro_calls.push(token);
                }
            }

            if expanded_macro_calls > 0 {
                // Try to expand any newly introduced calls from the current expansion
                tokens_without_macro_defs = tokens_without_macro_calls.drain(0..).collect();

            } else {
                break;
            }
        }

        // TODO check for any left over parameter or token groups and error
        for t in &tokens_without_macro_calls {
            if t.is(TokenType::Parameter) {
                // TODO return Err Unexpected Parameter outside of MacroDefinition

            } else if t.is(TokenType::TokenGroup) {
                // TODO return Err Unexpected TokenGroup outside of MacroCall
            }
        }

        Ok((
            user_macro_defs,
            macro_calls,
            tokens_without_macro_calls.into_iter().map(MacroToken::from).collect()
        ))

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
            MacroDefinition::builtin("DBG", vec![]),
            MacroDefinition::builtin("ABS", vec!["value"]),
            MacroDefinition::builtin("MAX", vec!["a", "b"]),
            MacroDefinition::builtin("STRUPR", vec!["str"]),
        ]
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{MacroLexer, MacroToken, MacroDefinition, MacroCall, InnerToken, IncludeToken};
    use crate::lexer::mocks::include_lex;

    fn macro_lexer<S: Into<String>>(s: S) -> MacroLexer {
        MacroLexer::try_from(include_lex(s)).expect("MacroLexer failed")
    }

    fn tfm<S: Into<String>>(s: S) -> Vec<MacroToken> {
        macro_lexer(s).tokens
    }

    macro_rules! mdef {
        ($name:expr, $args:expr, $body:expr) => {
            MacroDefinition {
                name: $name,
                arguments: $args,
                body: $body,
                builtin: false
            }
        }
    }

    macro_rules! mcall {
        ($name:expr, $args:expr) => {
            MacroCall {
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
                itk!(10, 12, "@a", "a")

            ], vec![])
        ]);
    }

    #[test]
    fn test_macro_def_multiple_args() {
        let lexer = macro_lexer("MACRO FOO(@a, @b, @c) ENDMACRO");
        assert!(lexer.tokens.is_empty());
        assert_eq!(lexer.macro_defs, vec![
            mdef!(itk!(6, 9, "FOO", "FOO"), vec![
                itk!(10, 12, "@a", "a"),
                itk!(14, 16, "@b", "b"),
                itk!(18, 20, "@c", "c")

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

    // Macro Calls ------------------------------------------------------------

    // TODO handle macro call errors
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
            mcall!(itk!(0, 3, "DBG", "DBG"), vec![])
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
            mcall!(itk!(0, 3, "ABS", "ABS"), vec![
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
            mcall!(itk!(0, 3, "ABS", "ABS"), vec![
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
            mcall!(itk!(0, 3, "MAX", "MAX"), vec![
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
            mcall!(itk!(0, 3, "MAX", "MAX"), vec![
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

}

