// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;

// Internal Dependencies ------------------------------------------------------
use crate::lexer::{IncludeLexer, MacroLexer};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler;

impl Compiler {
    pub fn compile<T: FileReader>(reader: T, entry: PathBuf) -> Result<(), Box<dyn Error>> {
        let include_lexer = IncludeLexer::from_file(&reader, &entry).map_err(|e| CompilerError::new("INCLUDE", e))?;
        let included_token_count = include_lexer.len();
        let macro_lexer = MacroLexer::try_from(include_lexer).map_err(|e| CompilerError::new("MACRO EXPANSION", e))?;
        println!("Included {} token(s).", included_token_count);
        println!("Found {} defined macro(s).", macro_lexer.macro_defs_count());
        println!("Found {} macro call(s).", macro_lexer.macro_calls_count());
        println!("{} token(s) after macro expansions.", macro_lexer.len());
        // TODO ValueLexer
        // TODO ExpressionLexer
        // TODO Parser
        // TODO Optimizer
        // TODO Generator
        Ok(())
    }
}


// Compiler Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct CompilerError {
    stage: String,
    source: Box<Error>
}

impl CompilerError {
    fn new(stage: &str, source: Box<Error>) -> Self {
        Self {
            stage: stage.to_string(),
            source
        }

    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.source)
    }
}

impl Error for CompilerError {}

