// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;

// Internal Dependencies ------------------------------------------------------
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler;

impl Compiler {
    pub fn compile<T: FileReader>(reader: T, entry: PathBuf) -> Result<(), Box<dyn Error>> {

        let include_lexer = Lexer::<IncludeStage>::from_file(&reader, &entry).map_err(|e| CompilerError::new("INCLUDE", e))?;
        let included_token_count = include_lexer.len();
        println!("Included {} token(s).", included_token_count);

        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| CompilerError::new("MACRO EXPANSION", e))?;
        println!("{} token(s) after macro expansions.", macro_lexer.len());

        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| CompilerError::new("VALUE CONVERSION", e))?;
        println!("{} token(s) after value construction.", value_lexer.len());

        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| CompilerError::new("EXPRESSION CONSTRUCTION", e))?;
        println!("{} token(s) after expression construction.", expr_lexer.len());

        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| CompilerError::new("ENTRY CONSTRUCTION", e))?;
        println!("{} token(s) after entry construction.", entry_lexer.len());


        // Go through all Entry Tokens
            // Extract Constants
            // Add Remaining Tokens to Layout

        // Resolve Constants
        // Resolve Sizes
        // Create a mapping of all labels defs to their rom location/offsets
        // Resolve all other expressions
            // Resolve labels via reference id
            // Also match macro_call_id of tokens
        // Optimize
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
        write!(f, "CompilerError: {}", self.source)
    }
}

impl Error for CompilerError {}

