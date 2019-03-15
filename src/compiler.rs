// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;

// Internal Dependencies ------------------------------------------------------
use crate::lexer::{IncludeLexer, MacroLexer, ValueLexer, ExpressionLexer, EntryLexer};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler;

impl Compiler {
    pub fn compile<T: FileReader>(reader: T, entry: PathBuf) -> Result<(), Box<dyn Error>> {
        let include_lexer = IncludeLexer::from_file(&reader, &entry).map_err(|e| CompilerError::new("INCLUDE", e))?;
        let included_token_count = include_lexer.len();
        println!("Included {} token(s).", included_token_count);

        let macro_lexer = MacroLexer::try_from(include_lexer).map_err(|e| CompilerError::new("MACRO EXPANSION", e))?;
        println!("Found {} defined macro(s).", macro_lexer.macro_defs_count());
        println!("Found {} macro call(s).", macro_lexer.macro_calls_count());
        println!("{} token(s) after macro expansions.", macro_lexer.len());

        let value_lexer = ValueLexer::try_from(macro_lexer).map_err(|e| CompilerError::new("VALUE CONVERSION", e))?;
        println!("{} token(s) after value construction.", value_lexer.len());

        let expr_lexer = ExpressionLexer::try_from(value_lexer).map_err(|e| CompilerError::new("EXPRESSION CONSTRUCTION", e))?;
        println!("{} token(s) after expression construction.", expr_lexer.len());

        let entry_lexer = EntryLexer::try_from(expr_lexer).map_err(|e| CompilerError::new("ENTRY CONSTRUCTION", e))?;
        println!("{} token(s) after entry construction.", entry_lexer.len());

        // TODO EntryLexer, removes: Name, Comma, OpenBracket, CloseBracket, Flag, Register -> Generates: Sections, Data, Constants, Variables, Instructions)

        // TODO ROM Layout
            // TODO handle file local global labels which start with a "_" by searching for a
            // matching label
            // TODO the names should already have been made unique in the value stage
        // TODO Optimizer
        // TODO Generator
        // Parser
        // TODO 1. Go through all sections ordered by base adress ascending
            // TODO 1.1. Calculate offsets for all the section's entries
            // TODO 1.2. Check for section overlaps / out of bounds

        // TODO 2. Peform jump target resolution
        // TODO 3. Perform instruction optimizations
        // TODO 4. go back to 1 and repeat until no more optimizations can be applied

        // TODO 1. Go through all sections ordered by base adress ascending
        // TODO 2. Serialize all section entries into the corresponding ROM part

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

