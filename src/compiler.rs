// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;

// Internal Dependencies ------------------------------------------------------
use crate::lexer::{IncludeLexer, MacroLexer, ValueLexer};
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

        let value_lexer = ValueLexer::try_from(macro_lexer).map_err(|e| CompilerError::new("MACRO EXPANSION", e))?;
        println!("{} token(s) after value construction.", value_lexer.len());

        // TODO ExpressionLexer, removes: OpenParen, CloseParen, Offset, Float, Integer, String, GlobalLabelDef, LocalLabelDef, LocalLabelRef, Operator -> Generates: Expressions
        //
        // TODO EntryLexer, removes: Comma, OpenBracket, CloseBracket -> Generates: Sections, Data, Variables, Instructions)
            // TODO Everything at this stage takes expansion arguments of either Type=Number or Type=String
            // TODO When does Integer Conversion happen? Are Expressions also float as soon as one
            // is introduced and Conversion only happens when required or is there an error thown
            // and Conversion must be explicit when trying to store a Float Expression into a byte?

        // TODO Parser
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

