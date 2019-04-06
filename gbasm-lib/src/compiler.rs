// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;

// Internal Dependencies ------------------------------------------------------
use crate::generator::Generator;
use crate::linker::Linker;
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler;

impl Compiler {
    pub fn compile<T: FileReader>(reader: T, entry: PathBuf) -> Result<(), Box<dyn Error>> {

        let include_lexer = Lexer::<IncludeStage>::from_file(&reader, &entry).map_err(|e| CompilerError::new("INCLUDE", e))?;
        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| CompilerError::new("MACRO EXPANSION", e))?;
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| CompilerError::new("VALUE CONVERSION", e))?;
        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| CompilerError::new("EXPRESSION CONSTRUCTION", e))?;
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| CompilerError::new("ENTRY CONSTRUCTION", e))?;
        println!("{} token(s) after entry construction.", entry_lexer.len());

        // TODO report sections and their entry counts
        let linker = Linker::from_lexer(entry_lexer, true, true).map_err(|e| CompilerError::new("LINKER", e))?;

        let _generator = Generator::from_linker(linker);

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

