// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use crate::generator::Generator;
use crate::error::SourceError;
use crate::linker::Linker;
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler;

impl Compiler {
    pub fn compile<T: FileReader>(reader: T, entry: PathBuf) -> Result<(), CompilerError> {

        let include_lexer = Lexer::<IncludeStage>::from_file(&reader, &entry).map_err(|e| CompilerError::new("file inclusion", e))?;
        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| CompilerError::new("macro expansion", e))?;
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| CompilerError::new("value construction", e))?;
        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| CompilerError::new("expression construction", e))?;
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| CompilerError::new("entry construction", e))?;
        println!("{} token(s) after entry construction.", entry_lexer.len());

        let linker = Linker::from_lexer(entry_lexer, true, true).map_err(|e| CompilerError::new("section linking", e))?;
        // TODO report section usage and write out symbol map if required

        let _generator = Generator::from_linker(linker);
        // TODO show rom warnings and write to output file if required

        Ok(())
    }
}


// Compiler Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct CompilerError {
    stage: String,
    source: SourceError
}

impl CompilerError {
    fn new(stage: &str, source: SourceError) -> Self {
        Self {
            stage: stage.to_string(),
            source
        }

    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO add "During file inclusion phase" / "During macro expansion phase" to message
        write!(f, "CompilerError: {}", self.source)
    }
}

