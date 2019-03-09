// STD Dependencies -----------------------------------------------------------
use std::env;
use std::fs::File;
use std::path::PathBuf;
use std::io::{Error as IOError, Read};


// Modules --------------------------------------------------------------------
mod lexer;
mod traits;


// Internal Dependencies ------------------------------------------------------
use lexer::Lexer;
use traits::FileReader;


// CLI Interface --------------------------------------------------------------
fn main() {
    // TODO minimal clap UI to lex a file and it's includes
    // 1. Generate Low Level Lexer Output
    // 2. Include Directives, Strings, Template Strings, Macro Calls, Macros Defs, Macro Parameters
    // 3. Resolve all Includes
    // 4. Expand all Macros calls and simply dump the tokens inline
    for file in env::args().skip(1) {

        // Create a project reader with the directory of the supplied argument file as the project
        // base so includes can be relative to the base directory by prefixing them with "/"
        let main = PathBuf::from(file);
        let reader = ProjectReader::new(main.clone());
        let main_file = PathBuf::from(main.file_name().unwrap());

        // Create a new lexer
        let mut lexer = Lexer::new();
        let count = lexer.lex_file(&reader, &main_file).expect("Lexer failed");
        println!("Parsed {} token(s)", count);

    }
}

// Helpers --------------------------------------------------------------------
struct ProjectReader {
    base: PathBuf
}

impl ProjectReader {
    fn new(mut main: PathBuf) -> Self {
        let mut base = env::current_dir().unwrap();
        main.set_file_name("");
        base.push(main);
        Self {
            base
        }
    }
}

impl FileReader for ProjectReader {
    fn read_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, String), IOError> {
        let full_path = Self::resolve_path(&self.base, parent, child);
        let mut file = File::open(&full_path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok((full_path, contents))
    }
}

