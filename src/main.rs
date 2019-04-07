// STD Dependencies -----------------------------------------------------------
use std::env;
use std::fs::File;
use std::path::PathBuf;
use std::io::{Error as IOError, Read};


// Internal Dependencies ------------------------------------------------------
use gbasm_lib::compiler::Compiler;
use gbasm_lib::traits::{FileError, FileReader};


// CLI Interface --------------------------------------------------------------
fn main() {
    // TODO clap usage if no file is specified (should be automatic if file is a required argument)
    for file in env::args().skip(1) {

        // Create a project reader with the directory of the supplied argument file as the project
        // base so includes can be relative to the base directory by prefixing them with "/"
        let main = PathBuf::from(file);
        let reader = ProjectReader::new(main.clone());
        let main_file = PathBuf::from(main.file_name().unwrap());

        let mut compiler = Compiler::new();
        compiler.set_print_segment_map();
        match compiler.compile(reader, main_file) {
            Ok(output) => println!("{}", output),
            Err((output, err)) => {
                println!("{}", output);
                eprintln!("{}", err);
            }
        }

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

    fn read_file_inner(&self, full_path: &PathBuf) -> Result<(PathBuf, String), IOError> {
        let mut file = File::open(full_path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok((full_path.clone(), contents))
    }

    fn read_binary_file_inner(&self, full_path: &PathBuf) -> Result<(PathBuf, Vec<u8>), IOError> {
        let mut file = File::open(full_path)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;
        Ok((full_path.clone(), contents))
    }

}

impl FileReader for ProjectReader {
    fn read_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, String), FileError> {
        let path = Self::resolve_path(&self.base, parent, child);
        self.read_file_inner(&path).map_err(|io| {
            FileError {
                io,
                path
            }
        })
    }

    fn read_binary_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, Vec<u8>), FileError> {
        let path = Self::resolve_path(&self.base, parent, child);
        self.read_binary_file_inner(&path).map_err(|io| {
            FileError {
                io,
                path
            }
        })
    }
}

