// STD Dependencies -----------------------------------------------------------
use std::env;
use std::fs::File;
use std::path::PathBuf;
use std::io::{Error as IOError, Read, Write};


// External Dependencies ------------------------------------------------------
use clap::{Arg, App};


// Internal Dependencies ------------------------------------------------------
use gbc_lib::compiler::Compiler;
use gbc_lib::traits::{FileError, FileReader, FileWriter};


// CLI Interface --------------------------------------------------------------
fn main() {

    let matches = App::new("gbc")
        .version("0.1")
        .author("Ivo Wetzel <ivo.wetzel@googlemail.com>")
        .about("GameBoy Compiler")
        .arg(Arg::with_name("SOURCE_FILE")
            .help("Sets the input source file to use")
            .required(true)
            .index(1)
        )
        .arg(Arg::with_name("ROM_FILE")
            .long("output-rom")
            .short("o")
            .value_name("FILE")
            .takes_value(true)
            .help("ROM file to generate")
        )
        .arg(Arg::with_name("MAP_FILE")
            .long("symbol-map")
            .short("m")
            .value_name("FILE")
            .takes_value(true)
            .help("Output symbol mapping for BGB debugger")
        )
        .arg(Arg::with_name("info")
            .long("info")
            .short("i")
            .help("Display ROM info")
        )
        .arg(Arg::with_name("segments")
            .long("segments")
            .short("S")
            .help("Display segments usage")
        )
        .arg(Arg::with_name("silent")
            .long("silent")
            .short("s")
            .help("Surpresses all output")
        )
        .arg(Arg::with_name("optimize")
            .long("optimize")
            .short("O")
            .help("Apply instruction optimizations")
        )
        .arg(Arg::with_name("debug")
            .long("debug")
            .short("D")
            .help("Enable debug instructions for BGB debugger")
        )
        .get_matches();

    if let Some(file) = matches.value_of("SOURCE_FILE") {

        // Create a project reader with the directory of the supplied argument file as the project
        // base so includes can be relative to the base directory by prefixing them with "/"
        let main = PathBuf::from(file);
        let mut reader = ProjectReader::new(main.clone());
        let main_file = PathBuf::from(main.file_name().unwrap());

        let mut compiler = Compiler::new();

        if matches.occurrences_of("segments") > 0 {
            compiler.set_print_segment_map();
        }

        if matches.occurrences_of("debug") == 0 {
            compiler.set_strip_debug_code();
        }

        if matches.occurrences_of("optimize") > 0 {
            compiler.set_optimize_instructions();
        }

        if matches.occurrences_of("info") > 0 {
            compiler.set_print_rom_info();
        }

        if let Some(rom) = matches.value_of("ROM_FILE") {
            compiler.set_generate_rom(PathBuf::from(rom));
        }

        if let Some(map) = matches.value_of("MAP_FILE") {
            compiler.set_generate_symbols(PathBuf::from(map));
        }

        match compiler.compile(&mut reader, main_file) {
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

    fn write_file_inner(&self, path: &PathBuf, data: String) -> Result<(), IOError> {
        let mut file = File::create(path)?;
        file.write_all(&data.into_bytes())?;
        Ok(())
    }

    fn write_binary_file_inner(&self, path: &PathBuf, data: Vec<u8>) -> Result<(), IOError> {
        let mut file = File::create(path)?;
        file.write_all(&data)?;
        Ok(())
    }

}

impl FileReader for ProjectReader {

    fn run_command(&self, name: String, args: Vec<String>, input: Vec<u8>) -> Result<Vec<u8>, String> {
        Err("Using commands is not yet fully implemented.".to_string())
    }

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

impl FileWriter for ProjectReader {
    fn write_file(&mut self, path: &PathBuf, data: String) -> Result<(), FileError> {
        self.write_file_inner(path, data).map_err(|io| {
            FileError {
                io,
                path: path.clone()
            }
        })
    }

    fn write_binary_file(&mut self, path: &PathBuf, data: Vec<u8>) -> Result<(), FileError> {
        self.write_binary_file_inner(path, data).map_err(|io| {
            FileError {
                io,
                path: path.clone()
            }
        })
    }
}

