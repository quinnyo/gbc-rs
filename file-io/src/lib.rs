// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::io::Error as IOError;


// External Dependencies ------------------------------------------------------
use colored::Colorize;


// Generic Traits -------------------------------------------------------------
#[derive(Debug)]
pub struct FileError {
    pub io: IOError,
    pub path: PathBuf
}

#[derive(Debug)]
pub struct CommandError {
    pub command: String,
    pub path: Option<PathBuf>,
    pub stdout: String
}

impl CommandError {
    pub fn to_string(&self) -> String {
        if let Some(path) = self.path.as_ref() {
            format!(
                "Failed to execute command \"{}\" on included file \"{}\":\n\n{}\n{}{}",
                self.command,
                path.display(),
                "---".red(),
                self.stdout,
                "---".red()
            )
        } else {
            format!(
                "Failed to execute command \"{}\":\n\n{}\n{}{}",
                self.command,
                "---".red(),
                self.stdout,
                "---".red()
            )
        }
    }
}

pub trait FileReader {

    fn run_command(&self, name: String, args: Vec<String>, input: &[u8]) -> Result<Vec<u8>, String>;

    fn read_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, String), FileError>;

    fn read_binary_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, Vec<u8>), FileError>;

    fn execute_raw_command(&self, path: Option<PathBuf>, command: &str, input: &[u8]) -> Result<Vec<u8>, String> {
        let mut args = command.split(' ');
        let name = args.next().expect("Failed to get command name");
        if name.is_empty() {
            Err("Missing command name".to_string())

        } else {
            let mut args: Vec<String> = args.map(|arg| arg.to_string()).collect();
            if let Some(path) = path {
                args.push(path.display().to_string());
            }
            self.run_command(name.to_string(), args, input)
        }
    }

    fn execute_command(&self, path: Option<PathBuf>, command: &str, input: String) -> Result<String, CommandError> {
        String::from_utf8(self.execute_raw_command(path.clone(), command, &input.into_bytes()).map_err(|stdout| {
            CommandError {
                command: command.to_string(),
                path: path.clone(),
                stdout
            }
        })?).map_err(|e| {
            CommandError {
                command: command.to_string(),
                path,
                stdout: format!("Command did not return a valid string: {}", e.to_string())
            }
        })
    }

    fn execute_binary_command(&self, path: Option<PathBuf>, command: &str, input: &[u8]) -> Result<Vec<u8>, CommandError> {
        self.execute_raw_command(path.clone(), command, input).map_err(|stdout| {
            CommandError {
                command: command.to_string(),
                path,
                stdout
            }
        })
    }

    fn resolve_path(base: &PathBuf, parent: Option<&PathBuf>, child: &PathBuf) -> PathBuf {
        let mut full_path = base.clone();
        if child.is_absolute() {
            full_path.push(child.strip_prefix("/").unwrap());

        } else {
            if let Some(parent) = parent {
                let mut p = parent.clone();
                p.set_file_name("");
                full_path.push(p.strip_prefix(base).unwrap());
            }
            full_path.push(child);
        }
        full_path
    }

}

pub trait FileWriter {
    fn write_file(&mut self, path: &PathBuf, data: String) -> Result<(), FileError>;
    fn write_binary_file(&mut self, path: &PathBuf, data: Vec<u8>) -> Result<(), FileError>;
}
