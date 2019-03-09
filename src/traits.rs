// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;
use std::io::Error as IOError;


// Generic Traits -------------------------------------------------------------
#[derive(Debug)]
pub struct FileError {
    pub io: IOError,
    pub path: PathBuf
}

impl fmt::Display for FileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FileError: {}", self.path.display())
    }
}

impl Error for FileError {}

pub trait FileReader {
    fn read_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, String), FileError>;
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

