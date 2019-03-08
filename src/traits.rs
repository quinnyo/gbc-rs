// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::io::Error as IOError;


// Generic Traits -------------------------------------------------------------
pub trait FileReader {
    fn read_file(&self, parent: Option<&PathBuf>, child: &PathBuf) -> Result<(PathBuf, String), IOError>;
    fn resolve_path(base: &PathBuf, parent: Option<&PathBuf>, child: &PathBuf) -> PathBuf {
        let mut full_path = base.clone();
        if child.is_absolute() {
            full_path.push(child.strip_prefix("/").unwrap());

        } else {
            if let Some(parent) = parent {
                let mut p = parent.clone();
                p.set_file_name("");
                full_path.push(p);
            }
            full_path.push(child);
        }
        full_path
    }
}

