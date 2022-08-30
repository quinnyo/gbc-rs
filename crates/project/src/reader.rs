// STD Dependencies -----------------------------------------------------------
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{Error as IOError, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

// External Dependencies ------------------------------------------------------
use file_io::{FileError, FileReader, FileWriter};

// Concrete File IO Implementation --------------------------------------------
#[derive(Debug)]
pub struct ProjectReader {
    base: PathBuf,
    overlay: RefCell<HashMap<PathBuf, String>>,
}

impl ProjectReader {
    pub fn from_relative(mut main: PathBuf) -> Self {
        let mut base = env::current_dir().unwrap();
        main.set_file_name("");
        base.push(main);
        Self {
            base,
            overlay: RefCell::new(HashMap::new()),
        }
    }

    pub fn from_absolute(base: PathBuf) -> Self {
        Self {
            base,
            overlay: RefCell::new(HashMap::new()),
        }
    }

    pub fn overlay_file(&self, path: PathBuf, text: String) {
        let mut overlays = self.overlay.borrow_mut();
        overlays.insert(path, text);
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

    fn write_file_inner(&self, path: &Path, data: String) -> Result<(), IOError> {
        let mut file = File::create(path)?;
        file.write_all(&data.into_bytes())?;
        Ok(())
    }

    fn write_binary_file_inner(&self, path: &Path, data: Vec<u8>) -> Result<(), IOError> {
        let mut file = File::create(path)?;
        file.write_all(&data)?;
        Ok(())
    }
}

impl FileReader for ProjectReader {
    fn base_dir(&self) -> &Path {
        &self.base
    }

    fn run_command(
        &self,
        name: String,
        args: Vec<String>,
        input: &[u8],
    ) -> Result<Vec<u8>, String> {
        let mut child = Command::new(name)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to execute process: {}", e))?;

        if let Some(stdin) = child.stdin.as_mut() {
            // We ignore any potential error here in case the child
            // is not waiting for STDIN
            stdin.write_all(input).ok();
        }

        let output = child
            .wait_with_output()
            .map_err(|e| format!("Failed to execute process (STDOUT): {}", e))?;

        if output.status.success() {
            Ok(output.stdout)
        } else {
            Err(String::from_utf8_lossy(&output.stderr).to_string())
        }
    }

    fn read_file(
        &self,
        parent: Option<&PathBuf>,
        child: &Path,
    ) -> Result<(PathBuf, String), FileError> {
        let path = Self::resolve_path(&self.base, parent, child);
        let overlays = self.overlay.borrow();
        if let Some(overlay) = overlays.get(&path) {
            Ok((path, overlay.clone()))
        } else {
            self.read_file_inner(&path)
                .map_err(|io| FileError { io, path })
        }
    }

    fn read_binary_file(
        &self,
        parent: Option<&PathBuf>,
        child: &Path,
    ) -> Result<(PathBuf, Vec<u8>), FileError> {
        let path = Self::resolve_path(&self.base, parent, child);
        self.read_binary_file_inner(&path)
            .map_err(|io| FileError { io, path })
    }
}

impl FileWriter for ProjectReader {
    fn write_file(&mut self, path: &Path, data: String) -> Result<(), FileError> {
        self.write_file_inner(path, data).map_err(|io| FileError {
            io,
            path: path.to_path_buf(),
        })
    }

    fn write_binary_file(&mut self, path: &Path, data: Vec<u8>) -> Result<(), FileError> {
        self.write_binary_file_inner(path, data)
            .map_err(|io| FileError {
                io,
                path: path.to_path_buf(),
            })
    }
}
