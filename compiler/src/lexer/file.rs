// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::InnerToken;


// Lexer File Abstraction -----------------------------------------------------
#[derive(Clone)]
pub struct LexerFile {
    pub index: usize,
    pub path: PathBuf,
    pub contents: String,
    pub include_stack: Vec<InnerToken>
}

impl LexerFile {

    pub fn new(index: usize, contents: String, path: PathBuf, include_stack: Vec<InnerToken>) -> Self {
        Self {
            index,
            path,
            contents,
            include_stack
        }
    }

    pub fn get_index(&self, get_line: usize, get_col: usize) -> usize {
        let (mut line, mut col) = (0, 0);
        for (i, c) in self.contents.chars().enumerate() {
            if line == get_line && col == get_col {
                return i;

            } else if c == '\n' || c == '\r' {
                line += 1;
                col = 0;

            } else {
                col += 1;
            }
        }
        0
    }

    pub fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        let (mut line, mut col) = (0, 0);
        for (i, c) in self.contents.chars().enumerate() {
            if i == index {
                break;

            } else if c == '\n' || c == '\r' {
                line += 1;
                col = 0;

            } else {
                col += 1;
            }
        }
        (line, col)
    }

}

