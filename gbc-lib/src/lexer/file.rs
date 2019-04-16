// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::InnerToken;


// Lexer File Abstraction -----------------------------------------------------
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

