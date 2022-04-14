// STD Dependencies -----------------------------------------------------------
use std::cell::RefCell;
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::InnerToken;


// Lexer File Abstraction -----------------------------------------------------
#[derive(Clone)]
pub struct LexerFile {
    pub index: usize,
    pub path: PathBuf,
    pub contents: String,
    pub include_stack: Vec<InnerToken>,
    lines: RefCell<Vec<(usize, usize)>>,
    last: RefCell<(usize, usize)>
}

impl LexerFile {
    pub fn new(index: usize, contents: String, path: PathBuf, include_stack: Vec<InnerToken>) -> Self {
        Self {
            index,
            path,
            contents,
            include_stack,
            lines: RefCell::new(Vec::new()),
            last: RefCell::new((0, 0))
        }
    }

    pub fn get_index(&self, get_line: usize, get_col: usize) -> usize {
        self.cache_lines();
        for (line, (start, _)) in self.lines.borrow().iter().enumerate() {
            if line == get_line {
                return start + get_col;
            }
        }
        // Fallback to last line
        self.last.borrow().0
    }

    pub fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        self.cache_lines();
        for (line, (start, end)) in self.lines.borrow().iter().enumerate() {
            if index >= *start && index < *end {
                return (line, index - start);
            }
        }
        // Fallback to last column on last line
        *self.last.borrow()
    }

    fn cache_lines(&self) {
        if self.lines.borrow().is_empty() {
            let mut lines = Vec::new();
            let mut index = 0;
            for line in self.contents.split_inclusive('\n') {
                lines.push((index, index + line.len()));
                index += line.len();
            }
            *self.last.borrow_mut() = (lines.len().saturating_sub(1), index);
            *self.lines.borrow_mut() = lines;
        }
    }
}

