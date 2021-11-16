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
    lines: RefCell<Vec<(usize, usize)>>
}

impl LexerFile {

    pub fn new(index: usize, contents: String, path: PathBuf, include_stack: Vec<InnerToken>) -> Self {
        Self {
            index,
            path,
            contents,
            lines: RefCell::new(Vec::new()),
            include_stack
        }
    }

    pub fn get_index(&self, get_line: usize, get_col: usize) -> usize {
        self.cache_lines();
        println!("get index: {},{}", get_line, get_col);
        for (line, (start, _)) in self.lines.borrow().iter().enumerate() {
            if line == get_line {
                return start + get_col;
            }
        }
        //let (mut line, mut col) = (0, 0);
        //for (i, c) in self.contents.chars().enumerate() {
        //    if line == get_line && col == get_col {
        //        return i;

        //    } else if c == '\n' || c == '\r' {
        //        line += 1;
        //        col = 0;

        //    } else {
        //        col += 1;
        //    }
        //}
        0
    }

    pub fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        self.cache_lines();
        for (line, (start, end)) in self.lines.borrow().iter().enumerate() {
            if index >= *start && index < *end {
                return (line, index - start);
            }
        }
        println!("failed to lookup line/col for {}", index);
        (0, 0)
        //let (mut line, mut col) = (0, 0);
        //for (i, c) in self.contents.chars().enumerate() {
        //    if i == index {
        //        break;

        //    } else if c == '\n' || c == '\r' {
        //        line += 1;
        //        col = 0;

        //    } else {
        //        col += 1;
        //    }
        //}
        //(line, col)
    }

    fn cache_lines(&self) {
        if self.lines.borrow().is_empty() {
            println!("cache lines {}", self.path.display());
            let mut lines = Vec::new();
            let mut index = 0;
            for line in self.contents.split_inclusive('\n') {
                lines.push((index, index + line.len()));
                index += line.len();
            }
            *self.lines.borrow_mut() = lines;
        }
    }
}

