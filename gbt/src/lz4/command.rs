// STD Dependencies -----------------------------------------------------------
use std::cmp;


// Constants ------------------------------------------------------------------
pub const MAX_LITERAL_LENGTH: u8 = 16;
pub const MAX_REPEAT_COUNT: i32 = 33;
pub const MAX_REPEAT_ZERO_COUNT: i32 = 16;
pub const MIN_REPEAT_LENGTH: i32 = 2;
pub const MAX_COPY_LENGTH: i32 = 34;
pub const MIN_COPY_LENGTH: i32 = 3;
pub const MAX_COPY_OFFSET: i32 = 254;

// 10ll_llll  DoubleByte
// 110l_llll  RepeatSingle
// 111l_llll  RepeatDual
// 010l_llll  Copy
// 011l_llll  Reverse Copy
// 0010_llll  RepeatZero
// 0011_uuuu  End of Stream
// 0001_0000  SingleLiteral
// 0000_llll  Literal
pub trait Command: std::fmt::Debug {
    fn serialize(&self) -> Vec<u8>;
    fn end(&self) -> usize;
    fn saved(&self) -> i32;
    fn name(&self) -> &str;
    fn len(&self) -> usize;
    fn offset(&self) -> i32;
    fn info(&self);
}

#[derive(Debug)]
struct Descriptor {
    length: u8,
    saved: i32,
    size: usize
}

impl Descriptor {
    fn new(length: u8, saved: i32, size: usize) -> Self {
        Self {
            length,
            saved,
            size
        }
    }
}


// Commands -------------------------------------------------------------------
#[derive(Debug)]
pub struct Literal {
    desc: Descriptor,
    data: Vec<u8>
}

impl Literal {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(length: u8, data: &[u8], index: usize) -> Box<dyn Command> {
        let data = data[index - length as usize..index].to_vec();
        Box::new(Self {
            desc: Descriptor::new(length, 0, 1 + data.len()),
            data
        })
    }
}

impl Command for Literal {
    fn serialize(&self) -> Vec<u8> {
        if self.data.len() == 1 && self.data[0] < 16 {
            // 0001_llll
            vec![0b0001_0000 | self.data[0]]

        } else {
            let mut bytes = Vec::new();
            // 0000_llll, 1-16 literals
            bytes.push((self.desc.length - 1) & 0x0F);
            bytes.extend(&self.data);
            bytes
        }
    }

    fn end(&self) -> usize {
        unimplemented!();
    }

    fn saved(&self) -> i32 {
        -1
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn offset(&self) -> i32 {
        0
    }

    fn name(&self) -> &str {
       "<Literal>"
    }

    fn info(&self) {
        println!("<Literal @{}  {:?}>", self.desc.length, self.data);
    }
}

#[derive(Debug)]
pub struct EndMarker;
impl Command for EndMarker {
    fn serialize(&self) -> Vec<u8> {
        vec![0x30]
    }

    fn end(&self) -> usize {
        unimplemented!();
    }

    fn saved(&self) -> i32 {
        -1
    }

    fn len(&self) -> usize {
        0
    }

    fn offset(&self) -> i32 {
        0
    }

    fn name(&self) -> &str {
        "<End>"
    }

    fn info(&self) {
        println!("<EndMarker>");
    }
}

#[derive(Debug)]
pub struct Copy {
    desc: Descriptor,
    offset: i32,
    reverse: bool,
    end: usize
}

impl Copy {

    #[allow(clippy::new_ret_no_self)]
    pub fn new(length: u8, saved: i32, offset: i32, reverse: bool, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 2),
            offset,
            reverse,
            end
        })
    }

    pub fn find(data: &[u8], index: i32) -> Option<Box<dyn Command>> {

        let copy = Copy::find_copy(data, index);
        if copy.length > MAX_COPY_LENGTH {
            return None;

        } else if copy.offset > MAX_COPY_OFFSET {
            return None;
        }

        let saved = copy.length - 2;
        if saved <= 0 {
            None

        } else {
            Some(Copy::new(
                copy.length as u8,
                saved,
                copy.offset,
                copy.reverse,
                (index + copy.length) as usize
            ))
        }

    }

    fn find_copy(data: &[u8], index: i32) -> CopyState {
        let forward = CopyState::find_copy(data, index, false);
        let reverse = CopyState::find_copy(data, index, true);

        if reverse.length > forward.length {
            reverse

        } else {
            forward
        }
    }

}

impl Command for Copy {
    fn serialize(&self) -> Vec<u8> {
        // 01R l_llll, offset
        // copy 3-34 bytes from offset 1-256 (+length)
        vec![
            0x40 | ((self.desc.length - MIN_COPY_LENGTH as u8) & 0x1F) | (if self.reverse { 0x20 } else { 0x00 }),
            self.offset as u8
        ]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

    fn len(&self) -> usize {
        self.desc.length as usize
    }

    fn offset(&self) -> i32 {
        self.offset
    }

    fn name(&self) -> &str {
        if self.reverse {
            "<CopyReverse>"

        } else {
            "<Copy>"
        }
    }

    fn info(&self) {
        //if self.reverse {
        //    println!("<Copy Reverse @{} from {}>", self.desc.length, self.offset);

        //} else {
        //    println!("<Copy @{} from {}>", self.desc.length, self.offset);
        //}
    }
}

#[derive(Debug)]
pub struct RepeatZero {
    desc: Descriptor,
    end: usize
}

impl RepeatZero {
    #[allow(clippy::new_ret_no_self)]
    fn new(length: u8, saved: i32, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 1),
            end
        })
    }
}

impl Command for RepeatZero {
    fn serialize(&self) -> Vec<u8> {
        // 0010_llll
        // repeat the zero byte 2-17 times
        vec![0x20 | (self.desc.length as u8 & 0x0F)]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

    fn len(&self) -> usize {
        self.desc.length as usize
    }

    fn offset(&self) -> i32 {
        0
    }

    fn name(&self) -> &str {
        "<RepeatZero>"
    }

    fn info(&self) {
        //println!("<RepeatZero @{}>", self.desc.length + 2);
    }
}

#[derive(Debug)]
struct RepeatSingle {
    desc: Descriptor,
    data: u8,
    end: usize
}

impl RepeatSingle {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(length: u8, saved: i32, data: u8, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 1 + 1),
            data,
            end
        })
    }
}

impl Command for RepeatSingle {
    fn serialize(&self) -> Vec<u8> {
        // 110 00000
        // repeat the next byte 2-33 times
        vec![0x80 | 0x40 | (self.desc.length as u8 & 0x1F), self.data]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn len(&self) -> usize {
        self.desc.length as usize
    }

    fn offset(&self) -> i32 {
        0
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

    fn name(&self) -> &str {
        "<RepeatSingle>"
    }

    fn info(&self) {
        //println!("<RepeatSingle @{}>", self.desc.length + 2);
    }
}

#[derive(Debug)]
pub struct RepeatDual {
    desc: Descriptor,
    data: u8,
    data_two: u8,
    end: usize
}

impl RepeatDual {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(length: u8, saved: i32, data: u8, data_two: u8, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 1 + 2),
            data,
            data_two,
            end
        })
    }
}

impl Command for RepeatDual {
    fn serialize(&self) -> Vec<u8> {
        // 111l_llll
        // repeat the next 2 bytes 2-33 times
        vec![0x80 | 0x40 | 0x20 | (self.desc.length as u8 & 0x1F), self.data, self.data_two]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

    fn len(&self) -> usize {
        self.desc.length as usize
    }

    fn offset(&self) -> i32 {
        0
    }

    fn name(&self) -> &str {
       "<RepeatDual>"
    }

    fn info(&self) {
        //println!("<RepeatDual @{}>", self.desc.length + 2);
    }
}


#[derive(Debug)]
struct DoubleByte {
    desc: Descriptor,
    data: u8,
    end: usize
}

impl DoubleByte {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(length: u8, saved: i32, data: u8, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 1),
            data,
            end
        })
    }
}

impl Command for DoubleByte {
    fn serialize(&self) -> Vec<u8> {
        // 10vv_vvvv
        // double a value between 0..=63
        vec![0x80 | (self.data as u8 & 0x3F)]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn len(&self) -> usize {
        self.desc.length as usize
    }

    fn offset(&self) -> i32 {
        0
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

    fn name(&self) -> &str {
        "<DoubleByte>"
    }

    fn info(&self) {
        //println!("<DoubleByte @{}>", self.desc.length + 2);
    }
}


// Intermediate Types ---------------------------------------------------------
pub struct Repeat;
impl Repeat {

    pub fn find(data: &[u8], index: i32) -> Option<Box<dyn Command>> {
        let single = Repeat::find_single(data, index);
        let dual = Repeat::find_dual(data, index);
        match (single, dual) {
            (Some(single), Some(dual)) => {
                if single.saved() >= dual.saved() {
                    Some(single)

                } else {
                    Some(dual)
                }
            },
            (Some(single), None) => Some(single),
            (None, Some(dual)) => Some(dual),
            _ => None
        }
    }

    fn find_single(data: &[u8], index: i32) -> Option<Box<dyn Command>> {
        let repeat = Repeat::find_single_repeat(data, index, index + MAX_REPEAT_COUNT);
        if let Some(mut repeat) = repeat {
            if repeat > MAX_REPEAT_COUNT {
                repeat = MAX_REPEAT_COUNT;
            }
            let saved = repeat - 2;
            let length = repeat - MIN_REPEAT_LENGTH;
            if saved <= 0 {
                if data[index as usize] < 64 {
                    Some(DoubleByte::new(1, 1, data[index as usize], (index + 2) as usize))

                } else {
                    None
                }

            // repeat the zero byte 2-17 times
            } else if data[index as usize] == 0 && length < MAX_REPEAT_ZERO_COUNT {
                Some(RepeatZero::new(length as u8, saved, (index + repeat) as usize))

            // repeat the next byte 2-65 times
            } else {
                Some(RepeatSingle::new(length as u8, saved, data[index as usize], (index + repeat) as usize))
            }

        } else {
            None
        }
    }

    fn find_dual(data: &[u8], index: i32) -> Option<Box<dyn Command>> {

        if let Some(mut repeat) = Repeat::find_multi_repeat(data, index, 2) {
            if repeat > MAX_REPEAT_COUNT {
                repeat = MAX_REPEAT_COUNT;
            }
            let saved = repeat * 2 - 3;
            let length = repeat - MIN_REPEAT_LENGTH;
            if saved < 0 {
                None

            // repeat the next 2 bytes 2-65 times
            } else {
                Some(RepeatDual::new(
                    length as u8,
                    saved,
                    data[index as usize],
                    data[index  as usize+ 1],
                    (index + repeat * 2) as usize
                ))
            }

        } else {
            None
        }

    }

    fn find_single_repeat(data: &[u8], from: i32, to: i32) -> Option<i32> {
        let s = data[from as usize];
        let mut repeat = 0;

        let to = cmp::min(to, data.len() as i32);
        for i in from..to {
            if data[i as usize] == s {
                repeat += 1;
                if repeat == MAX_REPEAT_COUNT {
                    break;
                }

            } else {
                break;
            }
        }

        if repeat < MIN_REPEAT_LENGTH {
            None

        } else {
            Some(repeat)
        }
    }

    fn find_multi_repeat(buf: &[u8], from: i32, length: i32) -> Option<i32> {
        let to = cmp::min(from + MAX_REPEAT_COUNT * length, buf.len() as i32);
        let from_length = cmp::min((from + length) as usize, buf.len());
        let initial = &buf[from as usize..from_length];

        let mut repeat = 0;
        let mut i = from;
        while i < to {
            let mut m = true;
            for b in 0..length {
                if (i + b) as usize >= buf.len() || initial[b as usize] != buf[(i + b) as usize] {
                    m = false;
                    break;
                }
            }

            if m {
                repeat += 1;
                if repeat == MAX_REPEAT_COUNT {
                    break;
                }

            } else {
                break;
            }
            i += length;
        }

        if repeat < MIN_REPEAT_LENGTH {
            None

        } else {
            Some(repeat)
        }
    }

}

struct CopyState {
    length: i32,
    offset: i32,
    reverse: bool
}

impl CopyState {
    fn find_copy(data: &[u8], from: i32, reverse: bool) -> CopyState {

        let mut state = CopyState {
            length: -1,
            offset: 0,
            reverse
        };

        let mut length = MIN_COPY_LENGTH;
        while length < cmp::max(cmp::min(MAX_COPY_LENGTH, data.len() as i32 - from + 1), 0) {
            let at = if reverse {
                CopyState::find_sub_copy_reverse(data, from, length)

            } else {
                CopyState::find_sub_copy(data, from, length)
            };

            if at != -1 {
                state.offset = at;
                state.length = length;

            } else {
                break;
            }

            length += 1;

        }

        state

    }

    fn find_sub_copy(data: &[u8], from: i32, length: i32) -> i32 {

        let mut at = from - length;
        while at >= cmp::max(from - MAX_COPY_OFFSET, 0) {

            // Check for maximum copy length
            let mut m = true;
            for i in 0..length {
                if data[(from + i) as usize] != data[(at + i) as usize] {
                    m = false;
                    break;
                }
            }

            if m {
                return from - at - length;
            }

            at -= 1;

        }

        -1

    }

    fn find_sub_copy_reverse(data: &[u8], from: i32, length: i32) -> i32 {
        let mut at = from - 1;
        while at >= cmp::max(from - MAX_COPY_OFFSET, 0) {

            // Check for maximum copy length
            let mut m = true;
            for i in 0..length {
                if at - i < 0 || data[(from + i) as usize] != data[(at - i) as usize] {
                    m = false;
                    break;
                }
            }

            if m {
                return from - at - length;
            }

            at -= 1;

        }
        -1
    }
}

