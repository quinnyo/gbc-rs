// STD Dependencies -----------------------------------------------------------
use std::cmp;


// Constants ------------------------------------------------------------------
const MAX_LITERAL_LENGTH: u8 = 32;
const MAX_REPEAT_COUNT: i32 = 65;
const MIN_REPEAT_LENGTH: i32 = 2;
const MAX_COPY_LENGTH: i32 = 34;
const MIN_COPY_LENGTH: i32 = 3;
const MAX_COPY_OFFSET: i32 = 254;

// Commands -------------------------------------------------------------------
// 0010 RepeatZero
// 0011 End of Stream
// 0000 Literal
// 0001 Unused TODO Repeat short? 0001_llll bbbb_bbbb
// 010  Copy
// 011  Reverse Copy
// 001  RepeatSingle
// 101  RepeatDual
//
// 0:
//     0:
//          1:
//              0: RepeatZero
//              1: End of Stream
//
//          0:
//              0: Literal
//              1: Unused
//
//     1:
//          0: Copy
//          1: Reverse Copy
// 1:
//     0: RepeatSingle
//     1: RepeatDual
trait Command: std::fmt::Debug {
    fn serialize(&self) -> Vec<u8>;
    fn end(&self) -> usize {
        unimplemented!();
    }
    fn saved(&self) -> i32 {
        unimplemented!();
    }
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

#[derive(Debug)]
struct Literal {
    desc: Descriptor,
    data: Vec<u8>
}

impl Literal {
    #[allow(clippy::new_ret_no_self)]
    fn new(length: u8, data: &[u8], index: usize) -> Box<dyn Command> {
        let data = data[index - length as usize..index].to_vec();
        Box::new(Self {
            desc: Descriptor::new(length, 0, 1 + data.len()),
            data
        })
    }
}

impl Command for Literal {
    fn serialize(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        // 00 0l_llll, 1-64 literals
        bytes.push((self.desc.length - 1) & 0x1F);
        bytes.extend(&self.data);
        bytes
    }
}

#[derive(Debug)]
struct EndMarker;
impl Command for EndMarker {
    fn serialize(&self) -> Vec<u8> {
        vec![0x30]
    }
}

#[derive(Debug)]
struct Copy {
    desc: Descriptor,
    offset: u8,
    reverse: bool,
    end: usize
}

impl Copy {

    #[allow(clippy::new_ret_no_self)]
    fn new(length: u8, saved: i32, offset: u8, reverse: bool, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 2),
            offset,
            reverse,
            end
        })
    }

    fn find(data: &[u8], index: i32) -> Option<Box<dyn Command>> {

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
                copy.offset as u8,
                copy.reverse,
                (index + copy.length) as usize
            ))
        }

    }

    fn find_copy(data: &[u8], index: i32) -> CopyState {
        let forward = Encoder::find_copy(data, index, false);
        let reverse = Encoder::find_copy(data, index, true);

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
            self.offset
        ]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

}

struct Repeat;
impl Repeat {

    fn find(data: &[u8], index: i32) -> Option<Box<dyn Command>> {
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

        let repeat = Encoder::find_single_repeat(data, index, index + MAX_REPEAT_COUNT);
        if let Some(repeat) = repeat {
            let saved = repeat - 2;
            let length = repeat - MIN_REPEAT_LENGTH;

            if repeat > MAX_REPEAT_COUNT {
                None

            } else if saved <= 0 {
                None

            // repeat the zero byte 2-17 times
            } else if data[index as usize] == 0 && length < 16 {
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

        if let Some(repeat) = Encoder::find_multi_repeat(data, index, 2) {
            let saved = repeat * 2 - 3;
            let length = repeat - MIN_REPEAT_LENGTH;

            if repeat > MAX_REPEAT_COUNT {
                None

            // repeat the next 2 bytes 2-65 times
            } else if saved < 0 {
                None

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

}

#[derive(Debug)]
struct RepeatZero {
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

}

#[derive(Debug)]
struct RepeatSingle {
    desc: Descriptor,
    data: u8,
    end: usize
}

impl RepeatSingle {
    #[allow(clippy::new_ret_no_self)]
    fn new(length: u8, saved: i32, data: u8, end: usize) -> Box<dyn Command> {
        Box::new(Self {
            desc: Descriptor::new(length, saved, 1 + 1),
            data,
            end
        })
    }
}

impl Command for RepeatSingle {

    fn serialize(&self) -> Vec<u8> {
        // 100 00000
        // repeat the next byte 2-65 times
        vec![0x80 | (self.desc.length as u8& 0x3F), self.data]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

}

#[derive(Debug)]
struct RepeatDual {
    desc: Descriptor,
    data: u8,
    data_two: u8,
    end: usize
}

impl RepeatDual {
    #[allow(clippy::new_ret_no_self)]
    fn new(length: u8, saved: i32, data: u8, data_two: u8, end: usize) -> Box<dyn Command> {
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
        // 110 00000
        // repeat the next 2 bytes 2-65 times
        vec![0xC0 | (self.desc.length as u8 & 0x3F), self.data, self.data_two]
    }

    fn end(&self) -> usize {
        self.end
    }

    fn saved(&self) -> i32 {
        self.desc.saved
    }

}



// Intermediate Types ---------------------------------------------------------
struct CopyState {
    length: i32,
    offset: i32,
    reverse: bool
}


// LZ4 alike dompression ------------------------------------------------------
pub fn compress(data: &[u8], eom: bool) -> (Vec<u8>, f32) {
    let commands = Encoder::analyze(data, eom);
    let output = Encoder::serialize(&commands);
    let ratio = 1.0 / data.len() as f32 * output.len() as f32;
    (output, ratio)
}

pub struct Encoder;
impl Encoder {

    fn serialize(commands: &[Box<dyn Command>]) -> Vec<u8> {
        let mut output = Vec::new();
        for c in commands {
            let mut bytes = c.serialize();
            output.append(&mut bytes);
        }
        output
    }

    fn analyze(data: &[u8], eom: bool) -> Vec<Box<dyn Command>> {

        let mut index = 0;
        let mut literal_count = 0;

        let mut commands: Vec<Box<Command>> = Vec::new();
        while index < data.len() {

            let command = Encoder::command(data, index);
            if command.is_some() && literal_count > 0 || literal_count == MAX_LITERAL_LENGTH {
                commands.push(Literal::new(literal_count, data, index));
                literal_count = 0;
            }

            if let Some(command) = command {
                index = command.end();
                commands.push(command);

            } else {
                literal_count += 1;
                index += 1;
            }

        }

        if literal_count > 0 {
            commands.push(Literal::new(literal_count, data, index));
        }

        if eom {
            commands.push(Box::new(EndMarker));
        }

        commands

    }

    fn command(data: &[u8], index: usize) -> Option<Box<dyn Command>> {

        let repeat = Repeat::find(data, index as i32);
        let copy = Copy::find(data, index as i32);

        match (repeat, copy) {
            (Some(repeat), Some(copy)) => {
                if repeat.saved() >= copy.saved() {
                    Some(repeat)

                } else {
                    Some(copy)
                }
            },
            (Some(repeat), None) => Some(repeat),
            (None, Some(copy)) => Some(copy),
            _ => None
        }

    }

    // Repeat Detection
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

    // Copy Detection
    fn find_copy(data: &[u8], from: i32, reverse: bool) -> CopyState {

        let mut state = CopyState {
            length: -1,
            offset: 0,
            reverse
        };

        let mut length = MIN_COPY_LENGTH;
        while length < cmp::max(cmp::min(MAX_COPY_LENGTH, data.len() as i32 - from + 1), 0) {
            let at = if reverse {
                Encoder::find_sub_copy_reverse(data, from, length)

            } else {
                Encoder::find_sub_copy(data, from, length)
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

#[cfg(test)]
mod test {

    use super::compress;

    fn compress_inline(input: Vec<u8>, expected: Vec<u8>) {
        let result = compress(&input, false);
        assert_eq!(result.0, expected);
    }

    #[test]
    fn test_compress_literal() {
        // Literal
        compress_inline(vec![1], vec![
            0, 1
        ]);
        compress_inline(vec![1, 1], vec![
            1, 1, 1
        ]);
        compress_inline(vec![1, 2, 3, 4], vec![
            3,
            1, 2, 3, 4
        ]);
        // Max Literal
        compress_inline(vec![
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32

        ], vec![
            31, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
            0, 32
        ]);
    }

    #[test]
    fn test_compress_repeat_zero() {
        compress_inline(vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], vec![
            47
        ]);
    }

    #[test]
    fn test_compress_repeat() {
        // Min Repeat
        compress_inline(vec![1, 1, 1], vec![
            129,
            1
        ]);
        compress_inline(vec![1, 1, 1, 1], vec![
            130,
            1
        ]);

        // Max Repeat
        compress_inline(vec![
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1

        ], vec![
            191,
            1,
            0,
            1
        ]);
    }

    #[test]
    fn test_compress_repeat_dual() {
        // Min Repeat
        compress_inline(vec![1, 2, 1, 2], vec![
            192,
            1,
            2
        ]);

        // Max Repeat
        compress_inline(vec![
            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 1, 2,

            1, 2, 1, 2

        ], vec![
            255,
            1, 2,
            1,
            1, 2
        ]);
    }

    #[test]
    fn test_compress_copy() {
        // Min Copy
        compress_inline(vec![
            0, 1, 2, 3, 1, 2
        ], vec![
            5, 0, 1, 2, 3, 1, 2
        ]);
        compress_inline(vec![
            0, 1, 2, 3, 1, 2, 3

        ], vec![
            3, 0, 1, 2, 3, 64, 0
        ]);

        // TODO max copy

        // TODO copy offset

    }

    #[test]
    fn test_compress_reverse_copy() {
        // Min Copy
        compress_inline(vec![
            0, 1, 2, 3, 2, 1
        ], vec![
            5, 0, 1, 2, 3, 2, 1
        ]);
        compress_inline(vec![
            0, 1, 2, 3, 3, 2, 1

        ], vec![
            3, 0, 1, 2, 3, 96, 254
        ]);

        compress_inline(vec![
            0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2

        ], vec![
            2, 0, 1, 2, 64, 0, 67, 0, 73, 0
        ]);

        // TODO max copy

        // TODO copy offset

    }

}

