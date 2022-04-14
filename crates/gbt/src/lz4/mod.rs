// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Modules --------------------------------------------------------------------
mod command;
use self::command::*;


// LZ4 alike compression ------------------------------------------------------
pub fn compress(data: &[u8], eom: bool) -> (Vec<u8>, f32) {
    let commands = Encoder::analyze(data, eom);
    let output = Encoder::serialize(&commands);
    let ratio = 1.0 / data.len() as f32 * output.len() as f32;
    (output, ratio)
}

pub fn analyze(data: &[u8]) -> (usize, usize) {
    let commands = Encoder::analyze(data, true);
    let output = Encoder::serialize(&commands);
    let ratio = 1.0 / data.len() as f32 * output.len() as f32;

    for c in &commands {
        c.info();
    }

    let mut stats: HashMap<&str, (usize, i32, usize, usize, i32, i32)> = HashMap::new();
    for c in &commands {
        let entry = stats.entry(c.name()).or_insert_with(|| (0, 0, 1000, 0, 1000, 0));
        entry.0 += 1;
        entry.1 += c.saved();
        entry.2 = entry.2.min(c.len());
        entry.3 = entry.3.max(c.len());
        entry.4 = entry.4.min(c.offset());
        entry.5 = entry.5.max(c.offset());
    }

    println!("=== Stats ===");
    println!("Bytes: {} / {}", output.len(), data.len());
    println!("Ratio: {:.2}\n", ratio);
    // println!("{:?}", data);
    // println!("{:?}", output);

    let mut stats: Vec<(&str, usize, i32, usize, usize, i32, i32)> = stats
        .into_iter()
        .map(|(s, (count, saved, min_len, max_len, min_offset, max_offset))| {
            (s, count, saved, min_len, max_len, min_offset, max_offset)

        }).collect();

    stats.sort_by(|a, b| b.2.cmp(&a.2));
    for (s, count, saved, min_len, max_len, min_offset, max_offset) in stats {
        println!("{: >18} {: >3}x = {: >4} byte(s) saved ({: >3}<>{: >3} length) ({: >3}<>{: >3} offset)", s, count, saved, min_len, max_len, min_offset, max_offset);
    }
    (output.len(), data.len())
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

        let mut commands: Vec<Box<dyn Command>> = Vec::new();
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
            _ => {
                None
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::compress;

    macro_rules! test_compress {
        ($input:expr, $expected:expr) => {
            let result = compress(&$input, false);
            assert_eq!(result.0, $expected);
        }
    }

    #[test]
    fn test_compress_single_literal() {
        test_compress!(vec![0], vec![
            16
        ]);
        test_compress!(vec![15], vec![
            31
        ]);
    }

    #[test]
    fn test_compress_literal() {
        test_compress!(vec![16], vec![
            0, 16
        ]);
        test_compress!(vec![1, 2, 3, 4], vec![
            3,
            1, 2, 3, 4
        ]);
        test_compress!(vec![
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32

        ], vec![
            15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
            15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 0, 32
        ]);
    }

    #[test]
    fn test_compress_double_byte() {
        test_compress!(vec![0, 0], vec![
            128
        ]);
        test_compress!(vec![63, 63], vec![
            191
        ]);
    }

    #[test]
    fn test_compress_repeat_zero() {
        test_compress!(vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], vec![
            47
        ]);
    }

    #[test]
    fn test_compress_repeat() {
        // Min Repeat
        test_compress!(vec![1, 1, 1], vec![
            193,
            1
        ]);
        test_compress!(vec![1, 1, 1, 1], vec![
            194,
            1
        ]);

        // Max Repeat
        test_compress!(vec![
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,

            1, 1, 1

        ], vec![
            255,
            1,
            1,
            17
        ]);
    }

    #[test]
    fn test_compress_repeat_dual() {
        // Min Repeat
        test_compress!(vec![1, 2, 1, 2], vec![
            224,
            1,
            2
        ]);

        // Max Repeat
        test_compress!(vec![
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
            255,
            1, 2
        ]);
    }

    #[test]
    fn test_compress_copy() {
        // Min Copy
        test_compress!(vec![
            0, 1, 2, 3, 1, 2
        ], vec![
            5, 0, 1, 2, 3, 1, 2
        ]);
        test_compress!(vec![
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
        test_compress!(vec![
            0, 1, 2, 3, 2, 1
        ], vec![
            5, 0, 1, 2, 3, 2, 1
        ]);
        test_compress!(vec![
            0, 1, 2, 87, 87, 2, 1

        ], vec![
            3, 0, 1, 2, 87, 96, 254
        ]);

        test_compress!(vec![
            0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2

        ], vec![
            2, 0, 1, 2, 64, 0, 67, 0, 73, 0
        ]);

        // TODO max copy

        // TODO copy offset

    }

}

