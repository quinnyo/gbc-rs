// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Huffman compression --------------------------------------------------------
pub fn compress(data: &[u8]) -> (Vec<u8>, f32) {
    let (tree, tree_header) = Tree::from_symbols(data);
    let mut bits = Encoder::from_tree(tree, tree_header);
    bits.append_slice(data);
    let encoded = bits.finalize();
    let ratio = 1.0 / data.len() as f32 * encoded.len() as f32;
    (encoded, ratio)
}

#[derive(Debug)]
struct BitStream {
    bits: Vec<u8>,
    data: Vec<u8>
}

impl BitStream {
    fn new() -> Self {
        Self {
            bits: Vec::new(),
            data: Vec::new()
        }
    }

    fn push_byte(&mut self, byte: u8) {
        for i in 0..8 {
            self.bits.push((byte >> i) & 1);
        }
        if self.bits.len() >= 8 {
            self.emit();
        }
    }

    fn push_bits(&mut self, bits: &[u8]) {
        self.bits.extend_from_slice(&bits);
        if self.bits.len() >= 8 {
            self.emit();
        }
    }

    fn emit(&mut self) {
        let (encoded, remaining) = self.bits.split_at(8.min(self.bits.len()));
        let mut byte = 0;
        if !encoded.is_empty() {
            for (i, e) in encoded.into_iter().enumerate() {
                byte += e << i;
            }
            self.data.push(byte);
            self.bits = remaining.to_vec();
        }
    }

    fn finalize(mut self) -> Vec<u8> {
        self.emit();
        self.data
    }
}

#[derive(Debug)]
struct Encoder {
    data: Vec<u8>,
    length: usize,
    lookup: HashMap<u8, Vec<u8>>,
    stream: BitStream
}

impl Encoder {
    fn from_tree(tree: Tree, tree_header: Vec<u8>) -> Self {
        let mut lookup = HashMap::new();
        tree.walk(|symbol, encoding| {
            if let Some(symbol) = symbol {
                lookup.insert(symbol, encoding);
            }
        });
        let l = tree_header.len() as u16;
        let mut data = vec![(l >> 8) as u8, l as u8];
        data.extend_from_slice(&tree_header);
        Self {
            data,
            lookup,
            length: 0,
            stream: BitStream::new()
        }
    }

    fn append_slice(&mut self, symbols: &[u8]) {
        for s in symbols {
            self.push_symbol(*s);
        }
    }

    fn push_symbol(&mut self, s: u8) {
        let e = &self.lookup[&s];
        self.stream.push_bits(e);
        self.length += 1;
    }

    fn finalize(mut self) -> Vec<u8> {
        let data = self.stream.finalize();
        self.data.extend_from_slice(&[(self.length >> 8) as u8, self.length as u8]);
        self.data.extend_from_slice(&data);
        self.data
    }
}

#[derive(Debug)]
enum Tree {
    Branch {
        f: u32,
        left: Box<Self>,
        right: Box<Self>
    },
    Leaf {
        symbol: u8,
        f: u32
    }
}

impl Tree {
    fn from_symbols(symbols: &[u8]) -> (Self, Vec<u8>) {
        let mut distribution = HashMap::new();
        for s in symbols {
            *distribution.entry(*s).or_insert_with(|| 0) += 1;
        }
        let mut trees = Vec::new();
        for (symbol, f) in distribution {
            trees.push(Self::Leaf {
                symbol,
                f
            });
        }
        while trees.len() >= 2 {
            trees.sort_by(|a, b| b.f().cmp(&a.f()));
            let left = trees.pop().unwrap();
            let right = trees.pop().unwrap();
            if left.is_leaf() {
                trees.push(Tree::Branch {
                    f: left.f() + right.f(),
                    left: Box::new(left),
                    right: Box::new(right)
                });

            } else {
                trees.push(Tree::Branch {
                    f: left.f() + right.f(),
                    left: Box::new(right),
                    right: Box::new(left)
                });
            }
        }

        let tree = trees.remove(0);

        let mut header = BitStream::new();
        tree.walk(|symbol, _| {
            if let Some(symbol) = symbol {
                header.push_bits(&[0]);
                header.push_byte(symbol);

            } else {
                header.push_bits(&[1]);
            }
        });
        (tree, header.finalize())
    }

    fn walk<C: FnMut(Option<u8>, Vec<u8>)>(&self, mut callback: C) {
        self.walk_encoding(&mut callback, Vec::new());
    }

    fn walk_encoding<C: FnMut(Option<u8>, Vec<u8>)>(&self, callback: &mut C, prefix: Vec<u8>) {
        match self {
            Self::Branch { left, right, .. } => {
                callback(None, prefix.clone());

                let mut l = prefix.clone();
                l.push(0);
                left.walk_encoding(callback, l);

                let mut r = prefix.clone();
                r.push(1);
                right.walk_encoding(callback, r);
            },
            Self::Leaf { symbol, .. } => {
                callback(Some(*symbol), prefix);
            }
        }
    }

    fn f(&self) -> u32 {
        match self {
            Self::Branch { f, .. } | Self::Leaf { f, .. } => *f
        }
    }

    fn is_leaf(&self) -> bool {
        match self {
            Self::Leaf { .. } => true,
            _ => false
        }
    }

}

fn decompress(data: &[u8]) -> Vec<u8> {
    let tree_len = ((data[0] as usize) << 8) | data[1] as usize + 2;
    let mut tree_index = 2;
    let mut payload_index = tree_len + 2;
    let final_size = ((data[tree_len] as usize) << 8) | data[tree_len + 1] as usize;

    let mut symbols = Vec::new();
    let mut tree_byte = data[tree_index];
    let mut tree_bit = tree_byte & 1;
    let mut tree_bits_left = 8;
    'outer: loop {
        let mut payload_byte = data[payload_index];

        // Go through all bits in the payload
        for _ in 0..8 {
            let payload_bit = payload_byte & 1;
            payload_byte = payload_byte >> 1;

            // Walk tree left and skip node type
            if payload_bit == 0 {
                tree_bit = next_tree_bit(&mut tree_byte, &mut tree_bits_left, &mut tree_index, data);

            // Walk tree right and skip node types and symbol payload
            } else if payload_bit == 1 {
                for _ in 0..10 {
                    tree_bit = next_tree_bit(&mut tree_byte, &mut tree_bits_left, &mut tree_index, data);
                }
            }

            // Check if we have arrived at a tree node
            if tree_bit == 0 {
                let mut symbol = 0;
                for i in 0..8 {
                    tree_bit = next_tree_bit(&mut tree_byte, &mut tree_bits_left, &mut tree_index, data);
                    symbol |= tree_bit << i;
                }
                symbols.push(symbol);

                // Reset tree position
                tree_index = 2;
                tree_byte = data[tree_index];
                tree_bit = tree_byte & 1;
                tree_bits_left = 8;

                // Stop once the full payload has been decoded
                if symbols.len() == final_size {
                    break 'outer;
                }
            }
        }
        payload_index += 1;
    }
    symbols
}

fn next_tree_bit(tree_byte: &mut u8, tree_bits_left: &mut usize, tree_index: &mut usize, data: &[u8]) -> u8 {
    *tree_byte = *tree_byte >> 1;
    *tree_bits_left -= 1;
    if *tree_bits_left == 0 {
        *tree_index += 1;
        *tree_byte = data[*tree_index];
        *tree_bits_left = 8;
    }
    *tree_byte & 1
}

#[cfg(test)]
mod test {

    use super::{compress, decompress};

    fn compress_inline(input: &[u8], expected: &[u8]) {
        let result = compress(input);
        assert_eq!(result.0, expected);
    }

    fn decompress_inline(input: &[u8], expected: &[u8]) {
        let result = decompress(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_encode_decode() {
        let uncompressed = vec![
             4, 4, 4, 4, 2, 2, 2, 1, 4, 1, 5
        ];
        let compressed = vec![
            0, 5, 17, 36, 80, 129, 0, 0, 11, 80, 221, 7
        ];
        compress_inline(&uncompressed, &compressed);
        decompress_inline(&compressed, &uncompressed);
    }
}

