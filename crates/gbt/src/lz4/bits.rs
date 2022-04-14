#[derive(Debug)]
pub struct BitStream {
    bits: Vec<u8>,
    data: Vec<u8>
}

impl BitStream {
    pub fn new() -> Self {
        Self {
            bits: Vec::new(),
            data: Vec::new()
        }
    }

    pub fn push_byte(&mut self, byte: u8, bits: usize) {
        for i in 0..bits {
            self.bits.push((byte >> (bits - 1 - i)) & 1);
        }
        if self.bits.len() >= 8 {
            self.emit();
        }
    }

    // pub fn push_bits(&mut self, bits: &[u8]) {
    //     self.bits.extend_from_slice(&bits);
    //     if self.bits.len() >= 8 {
    //         self.emit();
    //     }
    // }

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

    pub fn finalize(mut self) -> Vec<u8> {
        self.emit();
        self.data
    }
}


#[cfg(test)]
mod test {

    use super::BitStream;

    #[test]
    fn test_stream() {
        // The encoder reverses the bit order so decoding is simpler / faster
        let mut b = BitStream::new();

        b.push_byte(3, 3);
        b.push_byte(7, 4);
        // =1111_0110

        b.push_byte(5, 3);
        b.push_byte(33, 6);
        // =1000_0110

        assert_eq!(b.finalize(), vec![0b1111_0110, 0b1000_0110]);
    }
}

