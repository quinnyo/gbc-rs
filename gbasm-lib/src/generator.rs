// STD Dependencies ----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


// Internal Dependencies ------------------------------------------------------
use crate::linker::Linker;


// Statics --------------------------------------------------------------------
static NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
];

lazy_static! {
    static ref ROM_KB_SIZES: HashMap<u8, [u16; 2]> = {
        let mut sizes = HashMap::new();
        //                    KB    Banks
        sizes.insert(0x00, [  32,   0]);
        sizes.insert(0x01, [  64,   4]);
        sizes.insert(0x02, [ 128,   8]);
        sizes.insert(0x03, [ 256,  16]);
        sizes.insert(0x04, [ 512,  32]);
        sizes.insert(0x05, [1024,  64]); // Only 63 banks used by MBC1
        sizes.insert(0x06, [2048, 128]); // Only 125 banks used by MBC1
        sizes.insert(0x07, [4096, 256]);
        sizes.insert(0x52, [1152,  72]);
        sizes.insert(0x53, [1280,  80]);
        sizes.insert(0x54, [1536,  96]);
        sizes
    };

    static ref RAM_KB_SIZES: HashMap<u8, [u8; 2]> = {
        let mut sizes = HashMap::new();
        //                   KB   Banks
        sizes.insert(0x00, [  0,  0]); // None (must always be set with MBC2 even though it has 512x4 bits RAM)
        sizes.insert(0x01, [  2,  1]); // 1 Bank (only one quarter is used)
        sizes.insert(0x02, [  8,  1]); // 1 Bank (Full)
        sizes.insert(0x03, [ 32,  4]); // 4 Banks
        sizes.insert(0x04, [128, 16]); // 16 Banks
        sizes
    };

    static ref CART_TYPES: HashMap<u8, CartType> = {
        let mut types = HashMap::new();
        types.insert(0x00, CartType::from_str("ROM"));
        types.insert(0x01, CartType::from_str("MBC1"));
        types.insert(0x02, CartType::from_str("MBC1+RAM"));
        types.insert(0x03, CartType::from_str("MBC1+RAM+BATTERY"));
        types.insert(0x05, CartType::from_str("MBC2"));
        types.insert(0x06, CartType::from_str("MBC2+BATTERY"));
        types.insert(0x08, CartType::from_str("ROM+RAM"));
        types.insert(0x09, CartType::from_str("ROM+RAM+BATTERY"));
        types.insert(0x0B, CartType::from_str("MMM01"));
        types.insert(0x0C, CartType::from_str("MMM01+RAM"));
        types.insert(0x0D, CartType::from_str("MMM01+RAM+BATTERY"));
        types.insert(0x0F, CartType::from_str("MBC3+TIMER+BATTERY"));
        types.insert(0x10, CartType::from_str("MBC3+TIMER+RAM+BATTERY"));
        types.insert(0x11, CartType::from_str("MBC3"));
        types.insert(0x12, CartType::from_str("MBC3+RAM"));
        types.insert(0x13, CartType::from_str("MBC3+RAM+BATTERY"));
        types.insert(0x15, CartType::from_str("MBC4"));
        types.insert(0x16, CartType::from_str("MBC4+RAM"));
        types.insert(0x17, CartType::from_str("MBC4+RAM+BATTERY"));
        types.insert(0x19, CartType::from_str("MBC5"));
        types.insert(0x1A, CartType::from_str("MBC5+RAM"));
        types.insert(0x1B, CartType::from_str("MBC5+RAM+BATTERY"));
        types.insert(0x1C, CartType::from_str("MBC5+RUMBLE"));
        types.insert(0x1D, CartType::from_str("MBC5+RUMBLE+RAM"));
        types.insert(0x1E, CartType::from_str("MBC5+RUMBLE+RAM+BATTERY"));
        types.insert(0xFC, CartType::from_str("ROM+POCKET CAMERA"));
        types.insert(0xFD, CartType::from_str("ROM+BANDAI TAMA5"));
        types.insert(0xFE, CartType::from_str("HuC3"));
        types.insert(0xFF, CartType::from_str("HuC1+RAM+BATTERY"));
        types
    };
}


// Structs --------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct CartType {
    mapper: String,
    max_ram_size: u8,
    max_rom_size: u8,
    has_ram: bool,
    has_battery: bool,
    has_timer: bool,
    has_rumble: bool,
    has_camera: bool,
    has_tama: bool
}

impl CartType {
    fn from_str(s: &str) -> Self {
        let mapper = s.split('+').next().unwrap().to_string();
        Self {
            max_rom_size: match mapper.as_str() {
                "ROM" =>  0x00,
                "MBC1" => 0x06,
                "MBC2" => 0x03,
                "MBC3" => 0x06,
                "MBC4" => 0xFF, // ???
                "MBC5" => 0x07,
                _ => 0x00
            },
            max_ram_size: match mapper.as_str() {
                "ROM" =>  0x00,
                "MBC1" => 0x03,
                "MBC2" => 0x00, // 512x4 bits RAM built into the MBC2 chip, only the lower 4 bits can be read
                "MBC3" => 0x03,
                "MBC4" => 0xFF, // ???
                "MBC5" => 0x04,
                _ => 0x00
            },
            mapper,
            has_ram: s.contains("RAM"),
            has_battery: s.contains("BATTERY"),
            has_timer: s.contains("TIMER"),
            has_rumble: s.contains("RUMBLE"),
            has_camera: s.contains("CAMERA"),
            has_tama: s.contains("TAMA5")
        }
    }
}

#[derive(Debug)]
pub struct ROMInfo {
    // 0x104..=0x133
    logo: Vec<u8>,
    // 0x134..=0x13E
    title: [u8; 11],
    // 0x13F..=0x142
    designation: [u8; 4],
    // 0x143,
    cgb_flag: u8,
    // 0x144..=0x145
    sgb_license_code: [u8; 2],
    // 0x146,
    sgb_flag: u8,
    // 0x147,
    cart_type: Option<CartType>,
    // 0x148,
    cart_rom_size: u8,
    // 0x149,
    cart_ram_size: u8,
    // 0x14A
    country_code: u8,
    // 0x14B
    licensee_code: u8,
    // 0x14C
    mask_rom_version: u8,
    // 0x14D
    checksum_header: u8,
    // 0x14E..=0x14F
    checksum_rom: [u8; 2]
}

impl ROMInfo {
    fn from_buffer(buffer: &[u8]) -> Self {
        let mut title = [0; 11];
        title.copy_from_slice(&buffer[0x134..=0x13E]);
        Self {
            logo: buffer[0x104..=0x133].to_vec(),
            title,
            designation: [buffer[0x13F], buffer[0x140], buffer[0x141], buffer[0x142]],
            cgb_flag: buffer[0x143],
            sgb_license_code: [buffer[0x144], buffer[0x145]],
            sgb_flag: buffer[0x146],
            cart_type: CART_TYPES.get(&buffer[0x147]).map(|s| s.clone()),
            cart_rom_size: buffer[0x148],
            cart_ram_size: buffer[0x149],
            country_code: buffer[0x14A],
            licensee_code: buffer[0x14B],
            mask_rom_version: buffer[0x14C],
            checksum_header: buffer[0x14D],
            checksum_rom: [buffer[0x14E], buffer[0x14F]]
        }
    }
}


// ROM Generation -------------------------------------------------------------
pub struct Generator {
    #[allow(unused)]
    pub buffer: Vec<u8>
}

impl Generator {

    pub fn from_linker(linker: Linker) -> Self {
        Self {
            buffer: linker.into_rom_buffer()
        }
    }

    pub fn validate_rom(&self) -> Result<Vec<String>, String> {
        let _info = self.rom_info();
        // TODO validate logo
        // TODO validate cart type
        // TODO check if current cart_type supports configured rom size
        // TODO check if current cart_type supports configured ram size
        // TODO validate country code
        // TODO return a list of warnings or an error
        Ok(Vec::new())
    }

    pub fn finalize_rom(&mut self) {
        self.write_logo_to_rom();
        self.write_checksum();
    }

    pub fn rom_info(&self) -> ROMInfo {
        ROMInfo::from_buffer(&self.buffer)
    }

    fn write_logo_to_rom(&mut self) {
        for (index, b) in NINTENDO_LOGO.iter().enumerate() {
            self.buffer[index + 0x104] = *b;
        }
    }

    fn write_checksum(&mut self) {
        let mut header_checksum: i16 = 0;
        for i in 0x134..=0x14C {
            header_checksum = (((header_checksum - self.buffer[i] as i16) & 0xff) - 1) & 0xff;
        }
        self.buffer[0x14D] = header_checksum as u8;

        let mut rom_checksum: u32 = 0;
        for i in 0..self.buffer.len() {
            if i != 0x14E && i != 0x14F {
                rom_checksum += self.buffer[i] as u32;
            }
        }
        self.buffer[0x14E] = (rom_checksum >> 8) as u8;
        self.buffer[0x14F] = rom_checksum as u8;
    }

}

