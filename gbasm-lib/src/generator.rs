// External Dependencies ------------------------------------------------------
use lazy_static::lazy_static;


// Internal Dependencies ------------------------------------------------------
use crate::linker::Linker;


// Statics --------------------------------------------------------------------
#[allow(unused)]
static NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
];

lazy_static! {

}

// static ROM_KB_SIZES: [[]; 48] = [
//     0x00: [  32,   0],
//     0x01: [  64,   4],
//     0x02: [ 128,   8],
//     0x03: [ 256,  16],
//     0x04: [ 512,  32],
//     0x05: [1024,  64], // Only 63 banks used by MBC1
//     0x06: [2048, 128], // Only 125 banks used by MBC1
//     0x07: [4096, 256],
//     0x52: [1152,  72],
//     0x53: [1280,  80],
//     0x54: [1536,  96]
// ]

/*
    RAM_SIZES: {
        0x00: [ 0, 0],  // None (must always be set with MBC2 even though it has 512x4 bits RAM)
        0x01: [ 2, 1],  // 1 Bank (only one quarter is used)
        0x02: [ 8, 1],  // 1 Bank (Full)
        0x03: [32, 4],  // 4 Banks
        0x04: [128, 16] // 16 Banks
    },

    MAX_ROM_SIZE: {
        ROM:  0x00,
        MBC1: 0x06,
        MBC2: 0x03,
        MBC3: 0x06,
        MBC4: 0xFF, // ???
        MBC5: 0x07
    },

    MAX_RAM_SIZE: {
        ROM:  0x00,
        MBC1: 0x03,
        MBC2: 0x00, // 512x4 bits RAM built into the MBC2 chip, only the lower 4 bits can be read
        MBC3: 0x03,
        MBC4: 0xFF, // ???
        MBC5: 0x04
    },

    DESTINATION: {
        0x00: 'Japanese',
        0x01: 'Non-Japanese'
    },

    LICENSEES: {
        0x33: 'Super Gameboy',
        0x79: 'Accolade',
        0xA4: 'Konami'
    },

    COUNTRY_CODES: [
        0x00,
        0x01
    ]
    TYPES: {
        0x00: cartridgeType('ROM'),
        0x01: cartridgeType('MBC1'),
        0x02: cartridgeType('MBC1+RAM'),
        0x03: cartridgeType('MBC1+RAM+BATTERY'),
        0x05: cartridgeType('MBC2'),
        0x06: cartridgeType('MBC2+BATTERY'),
        0x08: cartridgeType('ROM+RAM'),
        0x09: cartridgeType('ROM+RAM+BATTERY'),
        0x0B: cartridgeType('MMM01'),
        0x0C: cartridgeType('MMM01+RAM'),
        0x0D: cartridgeType('MMM01+RAM+BATTERY'),
        0x0F: cartridgeType('MBC3+TIMER+BATTERY'),
        0x10: cartridgeType('MBC3+TIMER+RAM+BATTERY'),
        0x11: cartridgeType('MBC3'),
        0x12: cartridgeType('MBC3+RAM'),
        0x13: cartridgeType('MBC3+RAM+BATTERY'),
        0x15: cartridgeType('MBC4'),
        0x16: cartridgeType('MBC4+RAM'),
        0x17: cartridgeType('MBC4+RAM+BATTERY'),
        0x19: cartridgeType('MBC5'),
        0x1A: cartridgeType('MBC5+RAM'),
        0x1B: cartridgeType('MBC5+RAM+BATTERY'),
        0x1C: cartridgeType('MBC5+RUMBLE'),
        0x1D: cartridgeType('MBC5+RUMBLE+RAM'),
        0x1E: cartridgeType('MBC5+RUMBLE+RAM+BATTERY'),
        0xFC: cartridgeType('ROM+POCKET CAMERA'),
        0xFD: cartridgeType('ROM+BANDAI TAMA5'),
        0xFE: cartridgeType('HuC3'),
        0xFF: cartridgeType('HuC1+RAM+BATTERY')
    },

    function cartridgeType(ident) {
        return {
            Mapper: ident.split('+')[0],
            Ram: ident.indexOf('RAM') !== -1,
            Battery: ident.indexOf('BATTERY') !== -1,
            Timer: ident.indexOf('TIMER') !== -1,
            Rumble: ident.indexOf('RUMBLE') !== -1,
            Camera: ident === 'ROM+POCKET CAMERA',
            BandaiTama: ident === 'ROM+BANDAI TAMA5',
        };
    }

*/

// Structs --------------------------------------------------------------------
#[allow(unused)]
pub struct ROMInfo {
    // 0x104..=0x133
    logo: [u8; 48],
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
    cart_type: u8,
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
    checksum_cpl: u8,
    // 0x14E..=0x14F
    checksum: [u8; 2]
}


// ROM Generation -------------------------------------------------------------
pub struct Generator {
    #[allow(unused)]
    buffer: Vec<u8>
}

impl Generator {

    pub fn from_linker(linker: Linker) -> Self {
        Self {
            buffer: linker.into_rom_buffer()
        }
    }

    pub fn validate_rom(&self) -> Result<Vec<String>, String> {
        // TODO return a list of warnings or an error
        Ok(Vec::new())
    }

    #[allow(unused)]
    fn write_logo_to_rom(&mut self) {
        for (index, b) in NINTENDO_LOGO.iter().enumerate() {
            self.buffer[index + 0x104] = *b;
        }
    }

    // pub fn info(&self) -> ROMInfo {
    //     ROMInfo {

    //     }
    // }

}


/*
    getRomInfo(buffer) {
        return {

            // General
            logo: buffer.slice(0x104, 0x134),
            title: buffer.slice(0x134, 0x143).toString('ascii'),
            colorGameBoyFlag: buffer[0x143],

            // Super Gameboy related
            sgbLicenseeCode: (buffer[0x144] << 8) & buffer[0x145],
            sgbFlag: buffer[0x146],

            // Cartrige info
            type: buffer[0x147],
            rom: buffer[0x148],
            ram: buffer[0x149],
            countryCode: buffer[0x14A],
            licenseeCode: buffer[0x14B], // 33 = super gameboy, will use the code from above
            versionNumber: buffer[0x14C],

            // Checksums
            headerChecksum: buffer[0x14D],
            romChecksum: (buffer[0x14D] << 8) & buffer[0x14E],

            // Warning and Errors generated
            warnings: [],
            errors: []

        };
    },

    setRomChecksums(buffer) {

        // Header
        let checksum = 0, i;
        for(i = 0x134; i < 0x14D; i++) {
            checksum = (((checksum - buffer[i]) & 0xff) - 1) & 0xff;
        }

        buffer[0x14D] = checksum;

        // ROM
        checksum = 0;
        for(i = 0; i < buffer.length; i++) {
            if (i !== 0x14E && i !== 0x14F) {
                checksum += buffer[i];
            }
        }

        buffer[0x14E] = (checksum >> 8) & 0xff;
        buffer[0x14F] = checksum & 0xff;

    },
    getRomInfo(buffer) {
        return {

            // General
            logo: buffer.slice(0x104, 0x134),
            title: buffer.slice(0x134, 0x143).toString('ascii'),
            colorGameBoyFlag: buffer[0x143],

            // Super Gameboy related
            sgbLicenseeCode: (buffer[0x144] << 8) & buffer[0x145],
            sgbFlag: buffer[0x146],

            // Cartrige info
            type: buffer[0x147],
            rom: buffer[0x148],
            ram: buffer[0x149],
            countryCode: buffer[0x14A],
            licenseeCode: buffer[0x14B], // 33 = super gameboy, will use the code from above
            versionNumber: buffer[0x14C],

            // Checksums
            headerChecksum: buffer[0x14D],
            romChecksum: (buffer[0x14D] << 8) & buffer[0x14E],

            // Warning and Errors generated
            warnings: [],
            errors: []

        };
    },

    setRomChecksums(buffer) {

        // Header
        let checksum = 0, i;
        for(i = 0x134; i < 0x14D; i++) {
            checksum = (((checksum - buffer[i]) & 0xff) - 1) & 0xff;
        }

        buffer[0x14D] = checksum;

        // ROM
        checksum = 0;
        for(i = 0; i < buffer.length; i++) {
            if (i !== 0x14E && i !== 0x14F) {
                checksum += buffer[i];
            }
        }

        buffer[0x14E] = (checksum >> 8) & 0xff;
        buffer[0x14F] = checksum & 0xff;

    },
*/
