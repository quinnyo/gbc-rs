// STD Dependencies -----------------------------------------------------------
use std::fs::File;
use std::path::PathBuf;
use std::io::{stdin, stdout, BufReader, Error as IOError, ErrorKind, Read, Write};


// External Dependencies ------------------------------------------------------
use image::{gif, DynamicImage, AnimationDecoder, ImageFormat, RgbaImage};


// Structs --------------------------------------------------------------------
#[derive(Debug)]
pub struct Indicies {
    width: usize,
    height: usize,
    pixels: Vec<u8>
}


// Helpers --------------------------------------------------------------------

pub fn indicies_to_tiles(indicies: Indicies) -> Vec<[u8; 16]> {
    // See: http://www.huderlem.com/demos/gameboy2bpp.html
    //
    //   Tile:                                     Image:
    //
    //   .33333..                     .33333.. -> 01111100 -> $7C
    //   22...22.                                 01111100 -> $7C
    //   11...11.                     22...22. -> 00000000 -> $00
    //   2222222. <-- digits                      11000110 -> $C6
    //   33...33.     represent       11...11. -> 11000110 -> $C6
    //   22...22.     color                       00000000 -> $00
    //   11...11.     numbers         2222222. -> 00000000 -> $00
    //   ........                                 11111110 -> $FE
    //                                33...33. -> 11000110 -> $C6
    //                                            11000110 -> $C6
    //                                22...22. -> 00000000 -> $00
    //                                            11000110 -> $C6
    //                                11...11. -> 11000110 -> $C6
    //                                            00000000 -> $00
    //                                ........ -> 00000000 -> $00
    //                                            00000000 -> $00
    let (w, h) = (indicies.width, indicies.height);
    let mut tiles: Vec<[u8; 16]> = Vec::with_capacity(w / 8 * h / 8);
    for ty in 0..h / 8 {
        for tx in 0..w / 8 {
            let ox = tx * 8;
            let oy = ty * 8;
            let mut bytes = [0; 16];
            for y in 0..8 {
                for x in 0..8 {
                    let i = indicies.pixels[(oy + y) * w + ox + x];
                    if i & 1 == 1 {
                        bytes[y * 2] |= 1 << (7 - x);
                    }
                    if i & 2 == 2 {
                        bytes[y * 2 + 1] |= 1 << (7 - x);
                    }
                }
            }
            tiles.push(bytes);
        }
    }
    tiles
}

pub fn mirror_tile_vertical(tile: &[u8; 16]) -> [u8; 16] {
    // We invert the order of the 8 byte pairs
    [
        tile[14],
        tile[15],

        tile[13],
        tile[12],

        tile[10],
        tile[11],

        tile[8],
        tile[9],

        tile[6],
        tile[7],

        tile[4],
        tile[5],

        tile[2],
        tile[3],

        tile[0],
        tile[1]
    ]
}

pub fn mirror_tile_horizontal(tile: &[u8; 16]) -> [u8; 16] {
    // We invert the order of the bits in reach byte
    [
        reverse_bits(tile[0]),
        reverse_bits(tile[1]),
        reverse_bits(tile[2]),
        reverse_bits(tile[3]),
        reverse_bits(tile[4]),
        reverse_bits(tile[5]),
        reverse_bits(tile[6]),
        reverse_bits(tile[7]),
        reverse_bits(tile[8]),
        reverse_bits(tile[9]),
        reverse_bits(tile[10]),
        reverse_bits(tile[11]),
        reverse_bits(tile[12]),
        reverse_bits(tile[13]),
        reverse_bits(tile[14]),
        reverse_bits(tile[15]),
    ]
}

fn reverse_bits(mut b: u8) -> u8 {
   b = (b & 0xF0) >> 4 | (b & 0x0F) << 4;
   b = (b & 0xCC) >> 2 | (b & 0x33) << 2;
   b = (b & 0xAA) >> 1 | (b & 0x55) << 1;
   b
}

pub fn split_indicies(indicies: Indicies) -> (Indicies, Indicies) {
    (
        Indicies {
            width: indicies.width,
            height: indicies.height,
            pixels: indicies.pixels.iter().map(|p| *p & 3).collect()

        }, Indicies {
            width: indicies.width,
            height: indicies.height,
            pixels: indicies.pixels.iter().map(|p| (*p & 12) >> 2).collect()
        }
    )
}

pub fn image_to_indicies(buffer: RgbaImage, color_palette: &[[u8; 4]]) -> Result<Indicies, String> {
    let (w, h) = (buffer.width() as usize, buffer.height() as usize);
    let mut pixels: Vec<u8> = Vec::with_capacity(w * h);
    'pixels: for (index, pixel) in buffer.pixels().enumerate() {
        for (palette_index, color) in color_palette.iter().enumerate() {
            if &pixel.data == color {
                pixels.push(palette_index as u8);
                continue 'pixels;
            }
        }
        let x = index % w;
        let y = index / h;
        return Err(format!("Image pixel at {}x{} with color {:?} does not map to any entries in the color palette of {:?}", x, y, pixel.data, color_palette));
    }
    Ok(Indicies {
        width: w,
        height: h,
        pixels
    })
}

pub fn to_palette(color_palette: Vec<&str>) -> Vec<[u8; 4]> {
    color_palette.iter().map(|c| [
        u8::from_str_radix(&c[0..2], 16).unwrap_or(0),
        u8::from_str_radix(&c[2..4], 16).unwrap_or(0),
        u8::from_str_radix(&c[4..6], 16).unwrap_or(0),
        255

    ]).collect()
}

pub fn load_gif(path: &PathBuf) -> Result<Vec<(u16, RgbaImage)>, IOError> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let img = gif::Decoder::new(reader).map_err(|_| {
        IOError::new(ErrorKind::InvalidInput, "Failed to load gif image")
    })?;

    let frames = img.into_frames().collect_frames().map_err(|_| {
        IOError::new(ErrorKind::InvalidInput, "Failed to load gif frames")
    })?;
    Ok(frames.into_iter().map(|f| (f.delay().to_integer(), f.into_buffer())).collect())
}

pub fn load_image(path: &PathBuf) -> Result<DynamicImage, IOError> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let ext = path.extension().map(|s| s.to_str().unwrap_or("").to_ascii_lowercase()).unwrap_or_else(|| String::new());
    let format = match ext.as_str() {
        "png" => ImageFormat::PNG,
        "bmp" => ImageFormat::BMP,
        "pnm" => ImageFormat::PNM,
        "tga" => ImageFormat::TGA,
        "tiff" => ImageFormat::TIFF,
        _ => return Err(
            IOError::new(ErrorKind::InvalidInput, "Unsupported image format")
        )
    };
    image::load(reader, format).map_err(|_| {
        IOError::new(ErrorKind::InvalidInput, "Unsupported image format")
    })
}

pub fn load_binary(path: Option<PathBuf>) -> Result<Vec<u8>, IOError> {
    let mut bytes = Vec::new();
    if let Some(path) = path {
        let mut file = File::open(path)?;
        file.read_to_end(&mut bytes)?;

    } else {
        stdin().read_to_end(&mut bytes)?;
    }
    Ok(bytes)
}

pub fn save_binary(path: &PathBuf, bytes: Vec<u8>) -> Result<(), IOError> {
    let mut file = File::create(path)?;
    file.write_all(&bytes)
}

pub fn output_binary(output_file: Option<PathBuf>, bytes: Vec<u8>) -> Result<(), String> {
    if let Some(outfile) = output_file {
        save_binary(&outfile, bytes).map_err(|e| {
            format!("Failed to save GameBoy data: {}", e)
        })

    } else {
        stdout().write_all(&bytes).map_err(|_| {
            "Failed to write GameBoy data to stdout".to_string()
        })
    }
}

pub fn load_text(path: &PathBuf) -> Result<String, IOError> {
    let mut file = File::open(path)?;
    let mut text = String::new();
    file.read_to_string(&mut text)?;
    Ok(text)
}

pub fn output_text(output_file: Option<PathBuf>, text: String) -> Result<(), String> {
    if let Some(outfile) = output_file {
        save_binary(&outfile, text.into_bytes()).map_err(|e| {
            format!("Failed to save GameBoy data: {}", e)
        })

    } else {
        stdout().write_all(&text.into_bytes()).map_err(|_| {
            "Failed to write GameBoy data to stdout".to_string()
        })
    }
}

