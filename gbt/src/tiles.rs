// STD Dependencies -----------------------------------------------------------
use std::fs::File;
use std::path::PathBuf;
use std::io::{stdout, BufReader, Error as IOError, ErrorKind, Write};


// External Dependencies ------------------------------------------------------
use image::{DynamicImage, ImageFormat};


// GameBoy Tile Converter -----------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    sprite_size: &str,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {

    // Convert Arguments
    let sprite_size: u8 = sprite_size.parse().unwrap_or(8);
    let color_palette: Vec<[u8; 4]> = color_palette.iter().map(|c| [
        u8::from_str_radix(&c[0..2], 16).unwrap_or(0),
        u8::from_str_radix(&c[2..4], 16).unwrap_or(0),
        u8::from_str_radix(&c[4..6], 16).unwrap_or(0),
        255

    ]).collect();

    // Load Image
    let img = load_image(&image_file).map_err(|e| {
        format!("Failed to load image file: {}", e)
    })?;

    // Validate Image Dimensions
    let buffer = img.to_rgba();
    let (w, h) = (buffer.width() as usize, buffer.height() as usize);
    if w % 8 != 0 {
        return Err(format!("Image width of {} pixel(s) is not a multiple of 8.", w));

    } else if h % 8 != 0 {
        return Err(format!("Image height of {} pixel(s) is not a multiple of 8.", h));
    }

    // Convert Image Pixels to Palette Entries
    let mut indicies: Vec<u8> = Vec::with_capacity(w * h);
    'pixels: for (index, pixel) in buffer.pixels().enumerate() {
        for (palette_index, color) in color_palette.iter().enumerate() {
            if &pixel.data == color {
                indicies.push(palette_index as u8);
                continue 'pixels;
            }
        }
        let x = index % w;
        let y = index / h;
        return Err(format!("Image pixel at {}x{} with color {:?} does not map to any entries in the color palette of {:?}", x, y, pixel.data, color_palette));
    }

    // Convert Image Into tiles
    let mut gb_tiles: Vec<[u8; 16]> = Vec::with_capacity(w / 8 * h / 8);
    for ty in 0..h / 8 {
        for tx in 0..w / 8 {
            // Tile
            let ox = tx * 8;
            let oy = ty * 8;
            let mut bytes = [0; 16];
            for y in 0..8 {
                for x in 0..8 {
                    let i = indicies[(oy + y) * w + ox + x];
                    if i & 1 == 1 {
                        bytes[y * 2] |= 1 << (7 - x);
                    }
                    if i & 2 == 2 {
                        bytes[y * 2 +1] |= 1 << (7 - x);
                    }
                }
            }
            gb_tiles.push(bytes);
        }
    }

    // Re-Arrange for 8x16 sprites
    if sprite_size == 16 {
        // TODO
    }

    // Serialize into output bytes
    let mut gb_bytes: Vec<u8> = Vec::with_capacity(gb_tiles.len() * 16);
    for tile in gb_tiles {
        gb_bytes.extend_from_slice(&tile);
    }

    if let Some(outfile) = output_file {
        save_gameboy_binary(&outfile, gb_bytes).map_err(|e| {
            format!("Failed to load GameBoy data: {}", e)
        })

    } else {
        stdout().write_all(&gb_bytes).map_err(|_| {
            "Failed to write to stdout".to_string()
        })
    }
}


// Helpers --------------------------------------------------------------------
fn load_image(path: &PathBuf) -> Result<DynamicImage, IOError> {
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

fn save_gameboy_binary(path: &PathBuf, bytes: Vec<u8>) -> Result<(), IOError> {
    let mut file = File::create(path)?;
    file.write_all(&bytes)
}

