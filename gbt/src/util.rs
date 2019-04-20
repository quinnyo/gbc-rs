// STD Dependencies -----------------------------------------------------------
use std::fs::File;
use std::path::PathBuf;
use std::io::{stdout, BufReader, Error as IOError, ErrorKind, Read, Write};


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
                        bytes[y * 2 +1] |= 1 << (7 - x);
                    }
                }
            }
            tiles.push(bytes);
        }
    }
    tiles
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

