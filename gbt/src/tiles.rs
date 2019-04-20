// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;


// GameBoy Tile Converter -----------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    sprite_size: &str,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {

    // Convert Arguments
    let sprite_size: u8 = sprite_size.parse().unwrap_or(8);
    let color_palette = util::to_palette(color_palette);

    // Load Image
    let img = util::load_image(&image_file).map_err(|e| {
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
    let indicies = util::image_to_indicies(buffer, &color_palette)?;

    // Convert Image Into tiles
    let gb_tiles = util::indicies_to_tiles(indicies);

    // Re-Arrange for 8x16 sprites
    if sprite_size == 16 {
        // TODO
    }

    // Serialize into output bytes
    let mut gb_bytes: Vec<u8> = Vec::with_capacity(gb_tiles.len() * 16);
    for tile in gb_tiles {
        gb_bytes.extend_from_slice(&tile);
    }

    util::output_binary(output_file, gb_bytes)
}

