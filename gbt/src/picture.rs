// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::collections::HashSet;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4::compress;


// GameBoy Background Picture Converter ---------------------------------------
pub fn convert(
    image_file: PathBuf,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {

    // Convert Arguments
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
    let mut unique_tiles = HashSet::new();
    for tile in &gb_tiles {
        unique_tiles.insert(*tile);
    }
    let unique_tiles: Vec<[u8; 16]> = unique_tiles.into_iter().collect();
    if unique_tiles.len() > 256 {
        return Err(format!("Image contains more than 256 unique 8x8 tiles ({})", unique_tiles.len()));
    }

    let index_data: Vec<u8> = gb_tiles.iter().map(|gb_tile| {
        unique_tiles.iter().position(|t| t == gb_tile).unwrap_or(0) as u8

    }).collect();

    // Setup picture header
    let mut picture_data = Vec::new();

    // Extend with compressed tile data
    let mut tile_data = Vec::new();
    for tile in &unique_tiles {
        tile_data.extend_from_slice(tile);
    }
    let (mut tile_data, _) = compress(&tile_data, true);
    picture_data.append(&mut tile_data);
    picture_data.push(unique_tiles.len() as u8);

    // Extend with compressed index data
    let (mut index_data, _) = compress(&index_data, true);
    picture_data.append(&mut index_data);
    picture_data.push((w / 8) as u8);
    picture_data.push((h / 8) as u8);

    util::output_binary(output_file, picture_data)
}

