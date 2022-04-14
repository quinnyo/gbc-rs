// STD Dependencies -----------------------------------------------------------
use std::iter;
use std::path::PathBuf;
use std::collections::HashMap;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4::compress;


// Types ----------------------------------------------------------------------
type SGBTile = ([u8; 16], [u8; 16]);


// SGB Border Converter -------------------------------------------------------
pub fn convert_border(
    image_file: PathBuf,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {
    let (map, tiles, palette) = sgb_border_load(image_file, color_palette)?;

    let mut lower_tile_data = Vec::with_capacity(128 * 16);
    let mut upper_tile_data = Vec::with_capacity(128 * 16);
    let mut map_data = Vec::with_capacity(32 * 32 * 2 + 32);

    // Raw Tiles Data
    let tiles = with_padding(tiles, 256, ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));
    for (low, high) in tiles.iter().take(128) {
        lower_tile_data.extend_from_slice(low);
        lower_tile_data.extend_from_slice(high);
    }

    for (low, high) in tiles.iter().skip(128) {
        upper_tile_data.extend_from_slice(low);
        upper_tile_data.extend_from_slice(high);
    }

    // Tile to SNES Screen Mapping
    for word in with_padding(map, 32 * 32, 0) {
        map_data.push(word as u8);
        map_data.push((word >> 8) as u8);
    }

    // SNES Palettes for Tile Data
    for color in with_padding(palette, 16, [0, 0, 0, 0]) {
        let word = (color[2] as u16 / 8) << 10 | (color[1] as u16 / 8) << 5 | (color[0] as u16 / 8) << 0;
        map_data.push(word as u8);
        map_data.push((word >> 8) as u8);
    }

    // Compress data
    let (mut lower_tile_data, _) = compress(&lower_tile_data, true);
    let (mut upper_tile_data, _) = compress(&upper_tile_data, true);
    let (mut map_data, _) = compress(&map_data, true);
    let upper_offset = lower_tile_data.len();
    let map_offset = upper_offset + upper_tile_data.len();

    // Combine compressed data with offset markers
    let mut output = vec![
        (upper_offset as u8),
        (upper_offset >> 8) as u8,
        (map_offset as u8),
        (map_offset >> 8) as u8
    ];
    output.append(&mut lower_tile_data);
    output.append(&mut upper_tile_data);
    output.append(&mut map_data);

    util::output_binary(output_file, output)
}

fn sgb_border_load(
    image_file: PathBuf,
    color_palette: Vec<&str>

) -> Result<(Vec<u16>, Vec<SGBTile>, Vec<[u8; 4]>), String> {

    // Convert Arguments
    // TODO support multiple palettes later on
    let color_palette = util::to_palette(color_palette);

    // Load Image
    let img = util::load_image(&image_file).map_err(|e| {
        format!("Failed to load image file: {}", e)
    })?;

    // Validate Image Dimensions
    let buffer = img.to_rgba();
    let (w, h) = (buffer.width() as usize, buffer.height() as usize);
    if w != 256 {
        return Err(format!("Image width of {} pixel(s) is not exactly 256.", w));

    } else if h != 224 {
        return Err(format!("Image height of {} pixel(s) is not exactly 224.", h));
    }

    // Convert Image Pixels to Palette Entries
    let indicies = util::image_to_indicies(buffer, &color_palette)?;

    // Split the bit planes
    let (low, high) = util::split_indicies(indicies);

    // Convert bit planes into separate gb tiles
    let low_tiles = util::indicies_to_tiles(low);
    let high_tiles = util::indicies_to_tiles(high);

    // Re-combine the split tiles for de-duplication
    let combined_tiles: Vec<SGBTile> = low_tiles.into_iter().zip(high_tiles.into_iter()).collect();

    // Analyze image tiles
    let mut tile_index_map: HashMap<SGBTile, usize> = HashMap::new();
    let mut unique_tiles: Vec<SGBTile> = Vec::new();
    let mut border_layout: Vec<(u16, u16, u16)> = Vec::new();

    // Setup 0 character for transparent gameboy screen
    unique_tiles.push(combined_tiles[166].clone());
    tile_index_map.insert(combined_tiles[166], 0);

    for tile in combined_tiles {

        // Check for plain tile
        let entry = if let Some(index) = tile_index_map.get(&tile) {
            Some((*index as u16, 0, 0))

        // Check for horizontal mirror
        } else if let Some(index) = tile_index_map.get(&mirror_h(&tile)) {
            Some((*index as u16, 1, 0))

        // Check for vertical mirror
        } else if let Some(index) = tile_index_map.get(&mirror_v(&tile)) {
            Some((*index as u16, 0, 1))

        // Check for both horizontal and vertical mirror
        // TODO check if SNES is capable of this
        // } else if let Some(index) = tile_index_map.get(&mirror_v(&mirror_h(&tile))) {
        //     Some((*index as u16, 1, 1))

        } else {
            None
        };

        // Tile is already mapped
        let entry = if let Some(entry) = entry {
            entry

        // Insert tile
        } else {
            let index = unique_tiles.len();
            unique_tiles.push(tile.clone());
            tile_index_map.insert(tile, index);
            (index as u16, 0, 0)
        };

        // Build image data
        border_layout.push(entry);

    }

    // Convert Border Layout into Super GameBoy Data
    let border_map: Vec<u16> = border_layout.into_iter().map(|(index, h, v)| {
        // V-Flip    H-Flip    BG Priority  Palette#    Tile#
        (v << 15) | (h << 14) | (0 << 13) | (4 << 10) | (index & 0xFF)

    }).collect();

    Ok((
        border_map,
        unique_tiles,
        color_palette
    ))

}

fn mirror_h(tile: &SGBTile) -> SGBTile {
    (
        util::mirror_tile_horizontal(&tile.0),
        util::mirror_tile_horizontal(&tile.1)
    )
}

fn mirror_v(tile: &SGBTile) -> SGBTile {
    (
        util::mirror_tile_vertical(&tile.0),
        util::mirror_tile_vertical(&tile.1)
    )
}

fn with_padding<T: Clone>(mut vec: Vec<T>, len: usize, default: T) -> Vec<T> {
    let padding = len - vec.len();
    vec.append(&mut iter::repeat(default).take(padding).collect());
    vec
}

