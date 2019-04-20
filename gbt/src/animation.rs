// STD Dependencies -----------------------------------------------------------
use std::iter;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use gbc_lib::lz4::compress;


// Internal Dependencies ------------------------------------------------------
use super::util;


// Structs --------------------------------------------------------------------
#[derive(Debug)]
struct AnimationFrame {
    tiles: Vec<[u8; 16]>,
    delay: u8
}


// GameBoy Animation Converter ------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {

    // Convert Arguments
    let color_palette = util::to_palette(color_palette);

    // Load Image
    let frames = util::load_gif(&image_file).map_err(|e| {
        format!("Failed to load gif file: {}", e)
    })?;

    // Convert Image Pixels to Palette Entries
    let mut width = 0;
    let mut height = 0;
    let mut animation_frames = Vec::new();
    for (index, (delay, frame)) in frames.into_iter().enumerate() {
        // TODO fail if frames have different sizes
        width = frame.width();
        height = frame.height();
        animation_frames.push(AnimationFrame {
            tiles: util::indicies_to_tiles(util::image_to_indicies(frame, &color_palette).map_err(|e| {
                format!("Frame #{}: {}", index, e)
            })?),
            delay: delay as u8
        });
    }

    // Set up Animation Header
    let tile_count = (width / 8 * height / 8) as usize;
    let mut animation_data = vec![
        tile_count as u8,
        (width / 8) as u8,
        animation_frames.len() as u8
    ];

    let mut previous_frame = AnimationFrame {
        tiles: iter::repeat([0; 16]).take(tile_count).collect(),
        delay: 0
    };

    for frame in animation_frames {

        let mut frame_updates: Vec<u8> = vec![];
        let mut frame_update_count = 0;

        // Calculate Frame Differences
        for (index, (tile, previous_tile)) in frame.tiles.iter().zip(previous_frame.tiles.iter()).enumerate() {
            if tile != previous_tile {
                frame_updates.push(index as u8);
                frame_updates.extend(tile.iter().cloned());
                frame_update_count += 1;
            }
        }

        let (mut frame_data, _) = compress(&frame_updates, true);
        animation_data.push(frame_update_count);
        animation_data.push(frame.delay / 2);
        animation_data.append(&mut frame_data);
        previous_frame = frame;
    }

    util::output_binary(output_file, animation_data)
}

