// STD Dependencies -----------------------------------------------------------
use std::iter;
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4::compress;


// Structs --------------------------------------------------------------------
#[derive(Debug)]
struct AnimationFrame {
    tiles: Vec<[u8; 16]>,
    delay: u16
}


// GameBoy Animation Converter ------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    color_palette: Vec<&str>,
    decode_buffer_size: usize,
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
            delay: delay / 10
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

        let mut tile_updates: Vec<u8> = vec![];
        let mut tile_update_count = 0;

        // Calculate Frame Differences
        for (index, (tile, previous_tile)) in frame.tiles.iter().zip(previous_frame.tiles.iter()).enumerate() {
            if tile != previous_tile {
                let tile_data = tile.iter().cloned();
                if tile_updates.len() + 1 + tile_data.len() >= decode_buffer_size {
                    serialize_updates(
                        0,
                        &mut tile_update_count,
                        &mut tile_updates,
                        &mut animation_data
                    );
                }
                tile_updates.push(index as u8);
                tile_updates.extend(tile_data);
                tile_update_count += 1;
            }
        }

        serialize_updates(
            (frame.delay / 2) as u8,
            &mut tile_update_count,
            &mut tile_updates,
            &mut animation_data
        );
        previous_frame = frame;
    }

    util::output_binary(output_file, animation_data)
}

fn serialize_updates(
    delay: u8,
    tile_update_count: &mut u8,
    tile_updates: &mut Vec<u8> ,
    animation_data: &mut Vec<u8>
) {
    if *tile_update_count > 0 {

        // Compressed tile data
        let (mut frame_data, _) = compress(&tile_updates, true);
        animation_data.append(&mut frame_data);
        tile_updates.clear();

        // If delay is zero there will be more tile update data to uncompress and apply
        animation_data.push(*tile_update_count);
        animation_data.push(delay);
        *tile_update_count = 0;

    } else {
        *animation_data.last_mut().unwrap() = delay;
    }

}

