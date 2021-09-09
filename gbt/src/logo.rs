// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;


// GameBoy Tile Converter -----------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    bit_palette: Vec<&str>

) -> Result<(), String> {

    // Convert Arguments
    let bit_palette = util::to_bit_palette(bit_palette);

    // Load Image
    let img = util::load_image(&image_file).map_err(|e| {
        format!("Failed to load image file: {}", e)
    })?;

    // Validate Image Dimensions
    let buffer = img.to_rgba();
    let (w, h) = (buffer.width() as usize, buffer.height() as usize);
    if w != 48 {
        return Err(format!("Image width of {} pixel(s) is not 48.", w));

    } else if h != 8 {
        return Err(format!("Image height of {} pixel(s) is not 8.", h));
    }

    // Convert Logo Pixels to Palette Entries
    let indicies = util::image_to_indicies(buffer, &bit_palette)?;

    // Convert Image Into tiles
    let logo_bytes: Vec<String> = util::indicies_to_logo_bytes(indicies).into_iter().map(|b| {
        format!("0x{:02X?}", b)

    }).collect();
    println!("{}", logo_bytes[0..16].join(", "));
    println!("{}", logo_bytes[16..32].join(", "));
    println!("{}", logo_bytes[32..48].join(", "));

    Ok(())
}

