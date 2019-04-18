// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// GameBoy Tile Converter -----------------------------------------------------
pub fn convert(
    image_file: PathBuf,
    sprite_size: &str,
    color_palette: Vec<&str>,
    output_file: Option<PathBuf>

) -> Result<(), String> {
    let sprite_size: u8 = sprite_size.parse().unwrap_or(8);
    let color_palette: Vec<[u8; 3]> = color_palette.iter().map(|c| [
        u8::from_str_radix(&c[0..2], 16).unwrap_or(0),
        u8::from_str_radix(&c[2..4], 16).unwrap_or(0),
        u8::from_str_radix(&c[4..6], 16).unwrap_or(0)

    ]).collect();

    // TODO load image file and parsej
    // TODO image size must be multiple of 8x8
    // TODO map colors to indicies fail if color is not in palette (report pixel position and value)

    // TODO generate tiles
    // TODO combine tiles based on sprite_size

    // TODO output to raw stdout if no outfile is specified otherwise write to file
    println!("Image: {}", image_file.display());
    println!("Sprite Size: {}", sprite_size);
    println!("Palette: {:?}", color_palette);
    if let Some(outfile) = output_file {
        println!("Outfile: {}", outfile.display());
    }

    Ok(())
}

