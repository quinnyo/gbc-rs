// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4;


// GameBoy Data Compressor ----------------------------------------------------
pub fn convert(
    input_file: Option<PathBuf>,
    output_file: Option<PathBuf>,
    info: bool

) -> Result<(), String> {
    let input_bytes = util::load_binary(input_file).map_err(|e| {
        format!("Failed to load input file: {}", e)
    })?;
    if info {
        lz4::analyze(&input_bytes);
        Ok(())

    } else {
        let (compressed_bytes, _) = lz4::compress(&input_bytes, true);
        util::output_binary(output_file, compressed_bytes)
    }
}

