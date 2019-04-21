// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4::compress;


// GameBoy Data Compressor ----------------------------------------------------
pub fn convert(
    input_file: Option<PathBuf>,
    output_file: Option<PathBuf>

) -> Result<(), String> {
    let input_bytes = util::load_binary(input_file).map_err(|e| {
        format!("Failed to load input file: {}", e)
    })?;
    let (compressed_bytes, _) = compress(&input_bytes, true);
    util::output_binary(output_file, compressed_bytes)
}

