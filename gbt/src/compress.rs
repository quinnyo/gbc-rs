// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::util;
use super::lz4;
use super::hgb;


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

pub fn convert_ex(
    input_file: Option<PathBuf>,
    output_file: Option<PathBuf>

) -> Result<(), String> {
    let input_bytes = util::load_binary(input_file).map_err(|e| {
        format!("Failed to load input file: {}", e)
    })?;
    let (compressed_bytes, _) = hgb::compress(&input_bytes);
    //println!("{:?} {}", compressed_bytes, compressed_bytes.len());
    //unreachable!();
    util::output_binary(output_file, compressed_bytes)
}

