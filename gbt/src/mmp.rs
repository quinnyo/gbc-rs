// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use super::{lmms, util};


// GameBoy Music Converter ----------------------------------------------------
pub fn convert(
    mmp_files: Vec<PathBuf>,
    output_file: Option<PathBuf>

) -> Result<(), String> {

    // let mut projects = Vec::new();
    for file in mmp_files {
        let text = util::load_text(&file).map_err(|e| {
            format!("Failed to load LMMS project file: {}", e)
        })?;
        let p = lmms::Project::from_string(text)?;
        println!("{:#?}", p);
    }

    util::output_text(output_file, "; Empty\n".to_string())
}

