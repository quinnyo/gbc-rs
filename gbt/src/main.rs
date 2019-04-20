// STD Dependencies -----------------------------------------------------------
use std::process;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use colored::Colorize;
use clap::{Arg, App, SubCommand};


// Internal Dependencies ------------------------------------------------------
mod animation;
mod mmp;
mod tiles;
mod util;


// CLI Interface --------------------------------------------------------------
fn main() {

    // TODO MMP file conversion
    // TODO lz4 compression

    let matches = App::new("gbt")
        .version("0.1")
        .author("Ivo Wetzel <ivo.wetzel@googlemail.com>")
        .about("GameBoy Tooling")
        .subcommand(SubCommand::with_name("tiles")
            .about("converts image files into the GameBoy tile format")
            .author("Ivo Wetzel <ivo.wetzel@googlemail.com>")
            .version("0.1")
            .arg(Arg::with_name("IMAGE_FILE")
                .help("Input image file")
                .required(true)
                .index(1)
            )
            .arg(Arg::with_name("OUTPUT_FILE")
                .long("out-file")
                .short("o")
                .takes_value(true)
                .help("GameBoy tile file to generate")
            )
            .arg(Arg::with_name("SPRITE_SIZE")
                .long("sprite-size")
                .short("s")
                .possible_values(&["8", "16"])
                .takes_value(true)
                .default_value("8")
                .help("desired sprite size, affecting the arrangement of the generated tiles")
            )
            .arg(Arg::with_name("COLOR_PALETTE")
                .long("color-palette")
                .short("p")
                .min_values(4)
                .max_values(4)
                .use_delimiter(true)
                .validator(is_hex_color)
                .takes_value(true)
                .default_value("000000,404040,808080,ffffff")
                .help("palette used for mapping colors to GameBoy palette indicies")
            )
        )
        .subcommand(SubCommand::with_name("animation")
            .about("converts gif files into a custom GameBoy animation format")
            .author("Ivo Wetzel <ivo.wetzel@googlemail.com>")
            .version("0.1")
            .arg(Arg::with_name("IMAGE_FILE")
                .help("Input gif file")
                .required(true)
                .index(1)
            )
            .arg(Arg::with_name("OUTPUT_FILE")
                .long("out-file")
                .short("o")
                .takes_value(true)
                .help("GameBoy animation file to generate")
            )
            .arg(Arg::with_name("COLOR_PALETTE")
                .long("color-palette")
                .short("p")
                .min_values(4)
                .max_values(4)
                .use_delimiter(true)
                .validator(is_hex_color)
                .takes_value(true)
                .default_value("000000,404040,808080,ffffff")
                .help("palette used for mapping colors to GameBoy palette indicies")
            )
        )
        .subcommand(SubCommand::with_name("mmp")
            .about("converts LMMS projects into a custom GameBoy music format")
            .author("Ivo Wetzel <ivo.wetzel@googlemail.com>")
            .version("0.1")
            .arg(Arg::with_name("MMP_FILE")
                .help("Input lmms project file")
                .required(true)
                .multiple(true)
                .index(1)
            )
            .arg(Arg::with_name("OUTPUT_FILE")
                .long("out-file")
                .short("o")
                .takes_value(true)
                .help("GameBoy music data to generate")
            )
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("tiles") {
        if let Err(err) = tiles::convert(
            PathBuf::from(matches.value_of("IMAGE_FILE").unwrap()),
            matches.value_of("SPRITE_SIZE").unwrap(),
            matches.values_of("COLOR_PALETTE").unwrap().collect(),
            matches.value_of("OUTPUT_FILE").map(|f| PathBuf::from(f))

        ) {
            eprintln!("       {} {}", "Error".bright_red(), err);
            process::exit(1)
        }

    } else if let Some(matches) = matches.subcommand_matches("animation") {
        if let Err(err) = animation::convert(
            PathBuf::from(matches.value_of("IMAGE_FILE").unwrap()),
            matches.values_of("COLOR_PALETTE").unwrap().collect(),
            matches.value_of("OUTPUT_FILE").map(|f| PathBuf::from(f))

        ) {
            eprintln!("       {} {}", "Error".bright_red(), err);
            process::exit(1)
        }

    } else if let Some(matches) = matches.subcommand_matches("mmp") {
        if let Err(err) = mmp::convert(
            matches.values_of("MMP_FILE").unwrap().into_iter().map(PathBuf::from).collect(),
            matches.value_of("OUTPUT_FILE").map(|f| PathBuf::from(f))

        ) {
            eprintln!("       {} {}", "Error".bright_red(), err);
            process::exit(1)
        }
    }

}


// Helpers --------------------------------------------------------------------
fn is_hex_color(color: String) -> Result<(), String> {
    if color.len() == 6 && color.chars().all(|p| match p {
        '0'..='9' | 'a'..='f' | 'A'..='F' => true,
        _ => false
    }) {
        Ok(())

    } else {
        Err(format!("Palette color \"{}\" must be a 24-bit hex value e.g. 808080", color))
    }
}

