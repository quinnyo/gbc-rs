// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::collections::{HashMap, HashSet};


// Internal Dependencies ------------------------------------------------------
use super::util;

mod command;
mod lmms;
mod parser;
use command::Command;
use parser::{CommandTrack, Instrument, Parser};


// Structs --------------------------------------------------------------------
struct MMP {
    notes: HashSet<usize>,
    instruments: HashMap<String, (usize, Instrument)>,
    tracks: Vec<CommandTrack>
}

impl MMP {

    fn new() -> Self {
        Self {
            notes: HashSet::new(),
            instruments: HashMap::new(),
            tracks: Vec::new()
        }
    }

    fn add_project(&mut self, project: lmms::Project) -> Result<(), String> {
        let mut tracks = Parser::from_project(
            project,
            &mut self.instruments,
            &mut self.notes
        )?;
        self.tracks.append(&mut tracks);
        Ok(())
    }

    fn to_string(&self) -> String {

        let mut unique_notes: Vec<usize> = self.notes.clone().drain().collect();
        unique_notes.sort();

        let mut instruments: Vec<(usize, Instrument)> = self.instruments.values().cloned().collect();
        instruments.sort_by(|a, b| a.0.cmp(&b.0));

        let instruments: Vec<Instrument> = instruments.into_iter().map(|i| i.1).collect();
        let instrument_index: Vec<String> = instruments.iter().map(|i| {
            format!("    BW Instrument_{}", i.name())

        }).collect();

        let frequeny_table: Vec<String> = unique_notes.iter().map(|key| {
            let f = Parser::note_frequency(*key);
            let v = 2048 - ((4194304 / f) >> 5);
            format!("{}; {}hz", v, f)

        }).collect();

        let tracks: Vec<String> = self.tracks.iter().map(|track| {
            Self::track_to_string(&track, &unique_notes, &instruments)

        }).collect();

        let instruments: Vec<String> = instruments.iter().map(Self::instrument_to_string).collect();

        format!(r#"; MMP Frequency Table ---------------------------------------------------------
mmp_frequency_table:
    DW {}

; MMP Instrument Table --------------------------------------------------------
mmp_player_instrument_index:
{}

{}

; MMP Song Table --------------------------------------------------------------
{}"#,
            frequeny_table.join("\n    DW "),
            instrument_index.join("\n"),
            instruments.join(""),
            tracks.join("")
        )
    }

    fn instrument_to_string(instrument: &Instrument) -> String {
        match instrument {
            Instrument::Square1 { name, .. } => {
                format!(r#"; Instrument
instrument_{}:
    ; Channel (SQ1)

"#, name)
            },
            Instrument::Square2 { name, .. } => {
                format!(r#"; Instrument
instrument_{}:
    ; Channel (SQ2)

"#, name)
            },
            Instrument::PCM { name, .. } => {
                format!(r#"; Instrument
instrument_{}:
    ; Channel (PCM)

"#, name)
            },
            Instrument::Noise { name, .. } => {
                format!(r#"; Instrument
instrument_{}:
    ; Channel (Noise)

"#, name)
            }
        }
    }

    fn track_to_string(track: &CommandTrack, unique_notes: &[usize], instruments: &[Instrument]) -> String {
        let commands: Vec<String> = track.commands.iter().map(|c| Self::command_to_string(c, unique_notes, instruments)).collect();
        format!("mmp_track_{}:\n{}", track.name, commands.join(""))
    }

    fn command_to_string(command: &Command, unique_notes: &[usize], instruments: &[Instrument]) -> String {
        fn note_index(unique_notes: &[usize], key: &usize) -> usize {
            unique_notes.iter().position(|k| k == key).expect("Note Index not found")
        }

        match command {
            Command::LoopMarker { name, ..  } => {
                format!(".marker_{}", name)
            },

            Command::LoopJump { name, ..  } => {
                format!(r#"
    ; Loop Jump
    DB      $03, .marker_{} >> 8, .marker_{} & $FF
"#,
                    name,
                    name
                )
            },

            Command::Silence { channel, .. } => {
                let offset = (channel - 1) * 8;
                format!(r#"
    ; Silence / Channel Table Offset
    DB      ${:0>2X}
"#,
                    (offset << 2) | 1
                )
            },

            Command::Wait { frames, .. } => {
                format!(r#"
    ; Wait / Frames ({})
    DB      ${:0>2X}
"#,
                    frames,
                    (frames << 2) as u8 | 2
                )
            },

            Command::Stop { .. } => {
                format!(r#"
    ; Stop
    DB      ${:0>2X}
"#,
                    128 | 3
                )
            },

            Command::Note { note, key, frequency, .. } => {
                let instrument = &instruments[note.instrument];
                let key = note_index(unique_notes, key);
                let flags = if note.stops { 0 } else { 128 } | (note.wait_frames << 2);
                format!(r#"
    ; Flags({}), Instrument({}) / Priority Frames  / DutyCycle & Length Counter / Frequency ({}hz)
    DB      ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}
"#,
                    note.wait_frames,
                    instrument.name(),
                    frequency,

                    flags,
                    note.instrument,
                    note.priority_frames,
                    (instrument.duty_cycle() << 6) | note.length_counter as u8,
                    key * 2
                )
            },

            Command::PCM { note, key, frequency, .. } => {
                let instrument = &instruments[note.instrument];
                let key = note_index(unique_notes, key);
                let flags = if note.stops { 0 } else { 128 } | (note.wait_frames << 2);
                format!(r#"
    ; Flags({}), Instrument({}) / Priority Frames  / Length Counter / Frequency ({}hz)
    DB      ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}
"#,
                    note.wait_frames,
                    instrument.name(),
                    frequency,

                    flags,
                    note.instrument,
                    note.priority_frames,
                    note.length_counter as u8,
                    key * 2
                )
            },

            Command::Noise { note, frequency_shift, frequency_divisor } => {
                let instrument = &instruments[note.instrument];
                let flags = if note.stops { 0 } else { 128 } | (note.wait_frames << 2);
                let srw = instrument.shift_register_width();
                format!(r#"
    ; Flags({}), Instrument({}) / Priority Frames  / Length Counter / Frequency Shift({}) & Shift Register Width({}) & Divisor({})
    DB      ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}, ${:0>2X}
"#,
                    note.wait_frames,
                    instrument.name(),
                    frequency_shift,
                    if srw == 1 { 7 } else { 15 },
                    frequency_divisor,

                    flags,
                    note.instrument,
                    note.priority_frames,
                    note.length_counter as u8,
                    (frequency_shift << 4) | (srw << 3) | frequency_divisor
                )
            },
        }
    }
    /*

class NoiseCommand extends Command {

    serialize() {
        return `
    ; Flags(${this.waitFrames}), Instrument(${this.instrument.name}) / Priority Frames  / Length Counter / Frequency Shift(${this.frequencyShift}) & Shift Register Width(${this.instrument.shiftRegisterWidth ? 7 : 15}) & Divisor(${this.frequencyDivisor})
    DB      ${toHex(this.flags | (this.waitFrames << 2))}, ${toHex(this.instrument.index)}, ${toHex(this.priorityFrames)}, ${toHex(this.length)}, ${toHex((this.frequencyShift << 4) | (this.instrument.shiftRegisterWidth << 3) | this.frequencyDivisor)}
`;
    }

}



    fn to_string(&self) -> String {
        match self {
            ParserInstrument::Square1 { .. } => {
                /*
                   TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (SQ1)
    DB      ${toHex(this.channel - 1)}

    ; Sweep(time=${this.sweepTime} direction=${this.sweepDirection} shift=${this.sweepShift})
    DB      ${toHex((this.sweepTime << 4) | (this.sweepDirection << 3) | this.sweepShift)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume  << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;*/

                format!("")
            },
            ParserInstrument::Square2 { .. } => {
                /*
                 TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (SQ2)
    DB      ${toHex(this.channel - 1)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;*/

                format!("")
            }
            ParserInstrument::PCM { .. } => {
            /*
               TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (PCM)
    DB      ${toHex(this.channel - 1)}

    ; Output Level (${toOutputLevel(this.volume)})
    DB      ${toHex(this.volume << 5)}

    ; Samples
    DB      ${this.samples.map(toHex).join(', ')}
`;
*/
                format!("")
            },
            ParserInstrument::Noise { .. } => {
                /*
            TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (Noise)
    DB      ${toHex(this.channel - 1)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;
                 */
                format!("")
            }
        }
    }

    fn to_string(&self) -> String {
        match self {
            ParserInstrument::Square1 { .. } => {
                /*
                   TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (SQ1)
    DB      ${toHex(this.channel - 1)}

    ; Sweep(time=${this.sweepTime} direction=${this.sweepDirection} shift=${this.sweepShift})
    DB      ${toHex((this.sweepTime << 4) | (this.sweepDirection << 3) | this.sweepShift)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume  << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;*/

                format!("")
            },
            ParserInstrument::Square2 { .. } => {
                /*
                 TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (SQ2)
    DB      ${toHex(this.channel - 1)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;*/

                format!("")
            }
            ParserInstrument::PCM { .. } => {
            /*
               TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (PCM)
    DB      ${toHex(this.channel - 1)}

    ; Output Level (${toOutputLevel(this.volume)})
    DB      ${toHex(this.volume << 5)}

    ; Samples
    DB      ${this.samples.map(toHex).join(', ')}
`;
*/
                format!("")
            },
            ParserInstrument::Noise { .. } => {
                /*
            TODO
        return `
; Instrument
${this.serializeName()}:
    ; Channel (Noise)
    DB      ${toHex(this.channel - 1)}

    ; Envelope(initial=${this.envelopeVolume} direction=${this.envelopeDirection} step=${this.envelopeStep})
    DB      ${toHex((this.envelopeVolume << 4) | (this.envelopeDirection << 3) | this.envelopeStep)}
`;
                 */
                format!("")
            }
        }
    }*/


}

/*
function toOutputLevel(d) {
    return {
        0: 'Silent',
        1: '100%',
        2: '50%',
        3: '25%'
    }[d];
}

function toDuty(d) {
    return {
        0: '12%',
        1: '25%',
        2: '50%',
        3: '75%'
    }[d];
}

function toHex(v) {
    const h = v.toString(16).toUpperCase();
    if (h.length === 1) {
        return `$0${h}`;

    } else {
        return `$${h}`;
    }
}

function toChannelName(c) {
    return {
        1: 'SQ1',
        2: 'SQ2',
        3: 'PCM',
        4: 'Noise'
    }[c];
}


*/

// GameBoy Music Converter ----------------------------------------------------
pub fn convert(
    mmp_files: Vec<PathBuf>,
    output_file: Option<PathBuf>

) -> Result<(), String> {
    let mut mmp = MMP::new();
    for file in mmp_files {
        let text = util::load_text(&file).map_err(|e| {
            format!("Failed to load LMMS project file: {}", e)
        })?;
        let project = lmms::Project::from_string(text).map_err(|e| {
            format!("Failed to parse LMMS project file: {}", e)
        })?;
        mmp.add_project(project).map_err(|e| {
            format!("Failed to parse LMMS project: {}", e)
        })?;
    }

    util::output_text(output_file, mmp.to_string())
}

