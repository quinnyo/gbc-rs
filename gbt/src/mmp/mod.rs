// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
mod command;
mod lmms;
mod parser;
use super::util;


// Structs --------------------------------------------------------------------
struct MMP {
    tracks: Vec<MMPTrack>,
    instruments: Vec<MMPInstrument>
}

impl MMP {

    fn new() -> Self {
        Self {
            instruments: Vec::new(),
            tracks: Vec::new()
        }
    }

    fn add_project(&mut self, project: lmms::Project) -> Result<(), String> {
        Self::parse_tracks(project, &mut self.instruments, &mut self.tracks)
    }

    fn parse_tracks(project: lmms::Project, instruments: &mut Vec<MMPInstrument>, tracks: &mut Vec<MMPTrack>) -> Result<(), String> {
        let parser = parser::Parser::from_project(project)?;
        Ok(())
    }

    /*
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

    serialize() {

        const uniqueKeys = Object.keys(this.keys).map((i) => +i);
        const instrumentIndex = this.instruments.map((i) => {
            return `    BW ${i.serializeName()}`;
        })

        const instruments = this.instruments.map((i) => i.serialize());
        const tracks = this.tracks.map((s) => s.serialize(uniqueKeys));

        const frequencyTable = uniqueKeys.map((i) => {
            const f = Math.round(MMP.getNoteFrequency(i));
            const v = (2048 - ((4194304 / f) >> 5));
            return `${v}; ${f}hz`;
        });

        return `; MMP Frequency Table ---------------------------------------------------------
mmp_frequency_table:\n    DW ${frequencyTable.join('\n    DW ')}

; MMP Instrument Table --------------------------------------------------------
mmp_player_instrument_index:
${instrumentIndex.join('\n')}
${instruments.join('')}

; MMP Song Table --------------------------------------------------------------
${tracks.join('')}`;
    }

class Track {

    constructor(name, commands) {
        this.name = name;
        this.commands = commands;
    }

    serializeName() {
        return `mmp_track_${this.name}:`;
    }

    serialize(uniqueKeys) {
        return `mmp_track_${this.name}:\n` + this.commands.map((c) => c.serialize(uniqueKeys)).join('');
    }

}

class LoopMarkerCommand extends Command {
    serialize() {
        return `.marker_${this.name}:`
    }
}

class SilenceCommand extends Command {

    serialize() {
        const offset = (this.channel - 1) * 8;
        return `
    ; Silence / Channel Table Offset
    DB      ${toHex((offset << 2) | 1)}
`;
    }

}

class NoteCommand extends Command {

    serialize(uniqueKeys) {
        const keyIndex = uniqueKeys.indexOf(this.key);
        return `
    ; Flags(${this.waitFrames}), Instrument(${this.instrument.name}) / Priority Frames  / DutyCycle & Length Counter / Frequency (${Math.floor(this.frequency)}hz)
    DB      ${toHex(this.flags | (this.waitFrames << 2))}, ${toHex(this.instrument.index)}, ${toHex(this.priorityFrames)}, ${toHex((this.instrument.dutyCycle << 6) | this.length)}, ${toHex(keyIndex * 2)}
`;
    }
}

class PcmCommand extends Command {

    serialize(uniqueKeys) {
        const keyIndex = uniqueKeys.indexOf(this.key);
        return `
    ; Flags(${this.waitFrames}), Instrument(${this.instrument.name}) / Priority Frames  / Length Counter / Frequency (${Math.floor(this.frequency)}hz)
    DB      ${toHex(this.flags | (this.waitFrames << 2))}, ${toHex(this.instrument.index)}, ${toHex(this.priorityFrames)}, ${toHex(this.length)}, ${toHex(keyIndex * 2)}
`;
    }

}

class NoiseCommand extends Command {

    serialize() {
        return `
    ; Flags(${this.waitFrames}), Instrument(${this.instrument.name}) / Priority Frames  / Length Counter / Frequency Shift(${this.frequencyShift}) & Shift Register Width(${this.instrument.shiftRegisterWidth ? 7 : 15}) & Divisor(${this.frequencyDivisor})
    DB      ${toHex(this.flags | (this.waitFrames << 2))}, ${toHex(this.instrument.index)}, ${toHex(this.priorityFrames)}, ${toHex(this.length)}, ${toHex((this.frequencyShift << 4) | (this.instrument.shiftRegisterWidth << 3) | this.frequencyDivisor)}
`;
    }

}

class LoopJumpCommand extends Command {

    serialize() {
        return `
    ; Loop Jump
    DB      $03, .marker_${this.name} >> 8, .marker_${this.name} & $FF
`;
    }

}

class WaitCommand extends Command {

    serialize() {
        return `
    ; Wait / Frames (${this.frames})
    DB      ${toHex((this.frames << 2) | 2)}
`;
    }

}

class StopCommand extends Command {

    serialize() {
        return `
    ; Stop
    DB      ${toHex(128 | 3)}
`;
    }

}

*/

struct MMPTrack {

}

struct MMPInstrument {

}

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

    util::output_text(output_file, "; Empty\n".to_string())
}

