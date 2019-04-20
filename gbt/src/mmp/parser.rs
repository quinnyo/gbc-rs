// STD Dependencies -----------------------------------------------------------
use std::collections::{HashMap, HashSet};


// Internal Dependencies ------------------------------------------------------
use super::{lmms, command::{Command, NoteCommand}};


// Statics --------------------------------------------------------------------
const NOTE_DURATION: f32 = 0.0125;
const CUTOFF_FREQUENCY: f32 = 2015.0;


// MMP Parser -----------------------------------------------------------------
#[derive(Debug)]
pub struct Parser {
    instruments: HashMap<String, (usize, Instrument)>,
    tracks: Vec<CommandTrack>
}

impl Parser {

    pub fn from_project(project: lmms::Project) -> Result<Parser, String> {

        if project.head.master_pitch != 0 {
            return Err("Project MasterPitch must be 0".to_string());
        }

        let note_duration = NOTE_DURATION * (100.0 / project.head.bpm as f32);
        let timeline = project.song.timeline;

        // Keep Track of Instrument Data
        let mut used_notes = HashSet::new();
        let mut used_instruments = HashMap::new();

        // Parse individual tracks and combine them based on a naming pattern
        let mut combined_tracks: HashMap<String, (String, Vec<ParserTrack>)> = HashMap::new();
        for track in project.song.track_container.tracks.into_iter().filter(|t| !t.is_muted) {
            let t = ParserTrack::try_from(track)?;
            let key = t.name.split("_").take(3).map(|s| s.to_string()).collect::<Vec<String>>().join("_");
            let entry = combined_tracks.entry(key).or_insert_with(|| {
                (t.name.clone(), vec![])
            });
            entry.1.push(t);
        }

        // Combine tracks based on naming pattern
        let mut tracks = Vec::new();
        for track_list in combined_tracks.values() {

            // Parse Track Note Commands
            let mut commands: Vec<Command> = Vec::new();
            for track in &track_list.1 {

                // Get instrument index
                let hash = track.instrument.hash();
                let instrument_count = used_instruments.len();
                let entry = used_instruments.entry(hash).or_insert_with(|| {
                    (instrument_count, track.instrument.clone())
                });

                // Convert Track Notes into Commands
                let instrument_index = entry.0;
                for note in &track.notes {
                    used_notes.insert(note.key);
                    commands.append(&mut Self::commands_from_note(
                        note,
                        &track.instrument,
                        instrument_index,
                        note_duration
                    ));
                }

            }

            // Insert Loop Commands
            if timeline.is_looping {
                commands.push(Command::LoopMarker {
                    name: "loop_0".to_string(),
                    offset: timeline.loop_start
                });
                commands.push(Command::LoopJump {
                    name: "loop_0".to_string(),
                    offset: timeline.loop_end
                });
            }

            commands.sort_by(Command::sort_by);

            // Insert Stop Command
            if !timeline.is_looping {
                let offset = if let Some(command) = commands.last() {
                    command.end()

                } else {
                    0
                };
                commands.push(Command::Stop {
                    offset
                });
            }

            // Insert waits between commands
            let mut wait_commands = Vec::new();
            let mut accumulated_error_frames = 0.0;

            let mut previous: Option<&mut Command> = None;
            for command in &mut commands {
                wait_commands.append(&mut command.calculate_waits(
                    previous,
                    note_duration,
                    &mut accumulated_error_frames
                ));
                previous = Some(command);
            }

            // Merge and re-sort all commands
            commands.append(&mut wait_commands);
            commands.sort_by(Command::sort_by);

            // When looping cutoff commands after loop point
            if timeline.is_looping {
                /*
                TODO
                let loopIndex = -1;
                commands.forEach((c, i) => {
                    if (c instanceof LoopJumpCommand) {
                        loopIndex = i;
                    }
                });
                commands.length = loopIndex + 1;
                */
            }

            Parser::check_command_overlap(&track_list.0, &commands, 1)?;
            Parser::check_command_overlap(&track_list.0, &commands, 2)?;
            Parser::check_command_overlap(&track_list.0, &commands, 3)?;
            Parser::check_command_overlap(&track_list.0, &commands, 4)?;

            println!("{:?}", commands);
            tracks.push(CommandTrack {
                name: track_list.0.clone(),
                commands
            });

        }

        // println!("{:#?} {}", used_instruments, used_instruments.len());
        Ok(Self {
            instruments: used_instruments,
            tracks
        })

    }

    fn commands_from_note(
        note: &ParserNote,
        instrument: &Instrument,
        instrument_index: usize,
        note_duration: f32

    ) -> Vec<Command> {

        let command = NoteCommand {
            wait_frames: 0,
            flags: 0, // TODO set to if stops 128
            priority_frames: 0,
            channel: instrument.channel(),
            instrument: instrument_index,
            length: 0,
            offset: note.offset,
            end: note.offset + note.length
        };

        // Channel Specific data
        let frequency = note.freq;
        let (key, frequency_shift, frequency_divisor) = if command.channel != 4 {
            // Channel 1-3
            (note.key, 0, 0)

        } else {
            // Noise only
            let (shift, divisor) = note.freq_shift_divisor();
            (0, shift, divisor)
        };

        /*
        // Calculate playback length
        // Add one additional frame here to compensate for LMMS playback
        // differences
        const noteSeconds = (note.length * noteDuration) + FRAME_DURATION;
        const lengthVolumeLimit = instrument.lengthSeconds !== -1.0;

        const sweepSeconds = instrument.getSweepLength(this.key);
        const sweepVolumentLimit = sweepSeconds !== -1.0;

        let activeSeconds = lengthVolumeLimit ? instrument.lengthSeconds : noteSeconds;
        if (sweepVolumentLimit && sweepSeconds < activeSeconds) {
            activeSeconds = sweepSeconds;
        }

        if (!lengthVolumeLimit) {
            // PCM channel length counter
            if (instrument.channel === 3 && activeSeconds <= 1.00) {
                this.lengthFrames = Math.ceil(activeSeconds / FRAME_DURATION);
                this.lengthCounter = 256 - Math.floor(activeSeconds / (1 / 256));
                this.stops = true;

            // Other channels wave counter
            } else if (activeSeconds < 0.25) {
                this.lengthFrames = Math.ceil(activeSeconds / FRAME_DURATION);
                this.lengthCounter = 64 - Math.floor(activeSeconds / (1 / 256));
                this.stops = true;

            // Too long, requires a silence command
            } else {
                this.lengthFrames = Math.min(Math.ceil(activeSeconds / FRAME_DURATION), 255);
                if (instrument.channel === 3) {
                    this.lengthCounter = 0;

                } else {
                    this.lengthCounter = 0;
                }
                this.stops = false;
            }

        // If specified length is shorter than fade we need to manually stop
        } else if (noteSeconds < activeSeconds - FRAME_DURATION) {

            this.lengthFrames = Math.ceil(noteSeconds / FRAME_DURATION);

            if (noteSeconds < 0.25) {
                this.lengthCounter = 64 - Math.floor(noteSeconds / (1 / 256));
                this.stops = true;
                //console.log('COUNTER', this.instrument.name, this.lengthCounter);

            } else {
                this.lengthCounter = 0;
                this.stops = false;
                //console.log('SILENCE', this.instrument.name, noteSeconds, activeSeconds, this.lengthFrames);
            }

        // Otherwise fade out will already complete in time
        } else {
            this.lengthFrames = Math.ceil(activeSeconds / FRAME_DURATION);
            this.stops = true;
            this.lengthCounter = 0;
            // console.log('FADE', this.instrument.name);
        }

        const commands = [];
        if (this.instrument.channel === 4) {
            commands.push(new NoiseCommand(this.start, this, this.stops));

        } else if (this.instrument.channel === 3) {
            commands.push(new PcmCommand(this.start, this, this.stops));

        } else{
            commands.push(new NoteCommand(this.start, this, this.stops));
        }

        if (!this.stops) {
            commands.push(new SilenceCommand(this.end, this));
        }

        commands
        */

        Vec::new()
    }

    fn check_command_overlap(name: &str, commands: &[Command], channel: u8) -> Result<(), String> {
        let mut previous: Option<&Command> = None;
        for command in commands {
            if command.channel() == channel {
                if command.is_note() || command.is_silence() {
                    if let Some(p) = previous {
                        if p.end() > command.offset() {
                            return Err(format!("Overlapping notes on channel {} @ {} in Track \"{}\"", channel, command.offset(), name));
                        }
                    }
                    previous = Some(command);
                }
            }
        }
        Ok(())
    }

    fn gb_frequency(key: i32) -> u32 {
        let f = Self::note_frequency(key);
        (2048 - ((4194304 / f) >> 5))
    }

    fn note_frequency(key: i32) -> u32 {
        let pitch = (key as i32 - 57) as f32 / 12.0;
        (440.0 * 2f32.powf(pitch)).round() as u32
    }

    fn note_key(hz: u32) -> i32 {
        let pitch_model = 0;
        let pitch = (hz as f32 / 440.0).log2() - pitch_model as f32 * (100.0 / 12.0);
        ((pitch * 12.0) + 57.0).round() as i32
    }
}

#[derive(Debug)]
pub struct CommandTrack {
    name: String,
    commands: Vec<Command>
}

#[derive(Debug)]
pub struct ParserTrack {
    name: String,
    use_master_pitch: bool,
    pitch_model: i32,
    base_note: usize,
    instrument: Instrument,
    notes: Vec<ParserNote>
}

impl ParserTrack {
    fn try_from(t: lmms::Track) -> Result<Self, String> {

        if t.instrument_track.base_note != 57 {
            return Err(format!("BaseNote must be 57 for Track {}", t.name));
        }

        let mut track = ParserTrack {
            name: t.name.clone(),
            use_master_pitch: t.instrument_track.use_master_pitch,
            pitch_model: t.instrument_track.pitch,
            base_note: t.instrument_track.base_note,
            instrument: Instrument::try_from(t.name, t.instrument_track.instrument)?,
            notes: Vec::new()
        };

        for p in t.patterns {
            for n in p.notes {
                let freq = Parser::note_frequency(n.key as i32);
                let key = Parser::note_key(freq);
                // TODO
                // if (key < 25 || key > 95) && track.instrument.channel() != 4 {
                //     return Err(format!("Notes for Channels 1-3 must be within of range C2 to C7"));
                // }
                track.notes.push(ParserNote {
                    offset: p.pos + n.pos,
                    length: n.len,
                    freq,
                    key
                })
            }
        }

        track.notes.sort_by(|a, b| {
            a.offset.cmp(&b.offset)
        });

        Ok(track)
    }
}

#[derive(Debug)]
pub struct ParserNote {
    offset: usize,
    length: usize,
    freq: u32,
    key: i32
}

impl ParserNote {
    fn freq_shift_divisor(&self) -> (u32, u32) {
        let mut shift = 0;
        let mut divisor = 1;
        let mut optimal_freq = 524288.0 / (divisor as f32 * 2f32.powf(shift as f32 + 1.0));
        for s in 0..16 {
            for r in 0..8 {
                let target_freq = 524288.0 / (r as f32 * 2f32.powf(s as f32 + 1.0));
                if (self.freq as f32 - optimal_freq).abs() > (self.freq as f32 - target_freq).abs() {
                    optimal_freq = target_freq;
                    divisor = r;
                    shift = s;
                }
            }
        }
        /*
        TODO
        if (shift === 14 || shift === 15) {
            console.warn('LSFR will receive no clock!');
        }*/
        (shift, divisor)
    }
}

#[derive(Debug, Clone)]
pub enum Instrument {
    Square1 {
        name: String,
        envelope: Envelope,
        sweep: Sweep,
        duty_cycle: u8
    },
    Square2 {
        name: String,
        envelope: Envelope,
        duty_cycle: u8
    },
    PCM {
        name: String,
        volume: u8,
        samples: Vec<u8>
    },
    Noise {
        name: String,
        envelope: Envelope,
        shift_register_width: u8
    }
}

impl Instrument {
    fn try_from(name: String, i: lmms::Instrument) -> Result<Self, String> {
        if let Some(ref p) = i.papu {
            let ch1 = p.ch1so1 + p.ch1so2;
            let ch2 = p.ch2so1 + p.ch2so2;
            let ch3 = p.ch3so1 + p.ch3so2;
            let ch4 = p.ch4so1 + p.ch4so2;

            if ch1 % 2 != 0 || ch2 % 2 != 0 || ch3 % 2 != 0 || ch4 % 2 != 0 {
                Err(format!("Invalid instrument \"{}\": Non-Stereo channel configuration found", name))

            } else if ch1 + ch2 + ch3 + ch4 != 2 {
                Err(format!("Invalid instrument \"{}\": More than one channel is active", name))

            } else if ch1 == 2 {
                Self::parse_square1(&name, p)

            } else if ch2 == 2 {
                Self::parse_square2(&name, p)

            } else if ch3 == 2 {
                Self::parse_pcm(&name, p)

            } else if ch4 == 2 {
                Self::parse_noise(&name, p)

            } else {
                Err(format!("Invalid instrument \"{}\": No active channel", name))
            }

        } else {
            Err(format!("Invalid instrument \"{}\": Missing PAPU", name))
        }
    }

    fn parse_square1(name: &str, p: &lmms::Papu) -> Result<Self, String> {
        Ok(Instrument::Square1 {
            name: name.to_string(),
            envelope: Envelope::new(p.ch1vol, p.ch1ssl, p.ch1vsd),
            sweep: Sweep {
                shift: p.srs,
                time: if p.srs == 0 { 0 } else { p.st },
                direction: if p.srs == 0 { 0 } else { p.sd }
            },
            duty_cycle: p.ch1wpd
        })
    }

    fn parse_square2(name: &str, p: &lmms::Papu) -> Result<Self, String> {
        Ok(Instrument::Square2 {
            name: name.to_string(),
            envelope: Envelope::new(p.ch2vol, p.ch2ssl, p.ch2vsd),
            duty_cycle: p.ch2wpd
        })
    }

    fn parse_pcm(name: &str, p: &lmms::Papu) -> Result<Self, String> {
        Ok(Instrument::PCM {
            name: name.to_string(),
            volume: p.ch3vol,
            samples: p.samples.clone()
        })
    }

    fn parse_noise(name: &str, p: &lmms::Papu) -> Result<Self, String> {
        Ok(Instrument::Noise {
            name: name.to_string(),
            envelope: Envelope::new(p.ch4vol, p.ch4ssl, p.ch4vsd),
            shift_register_width: p.srw
        })
    }

    fn channel(&self) -> u8 {
        match self {
            Instrument::Square1 { .. } => 1,
            Instrument::Square2 { .. } => 2,
            Instrument::PCM { .. } => 3,
            Instrument::Noise { .. } => 4
        }
    }

    fn envelope(&self) -> Option<&Envelope> {
        match self {
            Instrument::Square1 { envelope, .. } => Some(envelope),
            Instrument::Square2 { envelope, .. } => Some(envelope),
            Instrument::PCM { .. } => None,
            Instrument::Noise { envelope, .. } => Some(envelope)
        }
    }

    fn sweep_length(&self, key: usize) -> f32 {
        match self {
            Instrument::Square1 { sweep, .. } => sweep.length_seconds(key),
            Instrument::Square2 { .. } => 0.0,
            Instrument::PCM { .. } => 0.0,
            Instrument::Noise { .. } => 0.0
        }
    }

    fn length_seconds(&self, key: usize) -> Option<f32> {
        if let Some(e) = self.envelope() {
            e.length_seconds

        } else {
            None
        }
    }

    fn hash(&self) -> String {
        let hash = format!("channel_{}_{}", self.channel(), self.envelope().map(|e| e.hash()).unwrap_or_else(|| "".to_string()));
        match self {
            Instrument::Square1 { sweep, duty_cycle, .. } => {
                format!("{}_{}_duty_cycle_{}", hash, sweep.hash(), duty_cycle)
            },
            Instrument::Square2 { duty_cycle, .. } => {
                format!("{}_duty_cycle_{}", hash, duty_cycle)
            }
            Instrument::PCM { volume, samples, .. } => {
                format!("{}_volume_{}_samples_{}", hash, volume, samples.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(","))
            },
            Instrument::Noise { shift_register_width, .. } => {
                format!("{}_shift_register_width_{}", hash, shift_register_width)
            }
        }
    }

    fn to_name(&self) -> String {
        match self {
            Instrument::Square1 { name, .. }
            | Instrument::Square2 { name, .. }
            | Instrument::Noise { name, .. }
            | Instrument::PCM { name, .. } => {
                format!("instrument_{}", name)
            }
        }
    }

}

#[derive(Debug, Clone)]
pub struct Envelope {
    volume: u8,
    step: u8,
    direction: u8,
    length_seconds: Option<f32>
}

impl Envelope {
    fn new(volume: u8, step: u8, direction: u8) -> Self {
        let fade_duration = volume as f32 * (step as f32 * (1.0 / 64.0));
        Self {
            volume,
            step,
            direction,
            length_seconds: if direction != 0 || fade_duration == 0.0 {
                None

            } else {
                Some(fade_duration)
            }
        }
    }

    fn hash(&self) -> String {
        format!("envelope_{}_{}_{}", self.volume, self.step, self.direction)
    }

}

#[derive(Debug, Clone)]
pub struct Sweep {
    shift: u8,
    time: u8,
    direction: u8
}

impl Sweep {

    fn hash(&self) -> String {
        format!("sweep_{}_{}_{}", self.shift, self.time, self.direction)
    }

    fn length_seconds(&self, key: i32) -> f32 {
        let mut lx = Parser::gb_frequency(key) as f32;
        let mut t = 0.0f32;
        for _ in 0..20 {
            if self.direction == 1 {
                lx = lx - lx / 2f32.powf(self.shift as f32);

            } else {
                lx = lx + lx / 2f32.powf(self.shift as f32);
            }

            t += self.time as f32 * 7.8168;

            // Audio circuit cutoff point
            if lx > CUTOFF_FREQUENCY {
                break;
            }

        }
        t * 0.001
    }
}

