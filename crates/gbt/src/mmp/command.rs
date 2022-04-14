// STD Dependencies -----------------------------------------------------------
use std::cmp::Ordering;


// Statics --------------------------------------------------------------------
use super::parser::FRAME_DURATION;


// MMP Commands ---------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct NoteCommand {
    pub wait_frames: usize,
    pub stops: bool,
    pub channel: u8,
    pub priority_frames: usize,
    pub instrument: usize,
    pub length_counter: usize,
    pub offset: usize,
    pub end: usize
}

#[derive(Debug, Clone)]
pub enum Command {
    LoopMarker {
        name: String,
        offset: usize
    },
    LoopJump {
        name: String,
        offset: usize
    },
    Stop {
        offset: usize
    },
    Wait {
        offset: usize,
        frames: usize
    },
    Silence {
        offset: usize,
        end: usize,
        channel: u8
    },
    Note {
        note: NoteCommand,
        frequency: u32,
        key: usize
    },
    PCM {
        note: NoteCommand,
        frequency: u32,
        key: usize
    },
    Noise {
        note: NoteCommand,
        frequency_shift: u8,
        frequency_divisor: u8
    }
}

impl Command {

    pub fn channel(&self) -> u8 {
        match self {
            Command::LoopMarker { .. } |
            Command::LoopJump { .. } |
            Command::Wait { .. } |
            Command::Stop { .. } => {
                0
            },
            Command::Silence { channel, .. } => *channel,
            Command::Note { note, .. } |
            Command::PCM { note, .. } |
            Command::Noise { note, .. } => {
                note.channel
            }
        }
    }

    pub fn end(&self) -> usize {
        match self {
            Command::LoopMarker { offset, .. } |
            Command::LoopJump { offset, .. } |
            Command::Wait { offset, .. } |
            Command::Stop { offset, .. } => {
                *offset
            },
            Command::Silence { end, .. } => *end,
            Command::Note { note, .. } |
            Command::PCM { note, .. } |
            Command::Noise { note, .. } => {
                note.end
            }
        }
    }

    pub fn offset(&self) -> usize {
        match self {
            Command::LoopMarker { offset, .. } |
            Command::LoopJump { offset, .. } |
            Command::Wait { offset, .. } |
            Command::Stop { offset, .. } |
            Command::Silence { offset, .. } => {
                *offset
            },
            Command::Note { note, .. } |
            Command::PCM { note, .. } |
            Command::Noise { note, .. } => {
                note.offset
            }
        }
    }

    pub fn is_note(&self) -> bool {
        match self {
            Command::Note { .. } => true,
            Command::PCM { .. } => true,
            Command::Noise { .. } => true,
            _ => false
        }
    }

    pub fn is_silence(&self) -> bool {
        match self {
            Command::Silence { .. } => true,
            _ => false
        }
    }

    pub fn is_loop_jump(&self) -> bool {
        match self {
            Command::LoopJump { .. } => true,
            _ => false
        }
    }

    fn is_stop(&self) -> bool {
        match self {
            Command::Stop { .. } => true,
            _ => false
        }
    }

    pub fn calculate_waits(
        &self,
        previous: Option<&mut Command>,
        note_duration: f32,
        accumulated_error_frames: &mut f32

    ) -> Vec<Command> {

        // Calculate frames since last note entry
        let previous_start = previous.as_ref().map(|p| p.offset()).unwrap_or(0);
        let seconds_since_last = ((self.offset() - previous_start) as f32 * note_duration - FRAME_DURATION).max(0.0);

        // For very short notes we want to remove the wait frames altogether
        let mut second_frames = seconds_since_last / FRAME_DURATION;
        if previous.as_ref().map(|p| p.is_note()).unwrap_or(false) && seconds_since_last <= 0.1 && self.is_stop() {
            // Don't merge wait frames in case this is a stop note
            second_frames = 0.0;
        }

        // Merge up to 31 wait frames into a previous note / noise command
        let mut previous_frames = second_frames.min(31.0).ceil();
        if let Some(previous) = previous {
            previous.set_wait_frames(previous_frames as usize);

        } else {
            previous_frames = 0.0;
        }

        let frame_since_last_exact = second_frames - previous_frames;
        let mut frames_since_last = frame_since_last_exact.floor();

        // Distribute error across all commands
        *accumulated_error_frames += frame_since_last_exact - frames_since_last;

        if *accumulated_error_frames > 1.0 {
            frames_since_last += 1.0;
            *accumulated_error_frames -= 1.0;
        }

        // We limit the wait command to 255 frames
        let abs_frames_since_last = frames_since_last.max(0.0) as usize;
        let count = abs_frames_since_last / 64;
        let remainder = abs_frames_since_last % 64;

        // So we need to insert multiple ones if required, this simpliflies
        // playback logic
        let mut commands = Vec::new();
        for _ in 0..count {
            commands.push(Command::Wait {
                offset: previous_start as usize,
                frames: 63
            });
        }

        if remainder > 0 {
            commands.push(Command::Wait {
                offset: previous_start as usize,
                frames: remainder
            });
        }

        commands

    }

    pub fn sort_by(a: &Command, b: &Command) -> Ordering {
        let i = Self::sort(a, b);
        if i < 0 {
            Ordering::Less

        } else if i == 0 {
            Ordering::Equal

        } else {
            Ordering::Greater
        }
    }

    fn sort(a: &Command, b: &Command) -> i32 {
        let offset_diff = a.offset() as i32 - b.offset() as i32;
        if offset_diff == 0 {
            let order_diff = a.order() - b.order();
            if order_diff == 0 {
                a.order_second() - b.order_second()

            } else {
                order_diff
            }

        } else {
            offset_diff
        }
    }

    fn set_wait_frames(&mut self, frames: usize) {
        match self {
            Command::Note { note, .. } | Command::PCM { note, .. } | Command::Noise { note, .. } => {
                note.wait_frames = frames;
            },
            _ => {}
        }
    }

    fn order(&self) -> i32 {
        match self {
            Command::LoopMarker { .. } => 0,
            Command::LoopJump { .. } => 3,
            Command::Wait { .. } => 4,
            Command::Stop { .. } => 8,
            Command::Silence { .. } => 1,
            Command::Note { .. } => 2,
            Command::PCM { .. } => 2,
            Command::Noise { .. } => 2,
        }
    }

    fn order_second(&self) -> i32 {
        match self {
            Command::LoopMarker { .. } => 0,
            Command::LoopJump { .. } => 0,
            Command::Wait { .. } => 0,
            Command::Stop { .. } => 0,
            Command::Silence { .. } => 0,
            Command::Note { note, .. } => note.wait_frames as i32,
            Command::PCM { note, .. } => note.wait_frames as i32,
            Command::Noise { note, .. } => note.wait_frames as i32
        }
    }

}

