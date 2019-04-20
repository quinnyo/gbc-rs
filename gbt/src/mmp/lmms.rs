// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::f32;

// External Dependencies ------------------------------------------------------
use serde::{de, Deserialize, de::Visitor};


// LMMS XML Structs -----------------------------------------------------------
#[derive(Deserialize, Debug)]
pub struct Project {
    #[serde(rename="type")]
    pub typ: String,
    pub head: Head,
    pub song: Song
}

impl Project {
    pub fn from_string(text: String) -> Result<Self, String> {
        serde_xml_rs::from_reader(text.as_bytes()).map_err(|e| {
            format!("Failed to load LMMS project file: {}", e)
        })
    }
}

#[derive(Deserialize, Debug)]
pub struct Head {
    #[serde(rename="mastervol")]
    pub master_volume: f32,
    pub bpm: i32,
    #[serde(rename="masterpitch")]
    pub master_pitch: i32
}

#[derive(Deserialize, Debug)]
pub struct Song {
    #[serde(rename="trackcontainer")]
    pub track_container: TrackContainer,
    pub timeline: Timeline
}

#[derive(Deserialize, Debug)]
pub struct Timeline {
    #[serde(rename="lpstate", deserialize_with="deserialize_number_as_bool")]
    pub is_looping: bool,
    #[serde(rename="lp0pos")]
    pub loop_start: usize,
    #[serde(rename="lp1pos")]
    pub loop_end: usize
}

#[derive(Deserialize, Debug)]
pub struct TrackContainer {
    #[serde(rename="type")]
    pub typ: String,
    #[serde(rename="track")]
    pub tracks: Vec<Track>
}

#[derive(Deserialize, Debug)]
pub struct Track {
    #[serde(rename="type")]
    pub typ: String,
    pub name: String,
    #[serde(rename="muted", deserialize_with="deserialize_number_as_bool")]
    pub is_muted: bool,
    #[serde(rename="solo", deserialize_with="deserialize_number_as_bool")]
    pub is_solo: bool,
    #[serde(rename="instrumenttrack")]
    pub instrument_track: InstrumentTrack,
    #[serde(rename="pattern")]
    pub patterns: Vec<Pattern>
}

#[derive(Deserialize, Debug)]
pub struct InstrumentTrack {
    #[serde(rename="vol")]
    pub volume: f32,
    pub pitch: i32,
    pub pan: f32,
    #[serde(rename="pitchrange")]
    pub pitch_range: i32,
    #[serde(rename="basenote")]
    pub base_note: usize,
    #[serde(rename="usemasterpitch", deserialize_with="deserialize_number_as_bool")]
    pub use_master_pitch: bool,
    pub instrument: Instrument
}

#[derive(Deserialize, Debug)]
pub struct Instrument {
    pub name: String,
    pub papu: Option<Papu>
}

#[derive(Deserialize, Debug)]
pub struct Papu {
    pub so1vol: u8,
    pub so2vol: u8,
    #[serde(rename="Bass")]
    pub bass: u32,
    #[serde(rename="Treble")]
    pub treble: i32,

    pub ch1so1: u8,
    pub ch1so2: u8,
    pub ch1vol: u8,
    pub ch1ssl: u8,
    pub ch1wpd: u8,
    pub ch1vsd: u8,
    pub srs: u8,
    pub sd: u8,
    pub st: u8,

    pub ch2so1: u8,
    pub ch2so2: u8,
    pub ch2vol: u8,
    pub ch2ssl: u8,
    pub ch2wpd: u8,
    pub ch2vsd: u8,

    pub ch3so1: u8,
    pub ch3so2: u8,
    pub ch3vol: u8,
    pub srw: u8,

    pub ch4so1: u8,
    pub ch4so2: u8,
    pub ch4vol: u8,
    pub ch4ssl: u8,
    pub ch4vsd: u8,
    #[serde(rename="sampleShape", deserialize_with="deserialize_sample_shape")]
    pub samples: Vec<u8>
}

#[derive(Deserialize, Debug)]
pub struct Pattern {
    pub pos: usize,
    #[serde(rename="note")]
    pub notes: Vec<Note>
}

#[derive(Deserialize, Debug)]
pub struct Note {
    #[serde(rename="vol")]
    pub volume: f32,
    pub pan: f32,
    pub key: usize,
    pub pos: usize,
    pub len: usize,
}


// Helpers --------------------------------------------------------------------
fn deserialize_number_as_bool<'de, D>(deserializer: D) -> Result<bool, D::Error> where D: de::Deserializer<'de> {
    struct BoolVisitor;
    impl<'de> Visitor<'de> for BoolVisitor {
        type Value = bool;
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a boolean value")
        }
        fn visit_i32<E>(self, value: i32) -> Result<Self::Value, E> where E: de::Error {
            Ok(i32::from(value) != 0)
        }
    }
    deserializer.deserialize_i32(BoolVisitor)
}

fn deserialize_sample_shape<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error> where D: de::Deserializer<'de> {
    struct SampleShapeVisitor;
    impl<'de> Visitor<'de> for SampleShapeVisitor {
        type Value = Vec<u8>;
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a base64 sample shape value")
        }
        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> where E: de::Error {
            if let Ok(bytes) = base64::decode(value) {
                let tmp: Vec<u32> = bytes.chunks(4).into_iter().map(|c| {
                    let b = (c[3] as u32) << 24 | (c[2] as u32) << 16 | (c[1] as u32) << 8 | (c[0] as u32);
                    f32::from_bits(b) as u32

                }).collect();
                Ok(tmp.chunks(2).into_iter().map(|c| {
                    (c[0] << 4 | c[1]) as u8

                }).collect())

            } else {
                Ok(vec![])
            }
        }
    }
    deserializer.deserialize_seq(SampleShapeVisitor)
}

