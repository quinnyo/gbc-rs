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
    pub track_container: TrackContainer
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
    pub base_note: u32,
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
    so1vol: u8,
    so2vol: u8,
    #[serde(rename="Bass")]
    bass: u32,
    #[serde(rename="Treble")]
    treble: i32,

    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch1so1: bool,
    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch1so2: bool,
    ch1vol: u8,
    ch1ssl: u8,
    ch1wpd: u8,
    ch1vsd: u8,
    srs: u8,
    sd: u8,
    st: u8,

    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch2so1: bool,
    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch2so2: bool,
    ch2vol: u8,
    ch2ssl: u8,
    ch2wpd: u8,
    ch2vsd: u8,

    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch3so1: bool,
    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch3so2: bool,
    ch3vol: u8,
    srw: u8,

    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch4so1: bool,
    #[serde(deserialize_with="deserialize_number_as_bool")]
    ch4so2: bool,
    ch4vol: u8,
    ch4ssl: u8,
    ch4vsd: u8,
    #[serde(rename="sampleShape", deserialize_with="deserialize_sample_shape")]
    samples: Vec<u8>
    // <papu sampleShape="AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="/>
}

#[derive(Deserialize, Debug)]
pub struct Pattern {
    #[serde(rename="note")]
    pub notes: Vec<Note>
}

#[derive(Deserialize, Debug)]
pub struct Note {
    #[serde(rename="vol")]
    pub volume: f32,
    pub pan: f32,
    pub key: u32,
    pub pos: u32,
    pub len: u32,
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


