// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::Deserialize;


// Internal Dependencies ------------------------------------------------------
use crate::state::State;
use crate::types::DebuggerOutlineLocation;


// Types ----------------------------------------------------------------------
#[derive(Debug, Default, Eq, PartialEq, Deserialize, Clone)]
pub struct EmulatorAddressInfo {
    pub address: u16,
    pub bank: u16
}


// Emulator Status -----------------------------------------------------------
#[derive(Debug, Default, Deserialize, Clone)]
pub struct EmulatorRegisters {
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
}

#[derive(Debug, Default, Deserialize, Clone)]
pub struct EmulatorStatus {
    pub name: String,
    pub paused: bool,
    pub menu: bool,
    pub filename: String,
    pub crc: u32,

    pub title: String,
    pub stopped: bool,
    pub pc: u16,
    pub bank: u16,
    pub registers: EmulatorRegisters,
    pub ime: bool,
    pub cgb: bool,
    pub oam: Vec<u8>,
    pub io_registers: Vec<u8>,
    pub bg_palettes: Vec<u8>,
    pub obj_palettes: Vec<u8>,

    pub breakpoints: Vec<EmulatorAddressInfo>,
    pub backtrace: Vec<EmulatorAddressInfo>,
}

impl EmulatorStatus {
    pub(in crate::emulator) fn to_status_message(&self, does_rom_match: bool) -> String {
        let detail = self.to_status_detail(does_rom_match);
        if does_rom_match {
            format!("{} via {} [{}]", self.title, self.name, detail)

        } else {
            detail
        }
    }

    fn to_status_detail(&self, does_rom_match: bool) -> String {
        if !does_rom_match {
            format!("ROM path does not match")

        } else if self.stopped {
            format!("Stopped @ ${:0>4X}", self.pc)

        } else if self.menu {
            format!("In Menu")

        } else if self.paused {
            format!("Paused")

        } else {
            format!("Running")
        }
    }

    pub(in crate::emulator) fn to_outline(
        &self,
        data: &State,
        does_rom_match: bool

    ) -> (String, HashMap<usize, DebuggerOutlineLocation>) {

        let mut locations: HashMap<usize, DebuggerOutlineLocation> = HashMap::new();
        let mut outline = format!("Status: {}\n", self.to_status_detail(does_rom_match));
        if !does_rom_match {
            return (outline, locations);
        }
        outline.push_str(&format!("ROM: {} (emulated via {})\n", self.title, self.name));

        // TODO use bank correctly
        let mut resolve_symbol_with_offset = |address: u16, _bank: u16, line: usize| {
            let mut label_address = 0;
            let mut label_name = "Unknown".to_string();
            let mut location = None;

            // Find the closest labeled address
            for (addr, name, filename, line, character) in data.labels().iter() {
                if *addr > address {
                    break

                } else {
                    label_address = *addr;
                    label_name = name.clone();
                    location = Some(DebuggerOutlineLocation {
                        filename: filename.clone(),
                        line: *line,
                        character: *character
                    });
                }
            }

            // Use location from addresses if available otherwise fallback to closest matching label
            if let Some(location) = data.addresses().get(&(address as usize)) {
                locations.insert(line, DebuggerOutlineLocation {
                    filename: location.uri.path().to_string(),
                    line: location.range.start.line as usize,
                    character: location.range.start.character as usize
                });

            } else if let Some(location) = location {
                locations.insert(line, location);
            }

            let offset = address.saturating_sub(label_address);
            if offset > 0 {
                format!("{}+${:0>3X}", label_name, offset)

            } else {
                label_name.to_string()
            }

        };

        // Registers
        let line_offset = 4;
        let f = self.registers.af as u8;
        outline.push_str(&format!(
            "Registers:\n   AF = ${:0>4X} ({}{}{}{})\n   BC = ${:0>4X} ({})\n   DE = ${:0>4X} ({})\n   HL = ${:0>4X} ({})\n   SP = ${:0>4X} ({})\n   PC = ${:0>4X} ({})\n  IME = {}    WBNK = {} SPD = {}\n\nBacktrace:\n",
            self.registers.af,
            if (f & 16) == 16 { "C" } else { "-" },
            if (f & 32) == 32 { "H" } else { "-" },
            if (f & 64) == 64 { "N" } else { "-" },
            if (f & 128) == 128 { "Z" } else { "-" },
            self.registers.bc,
            resolve_symbol_with_offset(self.registers.bc, self.bank, line_offset),
            self.registers.de,
            resolve_symbol_with_offset(self.registers.de, self.bank, line_offset + 1),
            self.registers.hl,
            resolve_symbol_with_offset(self.registers.hl, self.bank, line_offset + 2),
            self.registers.sp,
            resolve_symbol_with_offset(self.registers.sp, self.bank, line_offset + 3),
            self.pc,
            resolve_symbol_with_offset(self.pc, self.bank, line_offset + 4),
            if self.ime { "On"} else { "Off"},
            self.io_registers[0x70] & 0b111,
            self.io_registers[0x4D] >> 7
        ));

        // Backtrace
        let breakpoint_offset = line_offset + 10 + self.backtrace.len();
        for (i, b) in self.backtrace.iter().enumerate() {
            outline.push_str(&format!(
                "  {: >2}. {} (${:0>2X} ${:0>4X})\n",
                i + 1,
                resolve_symbol_with_offset(b.address, b.bank, line_offset + 8 + i),
                b.bank,
                b.address
            ));
        }

        // Breakpoints
        outline.push_str("\nBreakpoints:\n");
        if self.breakpoints.is_empty() {
            outline.push_str("   -\n");

        } else {
            for (i, b) in self.breakpoints.iter().enumerate() {
                outline.push_str(&format!(
                    "  {: >2}. {} (${:0>2X} ${:0>4X})\n",
                    i + 1,
                    resolve_symbol_with_offset(b.address, b.bank, breakpoint_offset + i),
                    if b.bank == 0xFFFF { 0 } else { b.bank },
                    b.address
                ));
            }
        }

        outline.push_str("\nVRAM:\n");
        {
            let lcdc = self.io_registers[0x40];
            outline.push_str(
                &format!(
                    "  PPU = {}            PRIO = {}\n  WIN = {}            MAP = {}\n   BG = {}  TILES = {}\n  OBJ = {}            SIZE = {}\n",
                    if (lcdc & 0x80) != 0 { "On"} else { "Off" },
                    if (lcdc & 1) != 0 { "On"} else { "Off" },
                    if (lcdc & 0x20) != 0 { "On"} else { "Off" },
                    if (lcdc & 0x40) != 0 { "$9C00-$9FFF"} else { "$9800-$9BFF" },
                    if (lcdc & 8) != 0 { "$9C00-$9FFF"} else { "$9800-$9BFF" },
                    if (lcdc & 0x10) != 0 { "$8800-$97FF"} else { "$8000-$8FFF" },
                    if (lcdc & 2) != 0 { "On"} else { "Off" },
                    if (lcdc & 4) != 0 { "8x16"} else { "8x8" }
                )
            );
            let stat = self.io_registers[0x41];
            outline.push_str(
                &format!(
                    " BANK = {}             MODE = {}\n",
                    self.io_registers[0x4F] & 0x01,
                    if stat & 0b11 == 0 {
                        "HBlank"

                    } else if stat & 0b11 == 1 {
                        "VBlank"

                    } else if stat & 0b11 == 2 {
                        "Search"

                    } else {
                        "Transfer"
                    }
                )
            );
            outline.push_str(&format!("  SCX = {: >3} SCY = {: >3} LY = {: >3}\n", self.io_registers[0x43], self.io_registers[0x42], self.io_registers[0x44]));
        }

        outline.push_str("\nBGPalette:\n");
        if self.cgb {
            for (index, obj) in self.bg_palettes.chunks(8).enumerate() {
                if let [al, ah, bl, bh, cl, ch, dl, dh] = *obj {
                    let a = gcb_to_rgb(((ah as u16) << 8) | al as u16);
                    let b = gcb_to_rgb(((bh as u16) << 8) | bl as u16);
                    let c = gcb_to_rgb(((ch as u16) << 8) | cl as u16);
                    let d = gcb_to_rgb(((dh as u16) << 8) | dl as u16);
                    outline.push_str(&format!("  {: >2}. #{:0>6X} #{:0>6X} #{:0>6X} #{:0>6X}\n", index, a, b, c, d));
                }
            }
        } else {
            let pal = self.io_registers[0x47];
            let a = dmg_to_rgb((pal >> 6) & 0b11);
            let b = dmg_to_rgb((pal >> 4) & 0b11);
            let c = dmg_to_rgb((pal >> 2) & 0b11);
            let d = dmg_to_rgb(pal & 0b11);
            outline.push_str(&format!("  {: >2}. #{:0>6X} #{:0>6X} #{:0>6X} #{:0>6X}\n", 0, a, b, c, d));
        }

        outline.push_str("\nOBJPalette:\n");
        if self.cgb {
            for (index, obj) in self.obj_palettes.chunks(8).enumerate() {
                if let [al, ah, bl, bh, cl, ch, dl, dh] = *obj {
                    let a = gcb_to_rgb(((ah as u16) << 8) | al as u16);
                    let b = gcb_to_rgb(((bh as u16) << 8) | bl as u16);
                    let c = gcb_to_rgb(((ch as u16) << 8) | cl as u16);
                    let d = gcb_to_rgb(((dh as u16) << 8) | dl as u16);
                    outline.push_str(&format!("  {: >2}. #{:0>6X} #{:0>6X} #{:0>6X} #{:0>6X}\n", index, a, b, c, d));
                }
            }
        } else {
            let pal = self.io_registers[0x48];
            let a = dmg_to_rgb((pal >> 6) & 0b11);
            let b = dmg_to_rgb((pal >> 4) & 0b11);
            let c = dmg_to_rgb((pal >> 2) & 0b11);
            let d = dmg_to_rgb(pal & 0b11);
            outline.push_str(&format!("  {: >2}. #{:0>6X} #{:0>6X} #{:0>6X} #{:0>6X}\n", 0, a, b, c, d));

            let pal = self.io_registers[0x49];
            let a = dmg_to_rgb((pal >> 6) & 0b11);
            let b = dmg_to_rgb((pal >> 4) & 0b11);
            let c = dmg_to_rgb((pal >> 2) & 0b11);
            let d = dmg_to_rgb(pal & 0b11);
            outline.push_str(&format!("  {: >2}. #{:0>6X} #{:0>6X} #{:0>6X} #{:0>6X}\n", 1, a, b, c, d));
        }

        outline.push_str("\nOAM:\n");
        let mut visible = false;
        for (index, obj) in self.oam.chunks(4).enumerate() {
            if let [y, x, tile, attr] = *obj {
                if y > 0 {
                    outline.push_str(&format!(
                        "  {: >2}. X={: >3}  Y={: >3}  T={: >3}  P={}  {}{}{}{}\n",
                        index + 1,
                        x,
                        y,
                        tile,
                        if self.cgb { attr & 0b111 } else { attr >> 4 & 0x1},
                        if attr & 0b0100_0000 != 0 { "W" } else { "-" },
                        if attr & 0b0010_0000 != 0 { "Y" } else { "-" },
                        if attr & 0b0001_0000 != 0 { "X" } else { "-" },
                        if attr & 0b0000_0100 != 0 { "1" } else { "0" },
                    ));
                    visible = true;
                }
            }
        }
        if !visible {
            outline.push_str("   -\n");
        }
        (outline, locations)
    }
}

fn dmg_to_rgb(c: u8) -> u32 {
    let palette: [u32; 4] = [0x00FFFFFF, 0x00808080, 0x00404040, 0x00000000];
    palette[c as usize]
}

fn gcb_to_rgb(c: u16) -> u32 {
    let b = (((c >> 10) & 0x1F) as f32 * 8.25).min(255.0) as u32;
    let g = (((c >> 5) & 0x1F) as f32 * 8.25).min(255.0) as u32;
    let r = ((c & 0x1F) as f32 * 8.25).min(255.0) as u32;
    (r << 16) | (g << 8) | b
}

