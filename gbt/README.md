# GameBoy Tooling

**gbt** is a [Rust](https://rust-lang.org) based collection of tooling for GameBoy.

Binaries:

- [Linux](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/gbt/target/x86_64-unknown-linux-musl/release/gbt?job=linux-musl)
- [Windows](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/gbt/target/x86_64-pc-windows-gnu/release/gbt.exe?job=windows-mingw)

## Commands

- `tiles`: Converts image files into the GameBoy tile data format.
- `lmms`: Converts [LMMS Projects](https://lmms.io/) that use the FreeBoy Audio Plugin into a custom GameBoy format.
- `compress`: Compresses input files using a custo LZ4 variant for decompression on the GameBoy.

## GameBoy Support 

- [gb-lib](https://gitlab.com/BonsaiDen/gb-lib) assembly code libraries for use with `gbt`

## License

Licensed under either of
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
at your option.

