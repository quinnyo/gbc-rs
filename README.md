# A opinionated Gameboy Compiler

[![pipeline status](https://gitlab.com/BonsaiDen/gbc-rs/badges/master/pipeline.svg)](https://gitlab.com/BonsaiDen/gbc-rs/commits/master)

**gbc** is a [Rust](https://rust-lang.org) based compiler for Gameboy z80 assembly code.

Pre-compiled binaries:

- [Linux](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/target/x86_64-unknown-linux-musl/release/gbc?job=linux-musl)
- [Windows](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/target/x86_64-pc-windows-gnu/release/gbc.exe?job=windows-mingw)


## Main Goals

- No further external programs required to build a ROM
- Nice error messages
- High test coverage
- Quality of life improvements via syntactic sugar

## Stuff made with GBC

- [Vectroid](https://gitlab.com/BonsaiDen/vectroid.gb) an "Asteroids" Game
- [Examples and Libraries](https://gitlab.com/BonsaiDen/gb-lib/tree/master/examples) for getting started with GameBoy development


## Additional Tooling

- [gbc-analyzer](https://gitlab.com/BonsaiDen/gbc-rs/tree/master/crates/lsp) a implementation of the language server protocol
- [gbc-tools.nvim](https://gitlab.com/BonsaiDen/gbc-rs/tree/master/gbc-tools.nvim) a nvim plugin to use the custom features of the lsp implementation

## Supported Tools and Libraries

- [gbt](https://gitlab.com/BonsaiDen/gbc-rs/tree/master/crates/gbt) data conversion tool
- [gb-lib](https://gitlab.com/BonsaiDen/gb-lib) assembly code libraries for use with `gbt`


## Usage

```
USAGE:
    gbc [OPTIONS] [SOURCE_FILE] [SUBCOMMAND]

ARGS:
    <SOURCE_FILE>    Input source file

OPTIONS:
    -d, --decompile            Decompile the input file instead
    -D, --debug                Enable debug instructions for BGB debugger
    -h, --help                 Print help information
    -i, --info                 Display ROM info
    -m, --symbol-map <FILE>    Output symbol mapping for debuggers
        --no-optimize          Disable instruction optimizations
    -o, --output-rom <FILE>    ROM file to generate
    -s, --silent               Surpress all output
    -S, --segments             Display segments usage
    -u, --source-map <FILE>    Output source mapping for debuggers
    -V, --version              Print version information

SUBCOMMANDS:
    debug      Builds a debug ROM using the local "gbc.toml" project configuration.
    emu        Builds a ROM and emulates it via the specified configuration.
    help       Print this message or the help of the given subcommand(s)
    release    Builds a release ROM using the local "gbc.toml" project configuration.
```

## Project Configuration Files

A `gbc.toml` file can be placed in the project's top level directory.

```toml
# Example Configuration File
[rom]
input = "src/main.gbc"
output = "build/rom.gbc"

[report]
info = true
segments = true

[emulator.gambatte]
command = "gambatte_sdl -s 2"

[emulator.bgb]
debug = true
command = "bgb"
```

The project can be now build via `gbc release` or `gbc debug` and once of the 
configured emulators can be invoked via `gbc emu <name>`.


## General Syntax

### Visibility

By default all **constants* and **labels** are visible only within their file of declaration.

To expose them to other files i.e. the global namespace, they must be prefixed with the `GLOBAL` keyword:

```asm
; File local Constant and label
foo EQU 2
file_label:


; Global Constant and label
GLOBAL foo EQU 2
GLOBAL file_label:
```

### Includes

gbc supports two types of includes:

```asm
; Source files
INCLUDE "foo.asm"
```

```asm
; Binary files
INCLUDE BINARY "foo.bin"
```

Both of these support an additional `USING` directive which can be used to 
pre-process their input before it gets parsed / included into the ROM by gbc.

The called program must output the raw result only to `stdout`.

```asm
; Generate a source file on demand
; This is equivialent to calling:
;    mod2gb --to-asm /absolute/path/to/project/src/music.mod
INCLUDE "src/music.mod" USING "mod2gb --to-asm"
```

```asm
; Pre-process a tile image using another program.
; This is equivialent to calling:
;    image2gb --some-palette -i /absolute/path/to/project/src/tiles.png
INCLUDE BINARY "src/tiles.png" USING "image2gb --some-palette -i"
```

### Default Constant declarations

In addition to the usual constant declarations of:

```asm
CONST FOO 1
CONST BAR "Hello World"
```

gbc supports `DEFAULT` declarations which can be later overwritten with a standard declaration:

```asm
; Inside of the library file
CONST DEFAULT LIBRARY_RAM_ADDRESS $D000

; In the actual project which already has other things reside at $D000
CONST LIBRARY_RAM_ADDRESS $D100
```

### Blocks

#### Using

gbc supports processing blocks of data with external commands:

```asm
; Pre-process the data declarations inside the block using another program.
BLOCK USING `compress --level 9`
    DB 1,1,1,1,1,1
    DB 2,2,2,2,2,2
ENDBLOCK
```

> Note: Only data declarations that reference no addresses are allowed in `USING` blocks.


#### Volatile

gbc does perform automatic optimization by default. This saves valuable bytes 
without resorting to heavy obfuscation of things like `cp 0` -> `or a`.

However, in certain situations this might break things which depend on fixed cycles / code sizes.

While the `--no-optimize` option exists to disable optimizations at the global level,
individual code sections can be excluded by wrapping them inside `VOLATILE` blocks.

```asm
BLOCK VOLATILE
    cp 0; will not be optimized to "or a"
ENDBLOCK
```

There is also the option to disable optimization of a single instruction by prefixing it directly with 'VOLATILE':

```asm
    VOLATILE cp 0; will not be optimized to "or a"
```

### User defined Macros

User Macros work by generating and substituting tokens.

They can be defined by using the following syntax:

```asm
MACRO USER_MACRO_NAME(@reg, @value)
ld  @reg,@value
ENDMACRO
```

And called like this:

```asm
USER_MACRO_NAME(a, 2)
```

### IF statements

Compile time if statements are supported, these can be nested and chained to any depth.

> Note: There is no scoping, any constants or labels declared inside an IF statement will have the same scoping rules applied as all other constants or labels.

```asm
IF 1 + 1 == 2 THEN 
; Code or Data

ELSE IF 1 + 1 == 4 THEN 
; Code or Data

ELSE
; Code or Data

ENDIF
```

### FOR statements

Compile time for statements are supported, these can be nested and chained to any depth.

> Note: It is not possible to declare constants inside of FOR statements.

```asm
FOR i IN 0 TO 10 REPEAT
    DB i
ENDFOR
```

### NAMESPACE statements

Labels can be grouped into `STRUCTS` in order to reduce code complexity:

```asm
; Create a NAMESPACE "foo" with two fields
NAMESPACE foo
    field: DB
    value: DB
ENDNAMESPACE

; Load the address of the first field into hl
ld      hl,foo::field
```


### Meta Instructions

Meta instructions encode multiple *native* instruction at the source level in order
to abstract and simplify often used code paths.

These aim at increasing the readability of the source and reducing the potential
for off-by-one errors.

#### **brk**

Inserts a debug breakpoint to be used with the BGB emulator's debugger

```asm
; ld b,b
brk
```

#### **neg**

A shorthand for negation of the accumulator:

```asm
; cpl
; inc a
neg
```

#### **addw**

Adds a 8-bit operand to a 16-bit register using only the `Accumulator`:

```asm
; ld      a,$ff
; add     l
; ld      l,a
; adc     h
; sub     l
; ld      h,a
addw  hl,$ff
addw  bc,$ff
addw  de,$ff

; ld      a,reg
; add     l
; ld      l,a
; adc     h
; sub     l
; ld      h,a
addw  hl,reg
addw  bc,reg
addw  de,reg
```

#### **subw**

Subtracts a 8-bit operand from the 16-bit register using only the `Accumulator`:

```asm
; sub     l
; cpl
; inc     a
; ld      l,a
; ld      a,h
; sbc     0
; ld      h,a
subw      hl,a

; ld      a,l
; sub     reg
; ld      l,a
; ld      a,h
; sbc     0
; ld      h,a
subw      hl,reg
subw      bc,reg
subw      de,reg
```

#### **retx**

Puts the argument into the `Accumulator` before returning:

```asm
; ld    a,[hl]
; ret
retx    [hl]

; ld    a,b
; ret
retx    b

; ld    a,2
; ret
retx    2
```

#### **jc**

Jumps based on the result of a comparison (==, !=, >=, <=, >, <):

```asm
; cp    20
; jr    z,.label
jc      a == 20,.label

; cp    [hl]
; jr    nz,.label
jc      a != hl,.label

; cp    b
; jr    c,.label
jc      a < b,.label

; cp    c
; jr    z,.skip
; jr    nc,.label
; .skip:
jc      a > c,.label

; ld    a,[someLabel]
; cp    40
; jr    z,.label
jc      [someLabel] == 80,.label

; ld    a,[de]
; cp    80
; jr    z,.label
jc      [de] == 40,.label
```

#### **vsync**

A shorthand for the VRAM access loop:

```asm
; ld      a,[$FF41]       
; and     31      
; jr      nz,@-6          
vsync
```

#### **incx**

Extended increment of a memory address, using the `Accumulator` as an intermediate register (destroying its contents):

```asm
; ld a,[$0000]
; inc a
; ld [$0000],a
incx [$0000]
```

#### **decx**

Extended decrement of a memory address, using the `Accumulator` as an intermediate register (destroying its contents):

```asm
; ld a,[$0000]
; dec a
; ld [$0000],a
decx [$0000]
```

#### **pushx**

Shorthand for pushing all registers onto the stack:

```asm
; push af
; push bc
; push de
; push hl
pushx
```

#### **popx**

Shorthand for popping all registers from the stack:

```asm
; pop hl
; pop de
; pop bc
; pop af
popx
```

#### **djnz**

Emulates the standard looping instruction of the Z80:

```asm
; dec b
; jr  nz,.label
djnz  .label
```

#### **ldxa**

Extended memory loads using no intermediate registers:

```asm
; ld  b,d
; ld  c,e
ldxa  bc,de

; ld  b,h
; ld  c,l
ldxa  bc,hl

; ld  d,b
; ld  e,c
ldxa  de,bc

; ld  d,h
; ld  e,l
ldxa  de,hl

; ld  [hl],e
; inc l
; ld  [hl],d
ldxa  [hl],de

; ld  [hl],c
; inc l
; ld  [hl],b
ldxa  [hl],bc

; ld  e,[hl]
; inc l
; ld  d,[hl]
ldxa  de,[hl]

; ld  c,[hl]
; inc l
; ld  b,[hl]
ldxa  bc,[hl]
```

Extended memory loads using the `Accumulator` as an intermediate register (destroying its contents):

```asm
; ld  a,[hli]
; ld  h,[hl]
; ld  l,a
ldxa  hl,[hl]

; ld  a,[hli]
; ld  R,a
ldxa  b,[hli]
ldxa  c,[hli]
ldxa  d,[hli]
ldxa  e,[hli]
ldxa  h,[hli]
ldxa  l,[hli]

; ld  a,[hld]
; ld  R,a
ldxa  b,[hld]
ldxa  c,[hld]
ldxa  d,[hld]
ldxa  e,[hld]
ldxa  h,[hld]
ldxa  l,[hld]

; ld  a,R
; ld  [hli],a
ldxa  [hli],b
ldxa  [hli],c
ldxa  [hli],d
ldxa  [hli],e
ldxa  [hli],h
ldxa  [hli],l

; ld   a,R
; ld   [hld],a
ldxa  [hld],b
ldxa  [hld],c
ldxa  [hld],d
ldxa  [hld],e
ldxa  [hld],h
ldxa  [hld],l

; ld  a,$ff
; ld  [$0000],a
ldxa  [$0000],$ff

; ld  a,$ff
; ld  [hli],a
ldxa  [hli],$ff

; ld  a,$ff
; ld  [hld],a
ldxa  [hld],$ff

; ld  a,R
; ld  [$0000],a
ldxa  [$0000],b
ldxa  [$0000],c
ldxa  [$0000],d
ldxa  [$0000],e
ldxa  [$0000],h
ldxa  [$0000],l

; ld  a,RR
; ld  a,R
; ld  $[$0000],a
; ld  a,R
; ld  $[$0000 + 1],a
ldxa  [$0000],bc
ldxa  [$0000],de
ldxa  [$0000],hl

; ld  a,[hli]
; ld  [$0000],a
ldxa  [$0000],[hli]

; ld  a,[hld]
; ld  [$0000],a
ldxa  [$0000],[hld]

; ld  a,[$0000]
; ld  [$0000],a
ldxa  [$0000],[$0000]

; ld  a,[$0000]
; ld  R,a
ldxa  b,[$0000]
ldxa  c,[$0000]
ldxa  d,[$0000]
ldxa  e,[$0000]
ldxa  h,[$0000]
ldxa  l,[$0000]

; ld  RR,a
; ld  a,$[0000]
; ld  R,a
; ld  a,$[0000 + 1]
; ld  R,a
ldxa  bc,[$0000]
ldxa  de,[$0000]
ldxa  hl,[$0000]

; ld  a,[$0000]
; ld  [hli],a
ldxa  [hli],[$0000]

; ld  a,[bc]
; ld  [hli],a
ldxa  [hli],[bc]

; ld  a,[de]
; ld  [hli],a
ldxa  [hli],[de]

; ld  a,[$0000]
; ld  [hld],a
ldxa  [hld],[$0000]

; ld  a,[bc]
; ld  [hld],a
ldxa  [hld],[bc]

; ld  a,[de]
; ld  [hld],a
ldxa  [hld],[de]

; ld  a,R
; ld  [bc],a
ldxa  [bc],b
ldxa  [bc],c
ldxa  [bc],d
ldxa  [bc],e
ldxa  [bc],h
ldxa  [bc],l
ldxa  [bc],$80
ldxa  [bc],[$0000]
ldxa  [bc],[hli]
ldxa  [bc],[hld]
ldxa  [bc],[bc]
ldxa  [bc],[de]
ldxa  [bc],[hl]
ldxa  [bc],bc
ldxa  [bc],de
ldxa  [bc],hl

; ld  a,R
; ld  [de],a
ldxa  [de],b
ldxa  [de],c
ldxa  [de],d
ldxa  [de],e
ldxa  [de],h
ldxa  [de],l
ldxa  [de],$80
ldxa  [de],[$0000]
ldxa  [de],[hli]
ldxa  [de],[hld]
ldxa  [de],[bc]
ldxa  [de],[de]
ldxa  [de],[hl]
ldxa  [de],bc
ldxa  [de],de
ldxa  [de],hl
```

## License

Licensed under either of
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
at your option.

