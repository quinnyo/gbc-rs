# A opinionated Gameboy Compiler

[![pipeline status](https://gitlab.com/BonsaiDen/gbc-rs/badges/master/pipeline.svg)](https://gitlab.com/BonsaiDen/gbc-rs/commits/master)

**gbc** is a [Rust](https://rust-lang.org) based compiler for Gameboy z80 assembly code.

## Main Goals

- No further external programs required to build a ROM
- Good error messages
- High test coverages
- Quality of life improvements via syntactic sugar

## Games made via GBC

- [Vectroid](https://gitlab.com/BonsaiDen/vectroid.gb)


## Support Tools and Libraries

- [gbt](https://gitlab.com/BonsaiDen/gbc-rs/tree/master/gbt) data conversion tool
- [gb-lib](https://gitlab.com/BonsaiDen/gb-lib) assembly code libraries for use with `gbt`

## Usage

Binaries:

- [Linux](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/target/x86_64-unknown-linux-musl/release/gbc?job=linux-musl)
- [Windows](https://gitlab.com/BonsaiDen/gbc-rs/-/jobs/artifacts/master/raw/target/x86_64-pc-windows-gnu/release/gbc.exe?job=windows-mingw)

```
USAGE:
    gbc [FLAGS] [OPTIONS] <SOURCE_FILE>

FLAGS:
    -D, --debug          Enable debug instructions for BGB debugger
    -h, --help           Prints help information
    -i, --info           Display ROM info
    -l, --lint           Run linter only and display warnings
        --no-optimize    Disable instruction optimizations
    -S, --segments       Display segments usage
    -s, --silent         Surpress all output
    -V, --version        Prints version information

OPTIONS:
    -m, --symbol-map <FILE>    Output symbol mapping for BGB debugger
    -o, --output-rom <FILE>    ROM file to generate

ARGS:
    <SOURCE_FILE>    Input source file
```

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
FOO EQU 1
BAR EQUS "Hello World"
```

gbc supports `DEFAULT` declarations which can be later overwritten with a standard declaration:

```asm
; Inside of the library file
LIBRARY_RAM_ADDRESS DEFAULT EQU $D000

; In the actual project which already has other things reside at $D000
LIBRARY_RAM_ADDRESS EQU $D100
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

### Meta Instructions

Meta instructions encode multiple *native* instruction at the source level in order
to abstract and simplify often used code paths.

These aim at increasing the readability of the source and reducing the potential
for off-by-one errors.

#### **addw**

Adds a 8-bit operand to a 16-bit register using only the `Accumulator`:

```asm
; ld      a,$ff
; add     a,l
; ld      l,a
; adc     a,h
; sub     l
; ld      h,a
addw  hl,$ff
addw  bc,$ff
addw  de,$ff

; add     a,l
; ld      l,a
; adc     a,h
; sub     l
; ld      h,a
addw  hl,a
addw  bc,a
addw  de,a

; ld      a,reg
; add     a,l
; ld      l,a
; adc     a,h
; sub     l
; ld      h,a
addw  hl,reg
addw  bc,reg
addw  de,reg
```

#### **subw**

Subtracts a 8-bit operand from the 16-bit register using only the `Accumulator`:

```asm
TODO add listing
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

#### **ldxa**

Extended memory loads using the `Accumulator` as an intermediate register (destroying its contents):

```asm
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

; ld  a,[$0000]
; ld  [hld],a
ldxa  [hld],[$0000]
```

## License

Licensed under either of
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
at your option.

