# A Gameboy Assembler 

gbasm is a (Rust)[https://rust-lang.org] based compiler for Gameboy z80 assembly code.

## Compatibility Notes

**gbasm** is mostly compatible with [rgbds](https://github.com/bentley/rgbds) 
but there are some deviations and additions:

### General

- *gbasm* is a multipass compiler, meaning the all sources files and definitions 
are parsed before resolving any names or sizes. 

### Syntax 

- The *load accumulator and increment/decrement hl* type instructions only take `hli` and `hld` as their second operand
- Memory operands do only support `[` and `]` in their syntax
- All names and labels which start with an underscore are treated as being local / private to the file they were defined in

### Instructions

**gbasm** supports additional meta instructions at the source level, 
which are compiled down to multiple native instructions.

These aim at increasing the readability of the source.

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

