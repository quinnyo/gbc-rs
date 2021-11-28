" GameBoy Outline - syntax
" 2019-2021 Ivo Wetzel
" http://gitlab.com/BonsaiDen/gbc-rs
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn keyword outline_on              On Running
syn keyword outline_off             Off Stopped Paused
syn keyword outline_unknown         Unknown
syn keyword outline_register        AF BC DE HL SP PC IME PPU PRIO WIN MAP BG OBJ TILES SIZE STAT SCX SCY LY X Y T P BANK MODE WBNK SPD
syn match outline_number_hex        "\$[0-9a-f-A-F][0-9a-fA-F_]*\>"
syn match outline_number_dec        "[0-9][0-9_]*\>"
syn match outline_number_bin        "%[0-1][0-1_]*\>"
syn keyword outline_heading         Status ROM Registers Backtrace Breakpoints VRAM OAM BGPalette OBJPalette CPU
syn match outline_name              "[A-Za-z_][A-Za-z0-9_]*"
syn match outline_offset            "[+\|:\|=\|-]"
syn match outline_parens            "[(\|)]"
syn match outline_list_item         "[0-9][0-9]*\."
syn keyword outline_special         8x16 VBlank HBlank Search Transfer

setl foldmethod=indent
setl shiftwidth=2
setl foldlevelstart=1
""syntax region foldBraces start=/{/ end=/}/ transparent fold keepend extend

hi def link outline_on              String
hi def link outline_off             Error
hi def link outline_unknown         Comment
hi def link outline_parens          Comment
hi def link outline_register        Identifier
hi def link outline_number_hex      Number
hi def link outline_number_dec      Number
hi def link outline_number_bin      Number
hi def link outline_heading         Macro
hi def link outline_list_item       Function
hi def link outline_special         Function
hi def link outline_offset          Comment

let b:current_syntax = 'GameBoy'

