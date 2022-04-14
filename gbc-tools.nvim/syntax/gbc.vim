" gbc - syntax
" 2019 Ivo Wetzel
" http://gitlab.com/BonsaiDen/gbc-rs
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn case ignore

syn keyword gb_instruction  adc add and bit ccf cp cpl daa dec di ei ex halt inc ld ldd ldi ldh ldio nop or pop push res reti rl rla rlc rlca rr rra rrc rrca rst sbc scf set sla sra srl stop sub swap xor res ldxa addw subw mul div neg decx incx vsync pushx popx djnz jc
syn keyword gb_register     a b c d e h l af bc de hl sp hld hli
syn keyword gb_branch       jr jp call
syn keyword gb_return       ret retx
syn keyword gb_debug        msg brk

syn match gb_macroArg       "@[a-z_][a-z0-9_]*"
syn match gb_indentifier    "[a-z_][a-z0-9_]*"
syn match gb_label          "^[a-z_][a-z0-9_]*:"he=e-1
syn match gb_localLabel     "^\.[a-z_][a-z0-9_]*:"he=e-1
syn match gb_condition      "\(\sc\|nc\|\sz\|nz\),\s*\(\.\|@\|[a-z0-9_]\{2,}\)"me=s+2
syn match gb_condition2     "\(\sc\|nc\|\sz\|nz\)$"
syn match gb_condition3     "\(\sc\|nc\|\sz\|nz\)\s*;"me=e-1

syn match gb_comment        ";.*" contains=gb_todos
syn region gb_string        start=+"+ skip=+\\"+ end=+"+
syn region gb_string        start=+'+ skip=+\\'+ end=+'+

syn keyword gb_todos        contained todo fixme xxx warning danger notice bug
syn keyword gb_macro        strupr strlwr strsub strin stpadr strpadl sin cos tan asin acos atan2 log exp floor ceil round sqrt max min abs rand
syn keyword gb_directive    global include macro endmacro section db dw bw ds8 ds16 ds block endblock const equ equs struct endstruct using namespace endnamespace
syn keyword gb_keyword      volatile binary for in to repeat endfor if then else endif default wram0 wramx rom0 romx hram

syn match gb_fixedNumber    "\d\\+.\d\+\>"
syn match gb_hexNumber	    "\$[0-9a-f][0-9a-f_]*\>"
syn match gb_decNumber	    "[0-9][0-9_]*\>"
syn match gb_binNumber	    "%[0-1][0-1_]*\>"

"hi link gb_label       Identifier
hi def link gb_localLabel  gbcLabel

hi def link gb_string      String
hi def link gb_comment     Comment
hi def link gb_instruction Statement
hi def link gb_branch      Statement
hi def link gb_return      Function
hi def link gb_debug       Todo
hi def link gb_register    Identifier
hi def link gb_todos       Todo
hi def link gb_macroArg    gbcMacroArg
hi def link gb_macro       Function
hi def link gb_condition   Function
hi def link gb_condition2  Function
hi def link gb_condition3  Function
hi def link gb_directive   Function
hi def link gb_keyword     Macro

hi def link gb_fixedNumber Number
hi def link gb_hexNumber   Number
hi def link gb_decNumber   Number
hi def link gb_binNumber   Number

let b:current_syntax = "gbc"

