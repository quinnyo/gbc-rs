enum Mnemonic {
    ADD_HL_BC(),
    META(MetaMnemonic)
}

enum MetaMnemonic {
    ADD_W(),
    VSync()
}

lexer_token!(EntryToken, (Debug, Eq, PartialEq), {
    BinaryFile((Vec<u8>))
}, {
    // TODO do const eval first in rom layout computation
    // TODO then do address resolution
    // TODO during parsing store constants and addresses to evaluate
    Constant {
        // TODO value (Expression)
        // TODO type Number / String
        name: String,
        size: usize
    },
    Variable {
        // TODO byte / word
        name: String,
        size: usize
    },
    Instruction {
        // TODO value(s) for parameters (Expression(s))
        mnemonic: Mnemonic // TODO put values directly into mnemonic enum
        // TODO size get from Mnemonic
    },
    SectionDeclaration {
        // TODO offset etc.
        name => String
    },
    GlobalLabelDef {
        name => String
    },
    LocalLabelDef {
        name => String
    }
});

