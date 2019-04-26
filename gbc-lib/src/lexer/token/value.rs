// STD Dependencies -----------------------------------------------------------
use std::fmt;


// Token Value ----------------------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TokenValue {
    INCLUDE,
    BINARY,
    USING,
    VOLATILE,
    SECTION,

    DB,
    DW,
    BW,
    DS,
    DS8,
    DS16,

    ROM0,
    ROMX,
    WRAM0,
    WRAMX,
    RAM,
    RAMX,
    HRAM,

    DEFAULT,
    BANK,
    EQU,

    BLOCK,
    ENDBLOCK,

    MACRO,
    ENDMACRO,

    IF,
    THEN,
    ELSE,
    ENDIF,

    FOR,
    IN,
    TO,
    REPEAT,
    ENDFOR,

    HLD,
    HLI,
    OpenBracket,
    CloseBracket,

    DBG,
    MAX,
    MIN,
    FLOOR,
    CEIL,
    ROUND,
    LOG,
    EXP,
    SQRT,
    ABS,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    ATAN2,
    STRUPR,
    STRLWR,
    STRLEN,
    STRSUB,
    STRIN,
    STRPADR,
    STRPADL,
    Raw(String)
}

impl TokenValue {
    pub fn as_str(&self) -> &str {
        match self {
            TokenValue::INCLUDE  => "INCLUDE",
            TokenValue::BINARY   => "BINARY",
            TokenValue::USING    => "USING",
            TokenValue::VOLATILE => "VOLATILE",
            TokenValue::SECTION  => "SECTION",

            TokenValue::DB       => "DB",
            TokenValue::DW       => "DW",
            TokenValue::BW       => "BW",
            TokenValue::DS       => "DS",
            TokenValue::DS8      => "DS8",
            TokenValue::DS16     => "DS16",

            TokenValue::ROM0     => "ROM0",
            TokenValue::ROMX     => "ROMX",
            TokenValue::WRAM0    => "WRAM0",
            TokenValue::WRAMX    => "WRAMX",
            TokenValue::RAM      => "RAM",
            TokenValue::RAMX     => "RAMX",
            TokenValue::HRAM     => "HRAM",

            TokenValue::DEFAULT  => "DEFAULT",
            TokenValue::BANK     => "BANK",
            TokenValue::EQU      => "EQU",

            TokenValue::BLOCK    => "BLOCK",
            TokenValue::ENDBLOCK => "ENDBLOCK",

            TokenValue::MACRO    => "MACRO",
            TokenValue::ENDMACRO => "ENDMACRO",

            TokenValue::IF       => "IF",
            TokenValue::THEN     => "THEN",
            TokenValue::ELSE     => "ELSE",
            TokenValue::ENDIF    => "ENDIF",

            TokenValue::FOR      => "FOR",
            TokenValue::IN       => "IN",
            TokenValue::TO       => "TO",
            TokenValue::REPEAT   => "REPEAT",
            TokenValue::ENDFOR   => "ENDFOR",

            TokenValue::HLD      => "hld",
            TokenValue::HLI      => "hli",
            TokenValue::OpenBracket => "[",
            TokenValue::CloseBracket => "]",

            TokenValue::DBG      => "DBG",
            TokenValue::MAX      => "MAX",
            TokenValue::MIN      => "MIN",
            TokenValue::FLOOR    => "FLOOR",
            TokenValue::CEIL     => "CEIL",
            TokenValue::ROUND    => "ROUND",
            TokenValue::LOG      => "LOG",
            TokenValue::EXP      => "EXP",
            TokenValue::SQRT     => "SQRT",
            TokenValue::ABS      => "ABS",
            TokenValue::SIN      => "SIN",
            TokenValue::COS      => "COS",
            TokenValue::TAN      => "TAN",
            TokenValue::ASIN     => "ASIN",
            TokenValue::ACOS     => "ACOS",
            TokenValue::ATAN     => "ATAN",
            TokenValue::ATAN2    => "ATAN2",
            TokenValue::STRUPR   => "STRUPR",
            TokenValue::STRLWR   => "STRLWR",
            TokenValue::STRLEN   => "STRLEN",
            TokenValue::STRSUB   => "STRSUB",
            TokenValue::STRIN    => "STRIN",
            TokenValue::STRPADR  => "STRPADR",
            TokenValue::STRPADL  => "STRPADL",
            TokenValue::Raw(s) => s.as_str()
        }
    }

    pub fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<String> for TokenValue {
    fn from(s: String) -> TokenValue {
        match s.as_str() {
            "INCLUDE"   =>  TokenValue::INCLUDE,
            "BINARY"    =>  TokenValue::BINARY,
            "USING"     =>  TokenValue::USING,
            "VOLATILE"  =>  TokenValue::VOLATILE,
            "SECTION"   =>  TokenValue::SECTION,

            "DB"        =>  TokenValue::DB,
            "DW"        =>  TokenValue::DW,
            "BW"        =>  TokenValue::BW,
            "DS"        =>  TokenValue::DS,
            "DS8"       =>  TokenValue::DS8,
            "DS16"      =>  TokenValue::DS16,

            "ROM0"      => TokenValue::ROM0,
            "ROMX"      => TokenValue::ROMX,
            "WRAM0"     => TokenValue::WRAM0,
            "WRAMX"     => TokenValue::WRAMX,
            "RAM"       => TokenValue::RAM,
            "RAMX"      => TokenValue::RAMX,
            "HRAM"      => TokenValue::HRAM,

            "DEFAULT"   =>  TokenValue::DEFAULT,
            "BANK"      =>  TokenValue::BANK,
            "EQU"       =>  TokenValue::EQU,

            "BLOCK"     =>  TokenValue::BLOCK,
            "ENDBLOCK"  =>  TokenValue::ENDBLOCK,

            "MACRO"     =>  TokenValue::MACRO,
            "ENDMACRO"  =>  TokenValue::ENDMACRO,

            "IF"        =>  TokenValue::IF,
            "THEN"      =>  TokenValue::THEN,
            "ELSE"      =>  TokenValue::ELSE,
            "ENDIF"     =>  TokenValue::ENDIF,

            "FOR"       =>  TokenValue::FOR,
            "IN"        =>  TokenValue::IN,
            "TO"        =>  TokenValue::TO,
            "REPEAT"    =>  TokenValue::REPEAT,
            "ENDFOR"    =>  TokenValue::ENDFOR,

            "hld"       =>  TokenValue::HLD,
            "hli"       =>  TokenValue::HLI,
            "["         =>  TokenValue::OpenBracket,
            "]"         =>  TokenValue::CloseBracket,

            "DBG"       =>  TokenValue::DBG,
            "MAX"       =>  TokenValue::MAX,
            "MIN"       =>  TokenValue::MIN,
            "FLOOR"     =>  TokenValue::FLOOR,
            "CEIL"      =>  TokenValue::CEIL,
            "ROUND"     =>  TokenValue::ROUND,
            "LOG"       =>  TokenValue::LOG,
            "EXP"       =>  TokenValue::EXP,
            "SQRT"      =>  TokenValue::SQRT,
            "ABS"       =>  TokenValue::ABS,
            "SIN"       =>  TokenValue::SIN,
            "COS"       =>  TokenValue::COS,
            "TAN"       =>  TokenValue::TAN,
            "ASIN"      =>  TokenValue::ASIN,
            "ACOS"      =>  TokenValue::ACOS,
            "ATAN"      =>  TokenValue::ATAN,
            "ATAN2"     =>  TokenValue::ATAN2,
            "STRUPR"    =>  TokenValue::STRUPR,
            "STRLWR"    =>  TokenValue::STRLWR,
            "STRLEN"    =>  TokenValue::STRLEN,
            "STRSUB"    =>  TokenValue::STRSUB,
            "STRIN"     =>  TokenValue::STRIN,
            "STRPADR"   =>  TokenValue::STRPADR,
            "STRPADL"   =>  TokenValue::STRPADL,
            _           => TokenValue::Raw(s)
        }
    }
}

