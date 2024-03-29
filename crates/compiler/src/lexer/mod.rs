// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Token Abstraction ----------------------------------------------------------
mod token;
pub use self::token::{LexerToken, InnerToken, TokenType, TokenIterator, Symbol};


// Macros ---------------------------------------------------------------------
macro_rules! underscore {
    ($bind:ty) => {
        _
    }
}

#[macro_export]
macro_rules! lexer_token {
    // Tuple only
    ($name:ident, $typ:ident, ($($de:ident),*), {
        $(
            $variant:ident(
                ($($bind:ty),*)
            )
        ),*
    }) => {

        use crate::lexer::token::TokenType;

        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        pub enum $typ {
            $(
                $variant
            ),*
        }
        impl TokenType for $typ {}

        #[derive($($de),*)]
        pub enum $name {
            $(
                $variant(InnerToken, $($bind),*)
            ),*
        }

        impl LexerToken for $name {

            type Typ = $typ;

            fn typ(&self) -> $typ {
                match self {
                    $(
                        $name::$variant(_, $(underscore!($bind)),*) => $typ::$variant
                    ),*
                }
            }

            fn inner(&self) -> &InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|* => {
                        inner
                    }
                }
            }

            fn inner_mut(&mut self) -> &mut InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|* => {
                        inner
                    }
                }
            }

            fn into_inner(self) -> InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|* => {
                        inner
                    }
                }
            }
        }
    };
    // Tuple and Struct
    ($name:ident, $typ:ident, ($($de:ident),*), {
        $(
            $variant:ident(
                ($($bind:ty),*)
            )
        ),*
    }, {
        $(
            $struct_variant:ident {
                $($struct_name:ident => $struct_type:ty),*
            }
        ),*

    }) => {

        use crate::lexer::token::TokenType;

        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        pub enum $typ {
            $(
                $variant
            ),*,
            $(
                $struct_variant
            ),*
        }
        impl TokenType for $typ {}

        #[derive($($de),*)]
        pub enum $name {
            $(
                $variant(InnerToken, $($bind),*)
            ),*,
            $(
                $struct_variant {
                    inner: InnerToken,
                    $($struct_name: $struct_type),*
                }
            ),*
        }

        impl LexerToken for $name {

            type Typ = $typ;

            fn typ(&self) -> $typ {
                match self {
                    $(
                        $name::$variant(_, $(underscore!($bind)),*) => $typ::$variant
                    ),*,
                    $(
                        $name::$struct_variant { .. } => $typ::$struct_variant
                    ),*
                }
            }

            fn inner(&self) -> &InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|*
                    |
                    $(
                        $name::$struct_variant {inner, ..}
                    )|*
                    => {
                        inner
                    }
                }
            }

            fn inner_mut(&mut self) -> &mut InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|*
                    |
                    $(
                        $name::$struct_variant {inner, ..}
                    )|*
                    => {
                        inner
                    }
                }
            }

            fn into_inner(self) -> InnerToken {
                match self {
                    $(
                        $name::$variant(inner, $(underscore!($bind)),*)
                    )|*
                    |
                    $(
                        $name::$struct_variant {inner, ..}
                    )|*
                    => {
                        inner
                    }
                }
            }
        }
    };
}


// External Dependencies ------------------------------------------------------
use file_io::FileReader;


// Modules --------------------------------------------------------------------
mod file;
pub mod stage;
pub use self::file::LexerFile;


// Re-Exports --------------------------------------------------------------------
use self::stage::LexerStage;
pub use self::stage::entry::{EntryStage, EntryToken};
pub use self::stage::expression::{ExpressionStage, ExpressionToken, IntegerMap};
pub use self::stage::include::IncludeStage;
pub use self::stage::macros::{MacroStage, MacroDefinition, BUILTIN_MACRO_DEFS, BUILTIN_MACRO_INDEX};
pub use self::stage::value::ValueStage;
pub use self::stage::macros::MacroCall;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;


// Lexer Abstraction ----------------------------------------------------------
pub struct Lexer<T: LexerStage> {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<T::Output>,
    pub macro_calls: Vec<MacroCall>,
    pub data: Vec<T::Data>
}

impl<T: LexerStage> Lexer<T> {

    pub fn from_file<R: FileReader>(file_reader: &R, child_path: &PathBuf) -> Result<Self, SourceError>{
        let mut files = Vec::with_capacity(16);
        let tokens = T::from_file(file_reader, child_path, &mut files).map_err(|err| {
            err.extend_with_location(&files)
        })?;
        Ok(Self {
            files,
            tokens,
            macro_calls: Vec::with_capacity(128),
            data: Vec::with_capacity(128)
        })
    }

    pub fn from_lexer(lexer: Lexer<T::Input>) -> Result<Self, SourceError> {
        let files = lexer.files;
        let mut data = Vec::with_capacity(128);
        let mut macro_calls = lexer.macro_calls;
        let tokens = T::from_tokens(lexer.tokens, &mut macro_calls, &mut data).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        Ok(Self {
            files,
            tokens,
            macro_calls,
            data
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    #[cfg(test)]
    pub fn macro_calls_count(&self) -> usize {
        self.macro_calls.len()
    }

    pub fn data(&mut self) -> Vec<T::Data> {
        std::mem::take(&mut self.data)
    }

}

