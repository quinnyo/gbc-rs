// Token Abstraction ----------------------------------------------------------
mod token;
pub use self::token::{LexerToken, InnerToken, TokenType, TokenIterator};


// Macros ---------------------------------------------------------------------
macro_rules! underscore {
    ($bind:ty) => {
        _
    }
}

#[macro_export]
macro_rules! lexer_token {
    ($name:ident, ($($de:ident),*), {
        $(
            $variant:ident(
                ($($bind:ty),*)
            )
        ),*
    }) => {
        #[derive($($de),*)]
        pub enum $name {
            $(
                $variant(InnerToken, $($bind),*)
            ),*
        }

        impl LexerToken for $name {
            fn typ(&self) -> TokenType {
                match self {
                    $(
                        $name::$variant(_, $(underscore!($bind)),*) => TokenType::$variant
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
    ($name:ident, ($($de:ident),*), {
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
            fn typ(&self) -> TokenType {
                match self {
                    $(
                        $name::$variant(_, $(underscore!($bind)),*) => TokenType::$variant
                    ),*,
                    $(
                        $name::$struct_variant { .. } => TokenType::$struct_variant
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
    }
}


// Modules --------------------------------------------------------------------
mod error;
mod file;
mod stage;
use self::error::LexerError;
use self::file::LexerFile;


// Re-Exports --------------------------------------------------------------------
pub use self::stage::include::IncludeLexer;
pub use self::stage::expression::ExpressionLexer;
pub use self::stage::macros::MacroLexer;
pub use self::stage::value::ValueLexer;

