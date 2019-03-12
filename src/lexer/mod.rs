// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


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
        // TODO remove once all lexers are done
        #[allow(unused)]
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
mod include;
mod expression;
mod macros;
mod value;
#[cfg(test)] mod mocks;


// Exports --------------------------------------------------------------------
pub use self::include::IncludeLexer;
pub use self::expression::ExpressionLexer;
pub use self::macros::MacroLexer;
pub use self::value::ValueLexer;
pub use self::error::LexerError;


// Lexer File Abstraction -----------------------------------------------------
pub struct LexerFile {
    index: usize,
    path: PathBuf,
    contents: String,
    include_stack: Vec<InnerToken>
}

impl LexerFile {

    fn new(index: usize, contents: String, path: PathBuf, include_stack: Vec<InnerToken>) -> Self {
        Self {
            index,
            path,
            contents,
            include_stack
        }
    }

    fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        let (mut line, mut col) = (0, 0);
        for (i, c) in self.contents.chars().enumerate() {
            if i == index {
                break;

            } else if c == '\n' || c == '\r' {
                line += 1;
                col = 0;

            } else {
                col += 1;
            }
        }
        (line, col)
    }

}

