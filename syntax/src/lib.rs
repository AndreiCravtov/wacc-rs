#![feature(trait_alias)]

extern crate core;

pub mod ast;
pub mod node;
mod nonempty;
pub mod parser;
pub mod source;
pub mod token;

pub(crate) mod private {
    // sealed traits support
    pub trait Sealed {}
    impl<T> Sealed for T {}
}

/// Namespace for crate-wide extension traits/methods
pub(crate) mod ext {
    use crate::private;
    use chumsky::{
        combinator::TryMapWith, error::LabelError, extra::ParserExtra, input::MapExtra,
        prelude::Input, DefaultExpected, Parser,
    };
    use extend::ext;

    /// Trait for holding all the [char] extension methods.
    #[ext(pub, name = CharExt, supertraits = private::Sealed)]
    impl char {
        /// Just like [char::to_string] but takes ownership of the character.
        fn to_string_owned(self) -> String {
            self.to_string()
        }

        /// Any ASCII character except `\`, `'` and `"`.
        fn normal_wacc_char(&self) -> bool {
            match self {
                '\\' | '\'' | '"' => false,
                _ => true,
            }
        }

        /// One of `0`, `b`, `t`, `n`, `f`, `r`, `"`, `'` or `\`.
        fn escaped_wacc_char(&self) -> bool {
            match self {
                '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' => true,
                _ => false,
            }
        }

        /// Looks up the character that a WACC-escaped character represents:
        ///
        /// | WACC-escaped character | ASCII Value | Description     |
        /// |------------------------|-------------|-----------------|
        /// | `0`                    | `0x00`      | null terminator |
        /// | `b`                    | `0x08`      | backspace       |
        /// | `t`                    | `0x09`      | tab             |
        /// | `n`                    | `0x0a`      | new line        |
        /// | `f`                    | `0x0c`      | form feed       |
        /// | `r`                    | `0x0d`      | carriage return |
        /// | `"`                    | `0x22`      | double quote    |
        /// | `'`                    | `0x27`      | single quote    |
        /// | `\`                    | `0x5c`      | backslash       |
        ///
        /// Source: WACC-language spec, Table 2.
        fn lookup_escaped_wacc_char(&self) -> Option<char> {
            match self {
                // Table-2 of the specification
                '0' => Some(0x00 as char),
                'b' => Some(0x08 as char),
                't' => Some(0x09 as char),
                'n' => Some(0x0a as char),
                'f' => Some(0x0c as char),
                'r' => Some(0x0d as char),
                '"' => Some(0x22 as char),
                '\'' => Some(0x27 as char),
                '\\' => Some(0x5c as char),
                _ => None,
            }
        }
    }

    /// Trait for holding all the [Parser] extension methods.
    #[ext(pub, name = ParserExt, supertraits = Sized + private::Sealed)]
    impl<'a, I, O, E, T> T
    where
        I: Input<'a>,
        E: ParserExtra<'a, I>,
        T: Parser<'a, I, O, E>,
    {
        /// Like [select] but applies to the output of a [Parser]; its internally implemented using
        /// the [try_map_with] combinator method.
        fn select_output<U, F>(
            self,
            f: F,
        ) -> TryMapWith<
            Self,
            O,
            impl Fn(O, &mut MapExtra<'a, '_, I, E>) -> Result<U, E::Error> + Clone,
        >
        where
            F: Fn(O, &mut MapExtra<'a, '_, I, E>) -> Option<U> + Clone,
        {
            self.try_map_with(move |x, extra| match f(x, extra) {
                None => Err(E::Error::expected_found(
                    Some(DefaultExpected::SomethingElse),
                    None,
                    extra.span(),
                )),
                Some(x) => Ok(x),
            })
        }
    }
}

/// Namespace for all the type/trait aliases used by this crate.
pub(crate) mod alias {
    use chumsky::extra;
    use chumsky::prelude::{Input, Rich};

    /// Trait alias for generic [chumsky::Parser] implementations used by the various parsers here
    pub trait Parser<'src, I, T> = chumsky::Parser<'src, I, T, extra::Full<Rich<'src, I::Token, I::Span>, (), ()>>
        + Clone
    where
        I: Input<'src>,
        I::Token: PartialEq;
}
