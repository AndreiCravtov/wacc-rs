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

/// Namespace for all the extension traits/methods used by this crate
pub(crate) mod ext {
    use crate::private;
    use crate::source::{SourcedNode, SourcedSpan};
    use crate::token::{Delim, Token};
    use chumsky::combinator::{DelimitedBy, MapWith};
    use chumsky::extra::ParserExtra;
    use chumsky::input::{Input, MapExtra, ValueInput};
    use chumsky::prelude::{just, nested_delimiters, via_parser};
    use chumsky::primitive::Just;
    use chumsky::recovery::{RecoverWith, ViaParser};
    use chumsky::Parser;

    /// Trait for holding all the [char] extension methods.
    pub(crate) trait CharExt: private::Sealed + Sized {
        /// Just like [char::to_string] but takes ownership of the character.
        fn to_string_owned(self) -> String;

        /// Any ASCII character except `\`, `'` and `"`.
        fn normal_wacc_char(&self) -> bool;

        /// One of `0`, `b`, `t`, `n`, `f`, `r`, `"`, `'` or `\`.
        fn escaped_wacc_char(&self) -> bool;

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
        fn lookup_escaped_wacc_char(&self) -> Option<Self>;
    }

    impl CharExt for char {
        fn to_string_owned(self) -> String {
            self.to_string()
        }

        fn normal_wacc_char(&self) -> bool {
            match self {
                '\\' | '\'' | '"' => false,
                _ => true,
            }
        }

        fn escaped_wacc_char(&self) -> bool {
            match self {
                '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' => true,
                _ => false,
            }
        }

        fn lookup_escaped_wacc_char(&self) -> Option<Self> {
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

    pub(crate) trait SourcedNodeParserExt<'src, I: Input<'src>, O, E: ParserExtra<'src, I>>:
        Parser<'src, I, O, E> + private::Sealed + Sized
    {
        /// Convenience method to wrap items in [SourcedNode] type.
        fn sn(self) -> MapWith<Self, O, fn(O, &mut MapExtra<'src, '_, I, E>) -> SourcedNode<O>>;
    }

    impl<'src, I, O, E, P> SourcedNodeParserExt<'src, I, O, E> for P
    where
        I: Input<'src, Token = Token, Span = SourcedSpan>,
        E: ParserExtra<'src, I>,
        P: Parser<'src, I, O, E>,
    {
        #[inline]
        fn sn(self) -> MapWith<Self, O, fn(O, &mut MapExtra<'src, '_, I, E>) -> SourcedNode<O>> {
            self.map_with(|t, e| SourcedNode::new(t, e.span()))
        }
    }

    pub(crate) trait DelimByParserExt<'src, I: Input<'src>, O, E: ParserExtra<'src, I>>:
        Parser<'src, I, O, E> + private::Sealed + Sized
    {
        /// Convenience method to delimit a parser pattern by a [Delim].
        fn delim_by(
            self,
            delim: Delim,
        ) -> DelimitedBy<Self, Just<Token, I, E>, Just<Token, I, E>, Token, Token>;
    }

    impl<'src, I, O, E, P> DelimByParserExt<'src, I, O, E> for P
    where
        I: Input<'src, Token = Token>,
        E: ParserExtra<'src, I>,
        P: Parser<'src, I, O, E>,
    {
        #[inline]
        fn delim_by(
            self,
            delim: Delim,
        ) -> DelimitedBy<Self, Just<Token, I, E>, Just<Token, I, E>, Token, Token> {
            self.delimited_by(just(Token::Open(delim)), just(Token::Close(delim)))
        }
    }

    pub(crate) trait RecoverWithDelimParserExt<'src, I: Input<'src>, O, E: ParserExtra<'src, I>>:
        Parser<'src, I, O, E> + private::Sealed + Sized
    {
        /// Convenience method to attempt to recover error recover by searching for the end
        /// of a [Delim] delimiter, while respecting any nested delimiter structure.

        fn recover_with_delim<F>(
            self,
            delim: Delim,
            fallback: F,
        ) -> RecoverWith<Self, ViaParser<impl Parser<'src, I, O, E> + Clone + Sized>>
        where
            F: Fn(I::Span) -> O + Clone;
    }

    impl<'src, I, O, E, P> RecoverWithDelimParserExt<'src, I, O, E> for P
    where
        I: ValueInput<'src, Token = Token>,
        E: ParserExtra<'src, I>,
        P: Parser<'src, I, O, E>,
    {
        #[inline]
        fn recover_with_delim<F>(
            self,
            delim: Delim,
            fallback: F,
        ) -> RecoverWith<Self, ViaParser<impl Parser<'src, I, O, E> + Clone + Sized>>
        where
            F: Fn(I::Span) -> O + Clone,
        {
            self.recover_with(via_parser(nested_delimiters(
                Token::Open(delim),
                Token::Close(delim),
                Delim::variants_except(delim).map(|d| (Token::Open(d), Token::Close(d))),
                fallback,
            )))
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

pub(crate) mod lib {}
