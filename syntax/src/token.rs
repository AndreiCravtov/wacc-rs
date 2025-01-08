use crate::source::{SourceId, WithSourceId};
use chumsky::error::Rich;
use chumsky::extra::ParserExtra;
use chumsky::input::WithContext;
use chumsky::prelude::{any, Input};
use chumsky::{IterParser, Parser};
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Paren,
    Bracket,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token<'src> {
    // literals
    Ident(&'src str),
    IntLiter(i32),
    CharLiter(char),
    StringLiter(&'src str),

    // delimiters
    Open(Delim),
    Close(Delim),

    // other symbols
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Semicolon,
    Comma,
    Star,
    Percent,
    Plus,
    Minus,
    ForwardSlash,
    Bang,
    Equals,
    BangEquals,
    EqualsEquals,

    // keywords
    Begin,
    End,
    Is,
    Skip,
    Read,
    Free,
    Return,
    Exit,
    Print,
    Println,
    If,
    Then,
    Else,
    Fi,
    While,
    Do,
    Done,
    Newpair,
    Pair,
    Fst,
    Snd,
    Call,
    Int,
    Bool,
    Char,
    String,
    Len,
    Ord,
    Chr,
    Null,
    True,
    False,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{}", s),
            Token::IntLiter(i) => write!(f, "{}", i),
            Token::CharLiter(c) => write!(f, "{}", c), // TODO: unescape the character literal, i.e. newline -> '\c'
            Token::StringLiter(s) => write!(f, "{}", s), // TODO: unescape the string literal, as with char literal
            Token::Open(d) => match d {
                Delim::Paren => write!(f, "("),
                Delim::Bracket => write!(f, "["),
            },
            Token::Close(d) => match d {
                Delim::Paren => write!(f, ")"),
                Delim::Bracket => write!(f, "]"),
            },
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Lte => write!(f, "<="),
            Token::Gte => write!(f, ">="),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Star => write!(f, "*"),
            Token::Percent => write!(f, "%"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::ForwardSlash => write!(f, "/"),
            Token::Bang => write!(f, "!"),
            Token::Equals => write!(f, "="),
            Token::BangEquals => write!(f, "!="),
            Token::EqualsEquals => write!(f, "=="),
            Token::Begin => write!(f, "begin"),
            Token::End => write!(f, "end"),
            Token::Is => write!(f, "is"),
            Token::Skip => write!(f, "skip"),
            Token::Read => write!(f, "read"),
            Token::Free => write!(f, "free"),
            Token::Return => write!(f, "return"),
            Token::Exit => write!(f, "exit"),
            Token::Print => write!(f, "print"),
            Token::Println => write!(f, "println"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Fi => write!(f, "fi"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::Done => write!(f, "done"),
            Token::Newpair => write!(f, "newpair"),
            Token::Pair => write!(f, "pair"),
            Token::Fst => write!(f, "fst"),
            Token::Snd => write!(f, "snd"),
            Token::Call => write!(f, "call"),
            Token::Int => write!(f, "int"),
            Token::Bool => write!(f, "bool"),
            Token::Char => write!(f, "char"),
            Token::String => write!(f, "string"),
            Token::Len => write!(f, "len"),
            Token::Ord => write!(f, "ord"),
            Token::Chr => write!(f, "chr"),
            Token::Null => write!(f, "null"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

type LexerInput<'src> = &'src str;
type SourcedLexerSpan<'src, S> = WithSourceId<S, <LexerInput<'src> as Input<'src>>::Span>;
type SourcedLexerInput<'src, S> = WithContext<SourcedLexerSpan<'src, S>, LexerInput<'src>>;

pub fn lexer<'src, S, E>(
) -> impl Parser<'src, SourcedLexerInput<'src, S>, Vec<(Token<'src>, SourcedLexerSpan<'src, S>)>, E>
where
    S: SourceId + 'src,
    E: ParserExtra<
        'src,
        SourcedLexerInput<'src, S>,
        Error = Rich<'src, char, SourcedLexerSpan<'src, S>>,
    >,
{
    any()
        .map_with(|t, extra| (Token::Null, extra.span()))
        .repeated()
        .collect()
}
