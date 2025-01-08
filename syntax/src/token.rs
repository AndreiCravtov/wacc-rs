use crate::source::{SourceId, WithSourceId};
use crate::CharExt;
use chumsky::error::Rich;
use chumsky::input::WithContext;
use chumsky::prelude::{any, choice, end, just, regex, skip_then_retry_until, Input};
use chumsky::{extra, text, IterParser, Parser};
use internment::ArcIntern;
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
    StringLiter(ArcIntern<str>),

    // delimiter symbols
    Open(Delim),
    Close(Delim),

    // other symbols
    Lte,
    Lt,
    Gte,
    Gt,
    BangEquals,
    Bang,
    EqualsEquals,
    Equals,
    Plus,
    Minus,
    Star,
    Percent,
    ForwardSlash,
    And,
    Or,
    Semicolon,
    Comma,

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
            Token::Lte => write!(f, "<="),
            Token::Lt => write!(f, "<"),
            Token::Gte => write!(f, ">="),
            Token::Gt => write!(f, ">"),
            Token::BangEquals => write!(f, "!="),
            Token::Bang => write!(f, "!"),
            Token::EqualsEquals => write!(f, "=="),
            Token::Equals => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Percent => write!(f, "%"),
            Token::ForwardSlash => write!(f, "/"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
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
type SpannedLexerOutput<'src, S> = Vec<(Token<'src>, SourcedLexerSpan<'src, S>)>;
type LexerExtra<'src, S> = extra::Full<Rich<'src, char, SourcedLexerSpan<'src, S>>, (), ()>;

pub fn lexer<'src, S: SourceId + 'src>(
) -> impl Parser<'src, SourcedLexerInput<'src, S>, SpannedLexerOutput<'src, S>, LexerExtra<'src, S>>
{
    // WACC identifiers are C-style, so we can use the default `text::ident` parser
    let ident = text::ident().map(|id| Token::Ident(id));

    // copy the Regex pattern found in the WACC spec verbatim
    let int_liter = regex("[\\+-]?[0-9]+")
        .try_map(|s: &str, span| match s.parse::<i32>() {
            Ok(i) => Ok(i),
            Err(_) => Err(Rich::custom(
                span,
                format!("The integer literal '{}' does not fit within 32 bytes", s),
            )),
        })
        .map(Token::IntLiter);

    // character parser
    let character = choice((
        any().filter(char::normal_wacc_char),
        just('\\')
            .ignore_then(any())
            .filter(char::escaped_wacc_char),
    ));

    // character literal parser
    let char_delim = just('\'');
    let char_liter = character
        .delimited_by(char_delim, char_delim)
        .map(Token::CharLiter);

    // string literal parser
    let string_delim = just('"');
    let string_liter = character
        .repeated()
        .collect::<String>()
        .delimited_by(string_delim, string_delim)
        .map(ArcIntern::from)
        .map(Token::StringLiter);

    let delim_symbols = choice((
        just('(').to(Token::Open(Delim::Paren)),
        just(')').to(Token::Close(Delim::Paren)),
        just('[').to(Token::Open(Delim::Bracket)),
        just(']').to(Token::Close(Delim::Bracket)),
    ));

    let other_symbols = choice((
        // if some symbols are ambiguous, make sure to place the more 'long' ones ahead
        just("<=").to(Token::Lte),
        just('<').to(Token::Lt),
        just(">=").to(Token::Gte),
        just('>').to(Token::Gt),
        just("!=").to(Token::BangEquals),
        just('!').to(Token::Bang),
        just("==").to(Token::EqualsEquals),
        just('=').to(Token::Equals),
        // symbols without ambiguity
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('%').to(Token::Percent),
        just('/').to(Token::ForwardSlash),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
    ));

    let keywords = choice([
        text::keyword("begin").to(Token::Begin),
        text::keyword("end").to(Token::End),
        text::keyword("is").to(Token::Is),
        text::keyword("skip").to(Token::Skip),
        text::keyword("read").to(Token::Read),
        text::keyword("free").to(Token::Free),
        text::keyword("return").to(Token::Return),
        text::keyword("exit").to(Token::Exit),
        text::keyword("print").to(Token::Print),
        text::keyword("println").to(Token::Println),
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("fi").to(Token::Fi),
        text::keyword("while").to(Token::While),
        text::keyword("do").to(Token::Do),
        text::keyword("done").to(Token::Done),
        text::keyword("newpair").to(Token::Newpair),
        text::keyword("pair").to(Token::Pair),
        text::keyword("fst").to(Token::Fst),
        text::keyword("snd").to(Token::Snd),
        text::keyword("call").to(Token::Call),
        text::keyword("int").to(Token::Int),
        text::keyword("bool").to(Token::Bool),
        text::keyword("char").to(Token::Char),
        text::keyword("string").to(Token::String),
        text::keyword("len").to(Token::Len),
        text::keyword("ord").to(Token::Ord),
        text::keyword("chr").to(Token::Chr),
        text::keyword("null").to(Token::Null),
        text::keyword("true").to(Token::True),
        text::keyword("false").to(Token::False),
    ]);

    let token = choice((
        // parser literals first, as they have precedence over any other occurrences of symbols
        // e.g. the literal +14343 should take precedence over the plus symbol '+'
        int_liter,
        char_liter,
        string_liter,
        // parse symbols
        delim_symbols,
        other_symbols,
        // first parse keywords, only then parse identifiers as a fallback
        keywords,
        ident,
    ));

    // the comments are only single-line, started with a hashtag
    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|t, e| (t, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        // We must consume the entire source at the end
        .then_ignore(end())
}
