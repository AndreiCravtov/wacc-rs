use crate::nonempty::NonemptyArray;
use crate::source::{SourcedNode, SourcedSpan};
use internment::ArcIntern;
use std::{fmt, fmt::Debug, ops::Deref};
use thiserror::Error;

/// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: SN<StatChain>,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub return_type: SN<Type>,
    pub name: SN<Ident>,
    pub params: Box<[FuncParam]>,
    pub body: SN<FuncBody>,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub r#type: SN<Type>,
    pub name: SN<Ident>,
}

/// Wrapper around [StatChain] with the additional constraint that every execution path
/// must end with either a `return` or `exit` statement
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct FuncBody(StatChain);

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct StatChain(NonemptyArray<SN<Stat>>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatChain` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

impl StatChain {
    #[inline]
    fn new(stat: Stat, span: SourcedSpan) -> Self {
        Self(NonemptyArray::singleton(SN::new(stat, span)))
    }

    pub fn try_new(spanned_stats: Vec<(Stat, SourcedSpan)>) -> Result<Self, EmptyStatVecError> {
        match NonemptyArray::try_from_boxed_slice(
            spanned_stats
                .iter()
                .map(|(stat, span)| SN::new(stat.clone(), span.clone()))
                .collect::<Box<[_]>>(),
        ) {
            Ok(s) => Ok(Self(s)),
            Err(_) => Err(EmptyStatVecError),
        }
    }
}

impl From<(Stat, SourcedSpan)> for StatChain {
    #[inline]
    fn from((stat, span): (Stat, SourcedSpan)) -> Self {
        StatChain::new(stat, span)
    }
}

impl TryFrom<Vec<(Stat, SourcedSpan)>> for StatChain {
    type Error = EmptyStatVecError;

    #[inline]
    fn try_from(spanned_stats: Vec<(Stat, SourcedSpan)>) -> Result<Self, Self::Error> {
        StatChain::try_new(spanned_stats)
    }
}

#[derive(Clone, Debug)]
pub enum Stat {
    Skip,
    VarDefinition {
        r#type: SN<Type>,
        name: SN<Ident>,
        rhs: AssignRhs,
    },
    Assignment {
        lhs: AssignLhs,
        rhs: AssignRhs,
    },
    Read(AssignLhs),
    Free(SN<Expr>),
    Return(SN<Expr>),
    Exit(SN<Expr>),
    Print(SN<Expr>),
    Println(SN<Expr>),
    IfThenElse {
        if_cond: SN<Expr>,
        then_body: SN<StatChain>,
        else_body: SN<StatChain>,
    },
    WhileDo {
        while_cond: SN<Expr>,
        body: SN<StatChain>,
    },
    Scoped(SN<StatChain>),
}

#[derive(Clone, Debug)]
pub enum AssignLhs {
    Ident(SN<Ident>),
    ArrayElem(ArrayElem),
    PairElem(PairElem),
}

#[derive(Clone, Debug)]
pub enum AssignRhs {
    Expr(SN<Expr>),
    ArrayLiter(Box<[SN<Expr>]>),
    Newpair(SN<Expr>, SN<Expr>),
    PairElem(PairElem),
    Call {
        func_name: SN<Ident>,
        args: Box<[SN<Expr>]>,
    },
}

#[derive(Clone, Debug)]
pub enum PairElem {
    Fst(SN<Expr>),
    Snd(SN<Expr>),
}

#[derive(Clone, Debug)]
pub enum Type {
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
    PairType(PairElemType, PairElemType),

    // Generated only by parser errors.
    Error(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Int,
    Bool,
    Char,
    String,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub elem_type: Type,
}

impl ArrayType {
    #[inline]
    pub fn new(elem_type: Type) -> Self {
        Self { elem_type }
    }
}

#[derive(Clone, Debug)]
pub enum PairElemType {
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
    Pair(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Liter(Liter),
    Ident(Ident),
    ArrayElem(ArrayElem),
    Unary(SN<UnaryOper>, SN<Self>),
    Binary(SN<Self>, SN<BinaryOper>, SN<Self>),
    Paren(SN<Self>),

    // Generated only by parser errors.
    Error(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum Liter {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(ArcIntern<str>),
    PairLiter,
}

#[derive(Clone, Debug)]
pub enum UnaryOper {
    Not,
    Minus,
    Len,
    Ord,
    Chr,
}

#[derive(Clone, Debug)]
pub enum BinaryOper {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    And,
    Or,
}

impl BinaryOper {
    /// The precedence of binary operators in WACC, where lower
    /// is higher. Source: WACC-language spec, Table 4.
    fn precedence(&self) -> u8 {
        match self {
            BinaryOper::Mul => 1,
            BinaryOper::Div => 1,
            BinaryOper::Mod => 1,
            BinaryOper::Add => 2,
            BinaryOper::Sub => 2,
            BinaryOper::Gt => 3,
            BinaryOper::Gte => 3,
            BinaryOper::Lt => 3,
            BinaryOper::Lte => 3,
            BinaryOper::Eq => 4,
            BinaryOper::Neq => 4,
            BinaryOper::And => 5,
            BinaryOper::Or => 6,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(ArcIntern<str>);

impl Ident {
    #[inline]
    pub fn from_str(s: &str) -> Self {
        Ident(ArcIntern::from(s))
    }

    #[inline]
    pub fn from_boxed_str(s: Box<str>) -> Self {
        Ident(ArcIntern::from(s))
    }

    #[inline]
    pub fn from_string(s: String) -> Self {
        Ident::from_boxed_str(s.into_boxed_str())
    }
}

impl fmt::Display for Ident {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(&self.0, f)
    }
}

impl Deref for Ident {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub indices: NonemptyArray<SN<Expr>>,
}

impl ArrayElem {
    #[inline]
    pub fn new(array_name: SN<Ident>, indices: NonemptyArray<SN<Expr>>) -> Self {
        Self {
            array_name,
            indices,
        }
    }
}
