use crate::source::{SourcedNode, SourcedSpan};
use internment::ArcIntern;
use std::slice;
use thiserror::Error;

/// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Box<[Func]>,
    pub body: SN<StatChain>,
}

#[derive(Clone, Debug)] // additional constraint that every execution path through body of function must end with return or exit statement
pub struct Func {
    pub return_type: SN<Type>,
    pub name: SN<Ident>,
    pub params: Box<[FuncParam]>,
    pub body: SN<StatChain>,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub param_type: SN<Type>,
    pub name: SN<Ident>,
}

#[derive(Clone, Debug)]
pub struct StatChain(Box<[SN<Stat>]>);

#[derive(Error, Debug)]
#[error("Cannot convert to `StatChain` because the supplied `Vec<Stat> is empty")]
pub struct EmptyStatVecError;

impl StatChain {
    #[inline]
    fn new(stat: Stat, span: SourcedSpan) -> Self {
        Self(Box::new([SN::new(stat, span)]))
    }

    pub(crate) fn try_new(
        spanned_stats: Vec<(Stat, SourcedSpan)>,
    ) -> Result<Self, EmptyStatVecError> {
        match spanned_stats.is_empty() {
            true => Err(EmptyStatVecError),
            false => Ok(Self(
                spanned_stats
                    .iter()
                    .map(|(stat, span)| SN::new(stat.clone(), span.clone()))
                    .collect(),
            )),
        }
    }

    #[inline]
    pub fn first(&self) -> &Stat {
        &self.0[0]
    }

    #[inline]
    pub fn last(&self) -> &Stat {
        &self.0[self.0.len() - 1]
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, SN<Stat>> {
        self.0.iter()
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
        var_type: SN<Type>,
        name: SN<Ident>,
        value: AssignRhs,
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
    PairType(SN<PairElemType>, SN<PairElemType>),
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
    pub elem_type: SN<Type>,
}

#[derive(Clone, Debug)]
pub enum PairElemType {
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
    Pair,
}

#[derive(Clone, Debug)]
pub enum Expr {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(ArcIntern<str>),
    PairLiter,
    Ident(Ident),
    ArrayElem(ArrayElem),
    Unary(SN<UnaryOper>, SN<Self>),
    Binary(SN<Self>, SN<BinaryOper>, SN<Self>),
    Paren(SN<Self>),

    // Generated only by parser errors.
    Error,
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

#[derive(Clone, Debug)]
pub struct Ident(pub ArcIntern<str>);

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub first_index: SN<Expr>,
    pub other_indices: Box<[SN<Expr>]>,
}
