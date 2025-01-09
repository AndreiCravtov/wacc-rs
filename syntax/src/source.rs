use crate::node::Node;
use ariadne::Span as AriadneSpan;
use chumsky::prelude::SimpleSpan;
use chumsky::span::Span as ChumskySpan;
use internment::Intern;
use std::fmt;
use std::ops::{Deref, Range};
use std::path::Path;

/// Trait for identifiers which uniquely refer to a source. In the simplest case,
/// it is just the fully qualified file path.
pub trait SourceId: Clone + PartialEq + ToOwned {}

/// A source which is identified by a string, most commonly a file path.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct StrSourceId(Intern<str>);

impl StrSourceId {
    #[inline]
    pub fn empty() -> Self {
        StrSourceId(Intern::from(""))
    }

    #[inline]
    pub fn repl() -> Self {
        StrSourceId(Intern::from("repl"))
    }

    #[inline]
    pub fn from_str(s: &str) -> Self {
        StrSourceId(Intern::from(s))
    }

    #[inline]
    pub fn from_boxed_str(s: Box<str>) -> StrSourceId {
        StrSourceId(Intern::from(s))
    }

    #[inline]
    pub fn from_string(s: String) -> Self {
        StrSourceId::from_boxed_str(s.into_boxed_str())
    }

    #[inline]
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        StrSourceId::from_string(path.as_ref().to_string_lossy().into_owned())
    }

    #[inline]

    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]

    pub fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SourceId for StrSourceId {}

impl fmt::Debug for StrSourceId {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        StrSourceId::fmt(self, f)
    }
}

impl fmt::Display for StrSourceId {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        StrSourceId::fmt(self, f)
    }
}

impl From<&str> for StrSourceId {
    #[inline]
    fn from(value: &str) -> Self {
        Self::from_str(value)
    }
}

impl From<Box<str>> for StrSourceId {
    #[inline]
    fn from(value: Box<str>) -> Self {
        Self::from_boxed_str(value)
    }
}

impl From<String> for StrSourceId {
    #[inline]
    fn from(value: String) -> Self {
        Self::from_string(value)
    }
}

impl Deref for StrSourceId {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<StrSourceId> for &StrSourceId {
    #[inline]
    fn as_ref(&self) -> &StrSourceId {
        self
    }
}

/// Spans which have a [SourceId] attached to them
pub trait SourceIdSpan: ChumskySpan<Offset = usize> {
    type SourceId: SourceId;

    fn source_id(&self) -> &Self::SourceId;
}

/// A span implementation with reference to the [SourceId] of the source being spanned.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct WithSourceId<SourceIdT = StrSourceId, SpanT = SimpleSpan>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    source_id: SourceIdT,
    span: SpanT,
}

impl<SourceIdT, SpanT> WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    #[inline]
    pub fn new(source_id: SourceIdT, span: SpanT) -> Self {
        Self { source_id, span }
    }

    #[inline]
    fn start(&self) -> usize {
        self.span.start()
    }

    #[inline]
    fn end(&self) -> usize {
        self.span.end()
    }

    #[inline]
    pub fn source_id(&self) -> &SourceIdT {
        &self.source_id
    }

    #[inline]
    pub fn as_range(&self) -> Range<usize> {
        self.span.start()..self.span.end()
    }

    #[inline]
    pub fn into_range(self) -> Range<usize> {
        self.span.start()..self.span.end()
    }
}

impl<SourceIdT, SpanT> fmt::Debug for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId + fmt::Debug,
    SpanT: ChumskySpan<Offset = usize> + fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.source_id, self.span)
    }
}

impl<SourceIdT, SpanT> fmt::Display for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId + fmt::Display,
    SpanT: ChumskySpan<Offset = usize> + fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.source_id, self.span)
    }
}

impl<SourceIdT, SpanT> ChumskySpan for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type Context = (SourceIdT, SpanT::Context);
    type Offset = usize;

    #[inline]
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        WithSourceId::new(context.0, SpanT::new(context.1, range))
    }

    #[inline]
    fn context(&self) -> Self::Context {
        (self.source_id.clone(), self.span.context())
    }

    #[inline]
    fn start(&self) -> Self::Offset {
        WithSourceId::start(&self)
    }

    #[inline]
    fn end(&self) -> Self::Offset {
        WithSourceId::end(&self)
    }
}

impl<SourceIdT, SpanT> SourceIdSpan for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type SourceId = SourceIdT;

    #[inline]
    fn source_id(&self) -> &Self::SourceId {
        WithSourceId::source_id(self)
    }
}

impl<SourceIdT, SpanT> AriadneSpan for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type SourceId = SourceIdT;

    #[inline]
    fn source(&self) -> &Self::SourceId {
        &self.source_id
    }

    #[inline]
    fn start(&self) -> usize {
        self.span.start()
    }

    #[inline]
    fn end(&self) -> usize {
        self.span.end()
    }
}

pub type SourcedSpan = WithSourceId<StrSourceId, SimpleSpan>;
pub type SourcedNode<T> = Node<T, SourcedSpan>;

impl<T> SourcedNode<T> {
    #[inline]
    pub fn source_id(&self) -> StrSourceId {
        self.context().source_id
    }

    #[inline]
    pub fn span(&self) -> SourcedSpan {
        self.context().clone()
    }
}
