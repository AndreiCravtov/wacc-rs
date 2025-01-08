use crate::SourceId::{SourceId, StrSourceId};
use ariadne::Span as AriadneSpan;
use chumsky::span::Span as ChumskySpan;
use std::fmt;
use std::ops::Range;

/// A span implementation with reference to the [SourceId] of the source being spanned.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourcedSpan<S: SourceId = StrSourceId> {
    /// The start offset of the span.
    pub start: usize,
    /// The end (exclusive) offset of the span.
    pub end: usize,
    pub source_id: S,
}

impl SourcedSpan {
    /// Create a [SourcedSpan] with zeroed start/end range, and empty string source ID.
    pub fn empty() -> Self {
        SourcedSpan {
            start: 0,
            end: 0,
            source_id: StrSourceId::empty(),
        }
    }
}

impl<S: SourceId> SourcedSpan<S> {
    pub fn new(start: usize, end: usize, source_id: S) -> Self {
        assert!(start <= end);
        Self {
            start,
            end,
            source_id,
        }
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn into_range(self) -> Range<usize> {
        self.start..self.end
    }

    pub fn union(self, other: Self) -> Self {
        assert!(
            self.source_id == other.source_id,
            "cannot union two spans from different sources"
        );
        SourcedSpan {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            ..self
        }
    }
}

impl<S: SourceId + fmt::Debug> fmt::Debug for SourcedSpan<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}..{:?}", self.source_id, self.start, self.end)
    }
}

impl<S: SourceId> ChumskySpan for SourcedSpan<S> {
    type Context = S;
    type Offset = usize;

    #[inline]
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        SourcedSpan::new(range.start, range.end, context)
    }

    fn context(&self) -> Self::Context {
        self.source_id.clone()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl<S: SourceId> AriadneSpan for SourcedSpan<S> {
    type SourceId = S;

    fn source(&self) -> &Self::SourceId {
        &self.source_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}
