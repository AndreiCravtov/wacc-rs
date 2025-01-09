use std::{ops, slice};
use thiserror::Error;

/// A pointer to a non-empty fixed-size slice allocated on the heap.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NonemptyArray<T>(Box<[T]>);

#[derive(Error, Debug)]
#[error("Cannot create to `NonemptyArray` because the supplied slice is empty")]
pub struct EmptySliceError;

impl<T> NonemptyArray<T> {
    #[inline]
    pub fn singleton(value: T) -> Self {
        Self(Box::new([value]))
    }

    #[inline]
    pub fn try_from_boxed_slice<S: Into<Box<[T]>>>(
        boxed_slice: S,
    ) -> Result<Self, EmptySliceError> {
        let boxed_slice = boxed_slice.into();
        match boxed_slice.is_empty() {
            true => Err(EmptySliceError),
            false => Ok(Self(boxed_slice)),
        }
    }

    #[inline]
    pub fn into_boxed_slice(self) -> Box<[T]> {
        self.0
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.0
    }

    #[inline]
    pub fn first(&self) -> &T {
        &self.0[0]
    }

    #[inline]
    pub fn last(&self) -> &T {
        &self.0[self.0.len() - 1]
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.0.iter()
    }
}

impl<T> ops::Index<usize> for NonemptyArray<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}
