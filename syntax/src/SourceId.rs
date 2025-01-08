use internment::Intern;
use std::fmt;
use std::ops::Deref;
use std::path::Path;

/// Trait for identifiers which uniquely refer to a source. In the simplest case,
/// it is just the fully qualified file path.
pub trait SourceId: Clone + PartialEq + ToOwned {}

/// A source which is identified by a string, most commonly a file path.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct StrSourceId(Intern<str>);

impl SourceId for StrSourceId {}

impl StrSourceId {
    pub fn empty() -> Self {
        StrSourceId(Intern::from(""))
    }

    pub fn repl() -> Self {
        StrSourceId(Intern::from("repl"))
    }

    pub fn from_str(s: &str) -> Self {
        StrSourceId(Intern::from(s))
    }

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

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
