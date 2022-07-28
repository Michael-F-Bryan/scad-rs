use std::{ops::Deref, sync::Arc};

/// A reference-counted string.
#[derive(Debug, Clone, Eq, Hash, PartialOrd, Ord)]
pub struct Text(Arc<str>);

impl Text {
    pub fn as_str(&self) -> &str {
        &*self
    }
}

impl Deref for Text {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S: Into<Arc<str>>> From<S> for Text {
    fn from(s: S) -> Self {
        Text(s.into())
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        &*self
    }
}

impl<S> PartialEq<S> for Text
where
    S: AsRef<str>,
{
    fn eq(&self, other: &S) -> bool {
        let s = &**self;
        s.eq(other.as_ref())
    }
}

impl PartialEq<Text> for str {
    fn eq(&self, other: &Text) -> bool {
        other.eq(&self)
    }
}
