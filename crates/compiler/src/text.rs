use std::{borrow::Borrow, ops::Deref, sync::Arc};

/// A reference-counted string.
#[derive(Debug, Clone, Eq, Hash, PartialOrd, Ord)]
pub struct Text(Arc<str>);

impl Text {
    pub fn new(s: &str) -> Self {
        Text(s.into())
    }

    /// Get a reference to the underlying [`str`].
    pub fn as_str(&self) -> &str {
        self
    }
}

impl Borrow<str> for Text {
    fn borrow(&self) -> &str {
        self
    }
}

impl Deref for Text {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<&Text> for Text {
    fn from(text: &Text) -> Self {
        text.clone()
    }
}

impl From<&rowan::SyntaxText> for Text {
    fn from(s: &rowan::SyntaxText) -> Self {
        s.to_string().into()
    }
}

impl From<rowan::SyntaxText> for Text {
    fn from(s: rowan::SyntaxText) -> Self {
        Text::from(&s)
    }
}

impl From<&str> for Text {
    fn from(s: &str) -> Self {
        Text::new(s)
    }
}

impl From<String> for Text {
    fn from(s: String) -> Self {
        Text(s.into())
    }
}

impl From<Box<str>> for Text {
    fn from(s: Box<str>) -> Self {
        Text(s.into())
    }
}

impl From<Arc<str>> for Text {
    fn from(s: Arc<str>) -> Self {
        Text(s)
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        self
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
