use im::HashSet;
use rowan::TextRange;

use crate::Text;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: Text,
    pub location: TextRange,
}

/// How severe a diagnostic is.
///
/// ```rust
/// use scad::Severity;
///
/// assert!(Severity::Bug > Severity::Error);
/// assert!(Severity::Error > Severity::Warning);
/// assert!(Severity::Warning > Severity::Hint);
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Severity {
    /// A suggestion.
    Hint,
    /// Something that isn't actually an error, but probably not desired
    /// behaviour.
    Warning,
    /// The code is incorrect.
    Error,
    /// An internal error.
    Bug,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostics(HashSet<Diagnostic>);

impl Diagnostics {
    pub fn empty() -> Diagnostics {
        Diagnostics::default()
    }

    pub fn push(&mut self, diag: impl IntoDiagnostic) {
        self.0.insert(diag.into_diagnostic());
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Diagnostic> + '_ {
        self.0.iter()
    }

    pub fn has_severity(&self, severity: Severity) -> bool {
        self.iter().any(|diag| diag.severity >= severity)
    }

    pub fn has_errors(&self) -> bool {
        self.has_severity(Severity::Error)
    }

    pub fn has_warnings(&self) -> bool {
        self.has_severity(Severity::Warning)
    }

    pub fn merge(self: &mut Diagnostics, other: Diagnostics) {
        self.0.extend(other.0);
    }
}

impl FromIterator<Diagnostic> for Diagnostics {
    fn from_iter<T: IntoIterator<Item = Diagnostic>>(iter: T) -> Self {
        Diagnostics(iter.into_iter().collect())
    }
}

impl Extend<Diagnostic> for Diagnostics {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl IntoIterator for Diagnostics {
    type Item = Diagnostic;
    type IntoIter = <HashSet<Diagnostic> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Diagnostics {
    type Item = &'a Diagnostic;
    type IntoIter = <&'a HashSet<Diagnostic> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

pub trait IntoDiagnostic: Sized {
    fn into_diagnostic(self) -> Diagnostic;
}

impl IntoDiagnostic for Diagnostic {
    fn into_diagnostic(self) -> Diagnostic {
        self
    }
}
