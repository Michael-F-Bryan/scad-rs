use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::BitOr,
};

use crate::SyntaxKind;

/// An efficient set of [`SyntaxKind`]s, backed by a bitset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const EMPTY: TokenSet = TokenSet(0);

    pub const fn single(kind: SyntaxKind) -> Self {
        TokenSet(mask(kind))
    }

    pub const fn new<const N: usize>(kinds: [SyntaxKind; N]) -> Self {
        let mut set = 0;
        let mut i = 0;

        while i < kinds.len() {
            set = set | mask(kinds[i]);
            i += 1;
        }

        TokenSet(set)
    }

    pub const fn with(self, kind: SyntaxKind) -> Self {
        let other = TokenSet::single(kind);
        TokenSet(self.0 | other.0)
    }

    pub const fn contains(self, kind: SyntaxKind) -> bool {
        (self.0 & mask(kind)) != 0
    }

    pub fn iter(self) -> impl Iterator<Item = SyntaxKind> {
        SyntaxKind::VARIANTS
            .iter()
            .copied()
            .filter(move |&kind| self.contains(kind))
    }
}

impl Display for TokenSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let joined = self
            .iter()
            .map(|k| format!("{k:?}"))
            .collect::<Vec<_>>()
            .join("|");
        f.write_str(&joined)
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1_u128 << kind as u16
}

impl BitOr<SyntaxKind> for TokenSet {
    type Output = TokenSet;

    fn bitor(self, rhs: SyntaxKind) -> Self::Output {
        self.with(rhs)
    }
}

impl BitOr<SyntaxKind> for SyntaxKind {
    type Output = TokenSet;

    fn bitor(self, rhs: SyntaxKind) -> Self::Output {
        TokenSet::single(self).with(rhs)
    }
}

impl BitOr for TokenSet {
    type Output = TokenSet;

    fn bitor(self, rhs: TokenSet) -> Self::Output {
        TokenSet(self.0 | rhs.0)
    }
}

impl From<SyntaxKind> for TokenSet {
    fn from(k: SyntaxKind) -> Self {
        TokenSet::single(k)
    }
}
