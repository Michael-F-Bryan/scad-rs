use im::Vector;

use crate::Text;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Id(Vector<Text>);

#[derive(Debug, PartialEq)]
pub struct Identifiers {
    segments: Vector<Text>,
}

impl Identifiers {
    pub fn root() -> Self {
        Identifiers {
            segments: Vector::new(),
        }
    }

    pub fn enter(&self, scope: impl Into<Text>) -> Self {
        let mut segments = self.segments.clone();
        segments.push_back(scope.into());
        Identifiers { segments }
    }

    pub fn create(&self, name: impl Into<Text>) -> Id {
        let mut segments = self.segments.clone();
        segments.push_back(name.into());
        Id(segments)
    }
}
