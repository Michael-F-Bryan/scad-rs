use im::Vector;

use crate::hir::Text;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Id(Vector<Segment>);

impl Id {
    pub(crate) fn root() -> Self {
        Id(Vector::new())
    }

    pub(crate) fn with_function(&self, name: impl Into<Text>) -> Self {
        let mut id = self.clone();
        id.0.push_back(Segment::Function(name.into()));
        id
    }

    pub(crate) fn with_module(&self, name: impl Into<Text>) -> Self {
        let mut id = self.clone();
        id.0.push_back(Segment::Module(name.into()));
        id
    }

    pub(crate) fn with_index(&self, index: usize) -> Self {
        let mut id = self.clone();
        id.0.push_back(Segment::Index(index));
        id
    }

    pub(crate) fn with_cfg_node(&self, index: usize) -> Self {
        let mut id = self.clone();
        id.0.push_back(Segment::CfgNode(index));
        id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Segment {
    Function(Text),
    Module(Text),
    Index(usize),
    CfgNode(usize),
}
