#[derive(Default)]
#[salsa::database(crate::lowering::LoweringStorage, crate::parsing::ParsingStorage)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}
