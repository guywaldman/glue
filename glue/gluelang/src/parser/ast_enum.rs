#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    /// Name used in the generated code, which may differ from the original name.
    pub effective_name: Option<String>,
    pub doc: Option<String>,
    pub variants: Vec<String>,
}
