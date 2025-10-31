#[derive(Debug, Clone, Default)]
pub struct SourceCodeMetadata<'a> {
    pub file_name: &'a str,
    pub contents: &'a str,
}
