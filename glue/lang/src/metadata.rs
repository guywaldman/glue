#[derive(Debug, Clone, Default)]
pub struct SourceCodeMetadata<'a> {
    pub file_name: &'a str,
    pub file_contents: &'a str,
}
