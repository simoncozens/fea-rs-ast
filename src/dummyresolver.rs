pub(crate) struct DummyResolver;
impl fea_rs::parse::SourceResolver for DummyResolver {
    fn get_contents(
        &self,
        path: &std::path::Path,
    ) -> Result<std::sync::Arc<str>, fea_rs::parse::SourceLoadError> {
        Err(fea_rs::parse::SourceLoadError::new(
            path.to_path_buf(),
            "DummyResolver cannot load files",
        ))
    }
}
