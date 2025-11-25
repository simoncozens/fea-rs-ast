use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("Cannot convert Statement to target type")]
    CannotConvert,
    #[error("Cannot load source file: {0}")]
    CannotLoadSourceFile(#[from] fea_rs::parse::SourceLoadError),
    #[error("Errors encountered during feature parsing: {0:?}")]
    FeatureParsing(fea_rs::DiagnosticSet),
}
