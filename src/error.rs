use thiserror::Error;

/// Errors that can occur in fea-rs-ast operations.
#[derive(Debug, Clone, Error)]
pub enum Error {
    /// Cannot convert Statement to target type
    #[error("Cannot convert Statement to target type")]
    CannotConvert,
    /// Cannot load source file
    #[error("Cannot load source file: {0}")]
    CannotLoadSourceFile(#[from] fea_rs::parse::SourceLoadError),
    /// Errors encountered during feature parsing
    #[error("Errors encountered during feature parsing: {0:?}")]
    FeatureParsing(fea_rs::DiagnosticSet),
    /// Errors encountered building glyph order
    #[error("Errors encountered building glyph order: {0:?}")]
    GlyphOrderBuilding(#[from] fea_rs::compile::error::GlyphOrderError),
}
