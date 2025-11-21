use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub struct CannotConvertError;
impl Display for CannotConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Cannot convert Statement to target type")
    }
}

impl Error for CannotConvertError {
    fn description(&self) -> &str {
        "Cannot convert Statement to target type"
    }
}
