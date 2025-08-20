use thiserror::Error;

#[derive(Error, Debug)]
pub enum RealityError {
    // Parsing errors
    #[error("Unexpected token encountered during parsing.")]
    UnexpectedToken,

    #[error("Unexpected operator {0} encountered during parsing.")]
    UnexpectedOperator(String),

    #[error("This reserved keyword {0} was encountered.")]
    ReservedKeyword(String),

    #[error("Unexpected end of block encountered.")]
    UnexpectedEndOfBlock,
    
    #[error("Unexpected end of file encountered.")]
    UnexpectedEndOfFile,

    #[error("Expected token {0} was not found.")]
    ExpectedToken(String),

    #[error("Internal error occurred: {0}")]
    InternalError(String),

    #[error("Internal parsing error occurred.")]
    InternalParsingError,
}
