use std::fmt::{Debug, Display};

use reality_ast::internal::types::Type;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum RealityError {
    // Parsing errors
    #[error("Unexpected operator {0} encountered during parsing.")]
    UnexpectedOperator(String),

    #[error("This reserved keyword {0} was encountered.")]
    ReservedKeyword(String),

    #[error("Unexpected end of block encountered.")]
    UnexpectedEndOfBlock,

    #[error("Unexpected end of file encountered.")]
    UnexpectedEndOfFile,

    #[error("Expected {0} was not found.")]
    ExpectedToken(String),

    #[error("Internal error occurred: {0}")]
    InternalError(String),

    #[error("Module not found: {0}")]
    ModuleNotFound(String),

    #[error("Cycle detected: {0}")]
    CycleDetected(String),

    #[error("Should not reach a require statement at this point")]
    NoRequireStatement,

    #[error("Type mismatch between {0} and {1}")]
    TypeMismatch(Type<String>, Type<String>),

    #[error("Signed integer {0} cannot be casted to unsigned {1}")]
    UnsignedIntegerMismatch(Type<String>, Type<String>),

    #[error("Variable {0} not found")]
    VariableNotFound(String),

    #[error("Expected function type but found {0}")]
    ExpectedFunction(Type<String>),

    #[error("Argument count mismatch: expected {expected}, found {found}")]
    ArgumentCountMismatch { expected: usize, found: usize },

    #[error("Should not reach a require statement at this point")]
    NoModuleDeclaration,

    #[error("Infinite type detected: {0}")]
    InfiniteType(String),

    #[error("Unbound generics found in function body {0}")]
    UnboundGenerics(Generics),

    #[error("No field {field} found in structure {structure}")]
    NoFieldInStructure { structure: String, field: String },

    #[error("Expected structure type but found {0}")]
    NotAStructure(Type<String>),

    #[error("Expected {expected} type arguments but found {found}")]
    WrongNumberOfTypeArguments {
        expected: usize,
        found: usize,
    },

    #[error("Unknown structure {0}")]
    UnknownStructure(String),
}

#[derive(Clone)]
pub struct Generics(pub Vec<String>);

impl Display for Generics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl Debug for Generics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

pub fn report_error<'a>(
    file: &'a str,
    location: (usize, usize),
    err: RealityError,
) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

    let mut color_gen = ColorGenerator::default();
    let color = color_gen.next();

    let error_message = format!("{}", err);

    let file_content = std::fs::read_to_string(file).unwrap_or_default();

    Report::build(ReportKind::Error, (file, location.0..location.1))
        .with_message(error_message)
        .with_label(
            Label::new((file, location.0..location.1))
                .with_message("Error occurred here")
                .with_color(color),
        )
        .finish()
        .print((file, Source::from(file_content)))
        .unwrap();
}
