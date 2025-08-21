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
    NoRequireStatement
}

pub fn report_error<'a>(
    file: &'a str,
    input: &'a str,
    location: (usize, usize),
    err: RealityError,
) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

    let mut color_gen = ColorGenerator::default();
    let color = color_gen.next();

    let error_message = format!("{}", err);

    Report::build(ReportKind::Error, (file, location.0..location.1))
        .with_message(error_message)
        .with_label(
            Label::new((file, location.0..location.1))
                .with_message("Error occurred here")
                .with_color(color),
        )
        .finish()
        .print((file, Source::from(input)))
        .unwrap();
}
