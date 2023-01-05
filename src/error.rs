use crate::source_loc::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    pub causes: Vec<ParseErrorCause>,
}

impl ParseError {
    pub fn new(causes: Vec<ParseErrorCause>) -> ParseError {
        ParseError { causes }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for cause in &self.causes {
            writeln!(f, "{}", cause)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseErrorCause {
    pub source_loc: SourceLoc,
    pub token: Option<String>,
    pub message: String,
}

impl ParseErrorCause {
    pub fn new(source_loc: SourceLoc, message: &str) -> ParseErrorCause {
        ParseErrorCause {
            source_loc,
            token: None,
            message: message.to_string(),
        }
    }

    pub fn new_with_location(
        source_loc: SourceLoc,
        token: &str,
        message: &str,
    ) -> ParseErrorCause {
        ParseErrorCause {
            source_loc,
            token: Some(token.to_string()),
            message: message.to_string(),
        }
    }
}

impl From<ParseErrorCause> for ParseError {
    fn from(error: ParseErrorCause) -> ParseError {
        ParseError {
            causes: vec![error],
        }
    }
}

impl std::fmt::Display for ParseErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(token) = &self.token {
            writeln!(
                f,
                "{}:{} at {token}: {}",
                self.source_loc.line, self.source_loc.column, self.message
            )
        } else {
            writeln!(
                f,
                "{}:{}: {}",
                self.source_loc.line, self.source_loc.column, self.message
            )
        }
    }
}
