// cSpell: ignore peekable
use std::cmp::Ordering;
use std::collections::HashMap;
use std::iter::Peekable;
use std::mem;

use lazy_static::lazy_static;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

use crate::error::*;
use crate::source_loc::*;
use crate::token::*;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        use crate::token::TokenType::*;
        m.insert("and", And);
        m.insert("break", Break);
        m.insert("class", Class);
        m.insert("else", Else);
        m.insert("false", False);
        m.insert("for", For);
        m.insert("fun", Fun);
        m.insert("if", If);
        m.insert("nil", Nil);
        m.insert("or", Or);
        m.insert("print", Print);
        m.insert("return", Return);
        m.insert("super", Super);
        m.insert("this", This);
        m.insert("true", True);
        m.insert("var", Var);
        m.insert("while", While);

        m
    };
}

#[derive(Clone)]
pub struct Scanner<'source, 'g> {
    source: &'source str,
    tokens: Vec<Token<'source>>,
    errors: Vec<ParseErrorCause>,
    indentation: Vec<u16>,
    grapheme_indices: Peekable<GraphemeIndices<'g>>,
    start: usize,
    current: usize,
    line: u32,
    column: u16,
    start_column: u16,
    eof: bool,
    // Beginning of line spaces used for indentation.
    bol_spaces: u16,
    is_bol: bool,
    is_line_continuation: bool,
    following_line_continuation: bool,
}

impl<'source, 'g> Scanner<'source, 'g>
where
    'source: 'g,
{
    pub fn new(source: &'source str) -> Scanner<'source, 'g> {
        Scanner {
            source,
            grapheme_indices: source.grapheme_indices(true).peekable(),
            tokens: Vec::new(),
            errors: Vec::new(),
            indentation: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_column: 1,
            eof: false,
            bol_spaces: 0,
            is_bol: true,
            is_line_continuation: false,
            following_line_continuation: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ParseError> {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.peek_index();
            self.scan_token();
        }

        self.start_column = self.column.saturating_sub(1);
        for _ in 0..self.indentation.len() {
            self.add_token(TokenType::RightBrace);
        }
        self.add_token(TokenType::Eof);

        let tokens = mem::take(&mut self.tokens);
        let errors = mem::take(&mut self.errors);

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(ParseError::new(errors))
        }
    }

    fn scan_token(&mut self) {
        self.start_column = self.column;
        match self.advance() {
            None => (),
            Some((_, grapheme_cluster)) => {
                use crate::token::TokenType::*;
                match grapheme_cluster {
                    "(" => {
                        self.bol_indentation_tokens();
                        self.add_token(LeftParen);
                    }
                    ")" => {
                        self.bol_indentation_tokens();
                        self.add_token(RightParen);
                    }
                    "{" => {
                        self.bol_indentation_tokens();
                        self.add_token(LeftBrace);
                    }
                    "}" => {
                        self.bol_indentation_tokens();
                        self.add_token(RightBrace);
                    }
                    "," => {
                        self.bol_indentation_tokens();
                        self.add_token(Comma);
                    }
                    "." => {
                        self.bol_indentation_tokens();
                        self.add_token(Dot);
                    }
                    "-" => {
                        self.bol_indentation_tokens();
                        self.add_token(Minus);
                        self.following_line_continuation = true;
                    }
                    "+" => {
                        self.bol_indentation_tokens();
                        self.add_token(Plus);
                        self.following_line_continuation = true;
                    }
                    ";" => {
                        self.bol_indentation_tokens();
                        self.add_token(Semicolon);
                    }
                    "*" => {
                        self.bol_indentation_tokens();
                        self.add_token(Star);
                        self.following_line_continuation = true;
                    }
                    "!" => {
                        self.bol_indentation_tokens();
                        if self.matches("=") {
                            self.add_token(BangEqual);
                        } else {
                            self.add_token(Bang);
                        }
                    }
                    "=" => {
                        self.bol_indentation_tokens();
                        if self.matches("=") {
                            self.add_token(EqualEqual);
                        } else {
                            self.add_token(Equal);
                        }
                    }
                    "<" => {
                        self.bol_indentation_tokens();
                        if self.matches("=") {
                            self.add_token(LessEqual);
                        } else {
                            self.add_token(Less);
                        }
                    }
                    ">" => {
                        self.bol_indentation_tokens();
                        if self.matches("=") {
                            self.add_token(GreaterEqual);
                        } else {
                            self.add_token(Greater);
                        }
                    }
                    "/" => {
                        self.bol_indentation_tokens();
                        if self.matches("/") {
                            // A comment until the end of the line.
                            self.advance_to_eol();
                        } else {
                            self.add_token(Slash);
                            self.following_line_continuation = true;
                        }
                    }
                    "\\" => {
                        // Logical line continuation.  Ignore a following
                        // newline character.

                        // Ignore spaces.
                        while self.matches(" ") {}

                        // Optional carriage return.
                        self.matches("\r");

                        if self.matches("\n") {
                            self.reset_line();
                            self.is_line_continuation = true;
                        } else {
                            self.error(ParseErrorCause::new(
                                SourceLoc::new(self.line, self.start_column),
                                "Unexpected character: '\\'",
                            ));
                        }
                    }
                    "\t" => {
                        self.error(ParseErrorCause::new(SourceLoc::new(self.line, self.column), "Illegal tab character; all whitespace must be spaces"));
                    }
                    " " => {
                        if self.is_bol {
                            self.bol_spaces = self.bol_spaces.saturating_add(1);
                        }
                    }
                    "\r" => (), // Ignore.
                    "\n" => {
                        let continue_line = self.following_line_continuation;
                        if !self.is_bol && !continue_line {
                            self.add_token(Semicolon);
                        }
                        self.reset_line();
                    }
                    "\"" => {
                        self.bol_indentation_tokens();
                        self.scan_string();
                    }
                    _ => {
                        self.bol_indentation_tokens();
                        if is_digit(grapheme_cluster) {
                            self.scan_number();
                        } else if is_alphabetic(grapheme_cluster) {
                            self.scan_identifier();
                        } else {
                            self.error(ParseErrorCause::new(
                                SourceLoc::new(self.line, self.column),
                                &format!(
                                    "Unexpected token: {}",
                                    grapheme_cluster
                                ),
                            ));
                        }
                    }
                };
            }
        }
    }

    fn reset_line(&mut self) {
        let line_was_blank = self.is_bol;
        self.line = self.line.saturating_add(1);
        self.column = 1;
        self.bol_spaces = 0;
        self.is_bol = true;
        // If the previous line was blank, do not affect line continuation
        // state.
        if !line_was_blank {
            self.is_line_continuation = self.following_line_continuation;
            self.following_line_continuation = false;
        }
    }

    // Call this when a new non-whitespace token is found, before the new token
    // is handled.
    fn bol_indentation_tokens(&mut self) {
        if !self.is_bol {
            return;
        }

        let last_indentation = self.indentation.last().map_or(0, |n| *n);
        match self.bol_spaces.cmp(&last_indentation) {
            Ordering::Equal => {}
            Ordering::Greater => {
                // Indentation increased.  Add begin block token.  But not if
                // we're continuing a previous line.
                if !self.is_line_continuation {
                    self.add_token(TokenType::LeftBrace);
                    self.indentation.push(self.bol_spaces);
                }
            }
            Ordering::Less => {
                // Indentation decreased.  Add one or more end block tokens.
                let mut found = false;
                while !self.indentation.is_empty() {
                    let amount = *self.indentation.last().unwrap();
                    match amount.cmp(&self.bol_spaces) {
                        Ordering::Equal => {
                            found = true;
                            break;
                        }
                        Ordering::Greater => {
                            self.indentation.pop();
                            self.add_token(TokenType::RightBrace);
                        }
                        Ordering::Less => {}
                    }
                }
                if !found && self.bol_spaces > 0 {
                    self.error(ParseErrorCause::new(SourceLoc::new(self.line, self.column), "Unindent does not match any previous indentation level"));
                }
            }
        }

        self.is_bol = false;
    }

    // Conditionally advance if the next grapheme cluster matches an expected
    // string.  Returns true if we matched.
    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.grapheme_indices.peek() {
            None => return false,
            Some((_, grapheme_cluster)) => {
                if *grapheme_cluster != expected {
                    return false;
                }
            }
        };

        // Consume this cluster when it's expected.
        self.advance();

        true
    }

    fn peek_index(&mut self) -> usize {
        match self.grapheme_indices.peek() {
            None => self.source.len(),
            Some((i, _)) => *i,
        }
    }

    // This is looking ahead 2 characters.
    fn peek_next_grapheme(&mut self) -> Option<&'g str> {
        match self.grapheme_indices.peek() {
            None => None,
            Some(_) => {
                let mut cloned = self.grapheme_indices.clone();
                cloned.next();
                match cloned.peek() {
                    None => None,
                    Some((_, grapheme_cluster)) => Some(grapheme_cluster),
                }
            }
        }
    }

    fn is_match(&mut self, expected: &str) -> bool {
        match self.grapheme_indices.peek() {
            None => false,
            Some((_, grapheme_cluster)) => *grapheme_cluster == expected,
        }
    }

    // Advance the grapheme cluster iterator.
    fn advance(&mut self) -> Option<(usize, &'g str)> {
        self.column = self.column.saturating_add(1);

        match self.grapheme_indices.next() {
            None => {
                self.eof = true;

                None
            }
            Some((i, cluster)) => {
                self.current = i;

                Some((i, cluster))
            }
        }
    }

    fn advance_to_eol(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if *grapheme_cluster == "\n" {
                        // Keep the newline next.
                        break;
                    }
                }
            };
            self.advance();
        }
    }

    fn scan_string(&mut self) {
        let start_index = self.peek_index();
        let start_line = self.line;

        while !self.is_match("\"") && !self.is_at_end() {
            match self.grapheme_indices.peek() {
                None => (),
                Some((_, grapheme_cluster)) => {
                    if *grapheme_cluster == "\n" {
                        self.line = self.line.saturating_add(1);
                        self.column = 0;
                    }
                }
            };
            self.advance();
        }

        // Unterminated string.
        if self.is_at_end() {
            let column = self.column.saturating_sub(1);
            self.error(ParseErrorCause::new(
                SourceLoc::new(self.line, column),
                "Unterminated string.",
            ));
            return;
        }

        // The closing quote.
        self.advance();

        // Trim the surrounding quotes.
        let value = &self.source[start_index..self.current];
        self.add_string_literal_token(value, start_line);
    }

    fn scan_number(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if !is_digit(grapheme_cluster) {
                        break;
                    }
                }
            };
            self.advance();
        }

        // Look for a fractional part.
        if self.is_match(".") {
            if let Some(c) = self.peek_next_grapheme() {
                if is_digit(c) {
                    // Consume the dot.
                    self.advance();
                }
            }

            loop {
                match self.grapheme_indices.peek() {
                    None => break,
                    Some((_, grapheme_cluster)) => {
                        if !is_digit(grapheme_cluster) {
                            break;
                        }
                    }
                };
                self.advance();
            }
        }

        let value = &self.source[self.start..self.peek_index()];
        let number: f64 = value.parse().unwrap_or_else(|_| {
            panic!("Unable to parse string as f64: {}", value)
        });
        self.add_number_literal_token(number);
    }

    fn scan_identifier(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if !is_alphanumeric(grapheme_cluster) {
                        break;
                    }
                }
            };
            self.advance();
        }

        let text = &self.source[self.start..self.peek_index()];

        // See if the identifier is a reserved word.
        let token_type = match KEYWORDS.get(text) {
            None => TokenType::Identifier,
            Some(token_type) => *token_type,
        };

        self.add_token(token_type);
    }

    fn is_at_end(&self) -> bool {
        self.eof
    }

    fn error(&mut self, error: ParseErrorCause) {
        self.errors.push(error);
    }

    // Add a token to the output.
    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(
            token_type,
            text,
            None,
            None,
            self.line,
            self.start_column,
        );
        self.tokens.push(token);
    }

    fn add_string_literal_token(
        &mut self,
        value: &'source str,
        start_line: u32,
    ) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(
            TokenType::String,
            text,
            Some(value),
            None,
            start_line,
            self.start_column,
        );
        self.tokens.push(token);
    }

    fn add_number_literal_token(&mut self, value: f64) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(
            TokenType::Number,
            text,
            None,
            Some(value),
            self.line,
            self.start_column,
        );
        self.tokens.push(token);
    }
}

fn is_digit(grapheme: &str) -> bool {
    // Note: built-in is_numeric() uses a more complicated unicode definition of
    // numeric.
    matches!(
        grapheme,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

fn is_alphabetic(grapheme: &str) -> bool {
    // Only look at the first base character.
    match grapheme.chars().next() {
        None => true,
        Some(c) => c.is_alphabetic() || c == '_',
    }
}

fn is_alphanumeric(grapheme: &str) -> bool {
    // Only look at the first base character.
    match grapheme.chars().next() {
        None => true,
        Some(c) => c.is_alphanumeric() || c == '_',
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_single_tokens() {
        let mut s = Scanner::new("!");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Bang, "!", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        let mut s = Scanner::new(".");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Dot, ".", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        let mut s = Scanner::new("=");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Equal, "=", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        let mut s = Scanner::new("<");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Less, "<", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        let mut s = Scanner::new("()");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::LeftParen, "(", None, None, 1, 1),
                Token::new(TokenType::RightParen, ")", None, None, 1, 2),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        let mut s = Scanner::new("{}");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::LeftBrace, "{", None, None, 1, 1),
                Token::new(TokenType::RightBrace, "}", None, None, 1, 2),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        // Next line.
        let mut s = Scanner::new("\n-");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Minus, "-", None, None, 2, 1),
                Token::new(TokenType::Eof, "", None, None, 2, 2)
            ])
        );
    }

    #[test]
    fn test_scan_double_tokens() {
        let mut s = Scanner::new("==");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::EqualEqual, "==", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        let mut s = Scanner::new("!=");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::BangEqual, "!=", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        let mut s = Scanner::new("<=");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::LessEqual, "<=", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
    }

    #[test]
    fn test_scan_string() {
        let mut s = Scanner::new("\"hello\"");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(
                    TokenType::String,
                    "\"hello\"",
                    Some("hello"),
                    None,
                    1,
                    1
                ),
                Token::new(TokenType::Eof, "", None, None, 1, 8)
            ])
        );
    }

    #[test]
    fn test_scan_multiline_string() {
        // Multi-line.
        let mut s = Scanner::new("\"hello\nthere\"");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(
                    TokenType::String,
                    "\"hello\nthere\"",
                    Some("hello\nthere"),
                    None,
                    1,
                    1
                ),
                Token::new(TokenType::Eof, "", None, None, 2, 7)
            ])
        );
    }

    #[test]
    fn test_scan_number() {
        let mut s = Scanner::new("9.5");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Number, "9.5", None, Some(9.5), 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("7");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Number, "7", None, Some(7.0), 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        let mut s = Scanner::new("144.25.");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(
                    TokenType::Number,
                    "144.25",
                    None,
                    Some(144.25),
                    1,
                    1
                ),
                Token::new(TokenType::Dot, ".", None, None, 1, 7),
                Token::new(TokenType::Eof, "", None, None, 1, 8)
            ])
        );
    }

    #[test]
    fn test_scan_identifier() {
        let mut s = Scanner::new("foo");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "foo", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("foo_bar2");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "foo_bar2", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 9)
            ])
        );
        let mut s = Scanner::new("π");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "π", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
        // Multi-character grapheme cluster.
        let mut s = Scanner::new("y̆");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "y̆", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 2)
            ])
        );
    }

    #[test]
    fn test_scan_keywords() {
        let mut s = Scanner::new("and");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::And, "and", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("break");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Break, "break", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 6)
            ])
        );
        let mut s = Scanner::new("class");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Class, "class", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 6)
            ])
        );
        let mut s = Scanner::new("else");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Else, "else", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 5)
            ])
        );
        let mut s = Scanner::new("false");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::False, "false", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 6)
            ])
        );
        let mut s = Scanner::new("for");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::For, "for", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("fun");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Fun, "fun", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("if");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::If, "if", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        let mut s = Scanner::new("nil");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Nil, "nil", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("or");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Or, "or", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 3)
            ])
        );
        let mut s = Scanner::new("print");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Print, "print", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 6)
            ])
        );
        let mut s = Scanner::new("return");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Return, "return", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 7)
            ])
        );
        let mut s = Scanner::new("this");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::This, "this", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 5)
            ])
        );
        let mut s = Scanner::new("true");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::True, "true", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 5)
            ])
        );
        let mut s = Scanner::new("var");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Var, "var", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 4)
            ])
        );
        let mut s = Scanner::new("while");
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::While, "while", None, None, 1, 1),
                Token::new(TokenType::Eof, "", None, None, 1, 6)
            ])
        );
    }

    #[test]
    fn test_scan_whitespace_indentation_as_blocks() {
        let mut s = Scanner::new(
            "one
  two
  three
four
",
        );
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "one", None, None, 1, 1),
                Token::new(TokenType::Semicolon, "\n", None, None, 1, 4),
                Token::new(TokenType::LeftBrace, "t", None, None, 2, 3),
                Token::new(TokenType::Identifier, "two", None, None, 2, 3),
                Token::new(TokenType::Semicolon, "\n", None, None, 2, 6),
                Token::new(TokenType::Identifier, "three", None, None, 3, 3),
                Token::new(TokenType::Semicolon, "\n", None, None, 3, 8),
                Token::new(TokenType::RightBrace, "f", None, None, 4, 1),
                Token::new(TokenType::Identifier, "four", None, None, 4, 1),
                Token::new(TokenType::Semicolon, "\n", None, None, 4, 5),
                Token::new(TokenType::Eof, "", None, None, 5, 1)
            ])
        );
    }

    #[test]
    fn test_scan_backslash_as_line_continuation() {
        let expected = Ok(vec![
            Token::new(TokenType::Identifier, "one", None, None, 1, 1),
            Token::new(TokenType::Identifier, "two", None, None, 2, 3),
            Token::new(TokenType::Semicolon, "\n", None, None, 2, 6),
            Token::new(TokenType::Eof, "", None, None, 3, 1),
        ]);
        let mut s = Scanner::new(
            "one \\
  two
",
        );
        assert_eq!(s.scan_tokens(), expected);

        // Multiple trailing spaces after the backslash.
        let mut s2 = Scanner::new(
            "one \\  
  two
",
        );
        assert_eq!(s2.scan_tokens(), expected);
    }

    #[test]
    fn test_scan_backslash_line_continuation_with_blank_lines() {
        let mut s = Scanner::new(
            "one \\


  two
",
        );
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "one", None, None, 1, 1),
                Token::new(TokenType::Identifier, "two", None, None, 4, 3),
                Token::new(TokenType::Semicolon, "\n", None, None, 4, 6),
                Token::new(TokenType::Eof, "", None, None, 5, 1)
            ])
        );
    }

    #[test]
    fn test_scan_backslash_without_newline_is_an_error() {
        let mut s = Scanner::new("one \\ two");
        let cause = ParseErrorCause::new(
            SourceLoc::new(1, 5),
            "Unexpected character: '\\'",
        );
        assert_eq!(s.scan_tokens(), Err(ParseError::new(vec![cause])));
    }

    #[test]
    fn test_scan_trailing_binary_operator_as_line_continuation() {
        let mut s = Scanner::new(
            "one +
  two
",
        );
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "one", None, None, 1, 1),
                Token::new(TokenType::Plus, "+", None, None, 1, 5),
                Token::new(TokenType::Identifier, "two", None, None, 2, 3),
                Token::new(TokenType::Semicolon, "\n", None, None, 2, 6),
                Token::new(TokenType::Eof, "", None, None, 3, 1)
            ])
        );
    }

    #[test]
    fn test_scan_trailing_binary_operator_line_continuation_with_blank_lines() {
        let mut s = Scanner::new(
            "one +


  two
",
        );
        assert_eq!(
            s.scan_tokens(),
            Ok(vec![
                Token::new(TokenType::Identifier, "one", None, None, 1, 1),
                Token::new(TokenType::Plus, "+", None, None, 1, 5),
                Token::new(TokenType::Identifier, "two", None, None, 4, 3),
                Token::new(TokenType::Semicolon, "\n", None, None, 4, 6),
                Token::new(TokenType::Eof, "", None, None, 5, 1)
            ])
        );
    }
}
