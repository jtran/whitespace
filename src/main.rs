#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

mod ast;
mod environment;
mod error;
mod parser;
mod scanner;
mod source_loc;
mod token;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::process;

use argparse::{ArgumentParser, Print, Store};

use crate::ast::Stmt;
use crate::error::*;
use crate::source_loc::*;

type Ast = Vec<Stmt>;

enum RunError {
    RunParseError(ParseError),
}

fn main() {
    let mut script_filename = "".to_string();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Coffee language interpreter");
        ap.add_option(
            &["--version"],
            Print(env!("CARGO_PKG_VERSION").to_string()),
            "Show version",
        );
        ap.refer(&mut script_filename)
            .add_argument("script_filename", Store,
                          "Coffee file to execute.  Omit to run an interactive REPL.");
        ap.parse_args_or_exit();
    }
    if ! script_filename.is_empty() {
        let run_result = run_file(&script_filename);

        match run_result {
            Ok(_) => (),
            Err(RunError::RunParseError(_)) => process::exit(65),
        }
    }
    else {
        run_repl();
    }
}

fn run_repl() {
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().expect("run_repl: unable to flush stdout");

        let mut input = String::new();
        match stdin.lock().read_line(&mut input) {
            Ok(_) => {
                let result = run(input, true);
                print_result(&result, true);
            }
            Err(error) => {
                println!("Error reading stdin: {:?}", error);
                break;
            }
        }
    }
}

// Returns true if there was an error running the file.
fn run_file(file_path: &str) -> Result<Ast, RunError> {
    let mut file = File::open(file_path).unwrap_or_else(|_| panic!("source file not found: {}", file_path));
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap_or_else(|_| panic!("unable to read file: {}", file_path));

    let result = run(contents, false);
    print_result(&result, true);

    result
}

fn run(source: String, for_repl: bool)
    -> Result<Ast, RunError>
{
    // If there's a parse error, it's converted to a run error here.
    let result = if for_repl {
        parser::parse_repl_line(&source)
    }
    else {
        parser::parse(&source)
    };

    result.map_err(|err| err.into())
}

fn print_result(result: &Result<Ast, RunError>, print_success: bool) {
    match result {
        Ok(value) => {
            if print_success {
                println!("{:#?}", value);
            }
        }
        Err(e) => {
            match e {
                RunError::RunParseError(err) => {
                    // Print all causes.
                    for cause in err.causes.iter() {
                        print_parse_error(cause);
                    }
                }
            }
        }
    }
}

fn print_parse_error(error: &ParseErrorCause) {
    match error.token {
        None => report_error(&error.source_loc, &error.message),
        Some(ref token) => report_error_at_token(&error.source_loc, token, &error.message)
    }
}

fn report_error(source_loc: &SourceLoc, message: &str) {
    eprintln!("[line {}:{}] Error: {}", source_loc.line, source_loc.column, message);
}

fn report_error_at_token(source_loc: &SourceLoc, token: &str, message: &str) {
    eprintln!("[line {}:{}] Error at '{}': {}", source_loc.line, source_loc.column, token, message);
}

impl From<ParseError> for RunError {
    fn from(err: ParseError) -> RunError {
        RunError::RunParseError(err)
    }
}
