pub mod ast;
pub mod environment;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod source_loc;
pub mod token;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm;

// Re-exports.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm::wasm_parse_string;