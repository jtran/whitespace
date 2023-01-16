#![warn(
    clippy::cloned_instead_of_copied,
    clippy::dbg_macro,
    clippy::exit,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::match_wildcard_for_single_variants,
    clippy::rc_buffer,
    clippy::ref_binding_to_reference,
    clippy::ref_option_ref,
    clippy::str_to_string,
    clippy::suboptimal_flops
)]

pub mod ast;
pub mod environment;
pub mod error;
pub mod parser;
pub mod resolver;
pub mod scanner;
pub mod source_loc;
pub mod token;
pub mod value;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm;

// Re-exports.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm::wasm_parse_string;
