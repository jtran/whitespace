//! This module contains everything WebAssembly-specific.
use wasm_bindgen::prelude::*;

use crate::parser;
use crate::resolver;

#[wasm_bindgen(js_name = parseString)]
pub fn wasm_parse_string(source: &str) -> Result<JsValue, JsError> {
    parser::parse(&source)
        .and_then(resolver::resolve)
        .map_err(JsError::from)
        .and_then(|resolved| {
            serde_wasm_bindgen::to_value(&resolved.statements)
                .map_err(JsError::from)
        })
}
