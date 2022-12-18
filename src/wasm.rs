//! This module contains everything WebAssembly-specific.
use wasm_bindgen::prelude::*;

use crate::parser;

#[wasm_bindgen(js_name = parseString)]
pub fn wasm_parse_string(source: &str) -> Result<JsValue, JsError> {
    parser::parse(&source)
        .map_err(JsError::from)
        .and_then(|x| serde_wasm_bindgen::to_value(&x).map_err(JsError::from))
}
