use glast::parser::{self, ast, parse_from_str, ParseResult};
use macaw::Vec4;
use std::{collections::HashMap, panic};
use wasm_bindgen::prelude::*;
use web_sys::js_sys;

use crate::interpreter::{eval_ast, Value};

#[wasm_bindgen(start)]
pub fn init_panic_hook() {
    // Set up the panic hook for better error messages
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    web_sys::console::log_1(&"Panic hook set up!".into());
}

#[wasm_bindgen]
pub fn parse_str_to_ast_json(code: &str) -> String {
    // Add debugging
    web_sys::console::log_1(&format!("Parsing GLSL: {}", code).into());

    let tree = match parse_from_str(code) {
        Ok(ast) => ast,
        Err(e) => {
            web_sys::console::error_1(&format!("Parsing failed: {:?}", e).into());
            panic!("Failed to parse code");
        }
    };
    let ParseResult { ast, .. } = tree.root(false);
    ast_to_json(&ast)
}

pub fn ast_to_json(ast: &[ast::Node]) -> String {
    serde_json::to_string_pretty(ast).unwrap()
}

#[wasm_bindgen]
pub fn eval_from_str(code: &str) -> js_sys::Array {
    // Add debugging
    web_sys::console::log_1(&format!("Evaluating GLSL: {}", code).into());

    // Parse the GLSL code
    let ast = parser::parse_from_str(&code).expect("Failed to parse the GLSL code");

    let ParseResult { ast, .. } = ast.root(false);

    // Interpret the AST
    let mut input_vars: HashMap<String, Value> = HashMap::new();
    input_vars.insert(
        "color".to_string(),
        Value::Vec4(Vec4::new(1.0, 0.0, 0.0, 1.0)),
    );

    input_vars.insert(
        "gl_FragCoord".to_string(),
        Value::Vec4(Vec4::new(0.0, 0.0, 0.0, 0.0)),
    );

    let result = eval_ast(&ast, input_vars);

    let frag_color_value = result.get("fragColor").unwrap();

    let frag_color = match frag_color_value {
        Value::Vec4(vec4) => vec4,
        _ => panic!("Expected Vec4"),
    };

    println!("{:#?}", result);

    frag_color
        .as_ref()
        .iter()
        .copied()
        .map(JsValue::from)
        .collect()
}
