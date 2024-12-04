use glast::parser::{
    ast::{self, TypeTy},
    parse_from_str, ParseResult,
};
use std::{collections::HashMap, panic};
use wasm_bindgen::prelude::*;

use serde::{Deserialize, Serialize};

use crate::interpreter::{self, eval_ast, get_env_from_ast, EnvHandle, Value};

// Define a struct for passing variables from JavaScript.
#[derive(Deserialize)]
pub struct ShaderInput {
    uniforms: HashMap<String, Value>,
    in_vars: HashMap<String, Value>,
    globals: HashMap<String, Value>,
}

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

pub fn str_to_ast(code: &str) -> Vec<ast::Node> {
    let tree = match parse_from_str(code) {
        Ok(ast) => ast,
        Err(e) => {
            web_sys::console::error_1(&format!("Parsing failed: {:?}", e).into());
            panic!("Failed to parse code");
        }
    };
    let ParseResult { ast, .. } = tree.root(false);
    ast
}

#[wasm_bindgen]
pub fn eval_from_str(code: &str, inputs: &str) -> String {
    // Add debugging
    web_sys::console::log_1(&format!("Evaluating GLSL: {}", code).into());

    let parsed_inputs: ShaderInput = serde_json::from_str(inputs).unwrap();

    // Parse the GLSL code
    let ast = str_to_ast(code);

    // Interpret the AST
    let mut input_vars: HashMap<String, Value> = HashMap::new();
    // Add the input variables

    for (name, value) in parsed_inputs.uniforms {
        input_vars.insert(name, value);
    }

    for (name, value) in parsed_inputs.in_vars {
        input_vars.insert(name, value);
    }

    for (name, value) in parsed_inputs.globals {
        input_vars.insert(name, value);
    }

    let result = eval_ast(&ast, input_vars);

    serde_json::to_string(&result).unwrap().into()
}

// convert the ast type into the name of the corresponding interpreter Value type
fn typety_to_value_type_string(ty: &ast::TypeTy) -> String {
    match ty {
        ast::TypeTy::Single(prim) => match prim {
            ast::Primitive::Scalar(f) => match f {
                ast::Fundamental::Float => "Float".to_string(),
                ast::Fundamental::Int => "Int".to_string(),
                ast::Fundamental::UInt => "UInt".to_string(),
                ast::Fundamental::Bool => "Bool".to_string(),
                _ => "Unknown".to_string(),
            },
            ast::Primitive::Vector(f, size) => match f {
                ast::Fundamental::Float => format!("Vec{}", size),
                ast::Fundamental::Int => format!("IVec{}", size),
                ast::Fundamental::UInt => format!("UVec{}", size),
                ast::Fundamental::Bool => format!("BVec{}", size),
                _ => "Unknown".to_string(),
            },
            ast::Primitive::Matrix(c, r) => {
                if c == r {
                    format!("Mat{}", c)
                } else {
                    format!("Mat{}x{}", c, r)
                }
            }
            _ => "Unknown".to_string(),
        },
        _ => "Unknown".to_string(),
    }
}

#[derive(Serialize)]
struct IOVarDesc {
    name: String,
    ty: String,
}

impl IOVarDesc {
    fn from_iovar(iovar: &interpreter::IOVar) -> IOVarDesc {
        IOVarDesc {
            name: iovar.name.clone(),
            ty: typety_to_value_type_string(&iovar.type_),
        }
    }
}
#[derive(Serialize)]
pub struct IOVarDescs {
    uniforms: Vec<IOVarDesc>,
    in_vars: Vec<IOVarDesc>,
    out_vars: Vec<IOVarDesc>,
}

impl IOVarDescs {
    fn from_iovars(iovars: Vec<interpreter::IOVar>) -> Vec<IOVarDesc> {
        iovars
            .iter()
            .map(|iovar| IOVarDesc::from_iovar(iovar))
            .collect()
    }
    fn from_env(env: &EnvHandle) -> Self {
        env.with(|env| {
            let uniforms = IOVarDescs::from_iovars(env.uniforms.clone());
            let in_vars = IOVarDescs::from_iovars(env.in_vars.clone());
            let out_vars = IOVarDescs::from_iovars(env.out_vars.clone());
            IOVarDescs {
                uniforms,
                in_vars,
                out_vars,
            }
        })
    }
}

#[wasm_bindgen]
pub fn get_iovars_from_str(code: &str) -> String {
    let ast = str_to_ast(code);

    let env = get_env_from_ast(&ast);

    let io_vars = IOVarDescs::from_env(&env);

    serde_json::to_string(&io_vars).unwrap().into()
}
