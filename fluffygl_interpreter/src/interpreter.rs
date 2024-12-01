use glam::{BVec2, BVec3, BVec4};
use glast::parser::ast::{
    BinOpTy, Expr, ExprTy, Lit, Node, NodeTy, Omittable, PreOpTy, Qualifier, QualifierTy, Type,
    TypeTy,
};
use macaw::Mat2;
use macaw::{IVec2, IVec3, Mat3, Mat4, Vec2, Vec2Swizzles, Vec3, Vec3Swizzles, Vec4, Vec4Swizzles};

use core::panic;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::builtins;

use crate::log;

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct Texture2D {
    pub id: u32,
    pub size: IVec2,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct Texture3D {
    pub id: u32,
    pub size: IVec3,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Value {
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f32),
    Double(f64),
    Vec2(Vec2),
    Vec3(Vec3),
    Vec4(Vec4),
    BVec2(BVec2),
    BVec3(BVec3),
    BVec4(BVec4),
    Mat2(Mat2),
    Mat3(Mat3),
    Mat4(Mat4),
    Sampler2D(Texture2D),
    Sampler3D(Texture3D),
    SamplerCube(Texture2D),
    Nothing,
}
pub enum ControlFlow {
    Next,                  // continue to the next statement
    Return(Option<Value>), // Return from the current function
    Break,                 // Break out of the current loop or switch statement
    Continue,              // Continue with the next iteration of the current loop

                           // handling discard as a control flow type is painful because it needs to be propagated
                           // up through expressions as well as statements. it will probably be more ergonomic
                           // to handle this using unwinding.
                           // Discard,               // Discard the current fragment.
}

#[derive(Clone, Debug)]
pub struct Handle<T>(Rc<RefCell<T>>);

impl<T> Handle<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let value = self.0.as_ref().borrow();
        f(&*value)
    }

    pub fn with_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut value = self.0.borrow_mut();
        f(&mut *value)
    }
}

#[derive(Clone, Debug)]
pub struct IOVar {
    pub name: String,
    pub type_: TypeTy,
    pub qualifiers: Vec<QualifierTy>,
    pub initial_value: Option<Value>,
}

// an individual scope in the execution context, for example, a function or block scope
#[derive(Clone, Debug)]
pub struct Scope {
    vars: HashMap<String, Value>,
    parent: Option<ScopeHandle>,
}

impl Scope {
    pub fn new(parent: Option<ScopeHandle>) -> Self {
        Self {
            vars: HashMap::new(),
            parent,
        }
    }

    pub fn new_with_vars(parent: Option<ScopeHandle>, vars: HashMap<String, Value>) -> Self {
        Self { vars, parent }
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        // check if the variable is in the current scope
        if let Some(value) = self.vars.get(name) {
            // log!(
            //     "get_var Found variable {} in current scope with value {:?}",
            //     name, value
            // );
            return Some(value.clone());
        }
        // if we have a parent scope, check there
        if let Some(parent) = &self.parent {
            // log!("get_var Checking parent scope for variable {}", name);
            return parent.with(|parent| parent.get_var(name));
        }
        None
    }

    // TODO: rename to define_var, panic if already defined
    pub fn set_var(&mut self, name: String, value: Value) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn update_var(&mut self, name: String, value: Value) {
        // if the variable is in the current scope, update it
        // otherwise, check the parent scope
        if self.vars.contains_key(&name) {
            // log!("update_var Updating variable {} to {:?}", name, value);
            self.set_var(name, value);
        } else {
            // log!("update_var Checking parent scope for variable {}", name);
            match &self.parent {
                Some(parent) => {
                    parent.update_var(name, value);
                }
                None => {
                    panic!(
                        "Tried to set variable {} but no binding found in uppermost scope",
                        name
                    );
                }
            }
        }
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }
}

type ScopeHandle = Handle<Scope>;
impl ScopeHandle {
    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.with(|scope| scope.get_var(name))
    }

    pub fn set_var(&self, name: String, value: Value) {
        self.with_mut(|scope| scope.set_var(name, value));
    }

    pub fn update_var(&self, name: String, value: Value) {
        self.with_mut(|scope| scope.update_var(name, value));
    }

    pub fn has_parent(&self) -> bool {
        self.with(|scope| scope.has_parent())
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub type_: TypeTy,
}
#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: TypeTy,
    pub params: Vec<Param>,
    pub body: Vec<Node>,
}

// the top level environment of an execution context.
#[derive(Clone, Debug)]
pub struct Env {
    // the inputs and outputs of the shader
    in_vars: Vec<IOVar>,
    out_vars: Vec<IOVar>,
    uniforms: Vec<IOVar>,
    functions: HashMap<String, Function>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            in_vars: Vec::new(),
            out_vars: Vec::new(),
            uniforms: Vec::new(),
            functions: HashMap::new(),
        }
    }
}

type EnvHandle = Handle<Env>;

impl EnvHandle {
    pub fn add_io_var(
        &self,
        name: &String,
        type_: &TypeTy,
        qualifiers: &Vec<Qualifier>,
        qualifier_type: QualifierTy,
    ) {
        self.with_mut(|env| {
            let io_var = IOVar {
                name: name.clone(),
                type_: type_.clone(),
                qualifiers: qualifiers.iter().map(|q| q.ty.clone()).collect(),
                initial_value: None,
            };
            match qualifier_type {
                QualifierTy::In => env.in_vars.push(io_var),
                QualifierTy::Out => env.out_vars.push(io_var),
                QualifierTy::Uniform => env.uniforms.push(io_var),
                _ => panic!("Invalid qualifier type {:?}", qualifier_type),
            }
        });
    }

    pub fn add_function(&self, function: Function) {
        self.with_mut(|env| {
            env.functions.insert(function.name.clone(), function);
        });
    }

    pub fn get_function(&self, name: &String) -> Option<Function> {
        self.with(|env| env.functions.get(name).cloned())
    }

    pub fn call_function(
        &self,
        name: &String,
        args: Vec<Value>,
        parent_scope: &ScopeHandle,
    ) -> ControlFlow {
        let function = self.get_function(name).expect(&format!(
            "Tried to call function {}, no function defined with that name",
            name
        ));
        let function_scope = ScopeHandle::new(Scope::new(Some(parent_scope.clone())));

        for (param, arg) in function.params.iter().zip(args) {
            function_scope.set_var(param.name.clone(), arg);
        }

        match eval_statements(self, &function_scope, &function.body) {
            ControlFlow::Return(value) => ControlFlow::Return(value),
            _ => panic!("Function did not return a valid ControlFlow"),
        }
    }
}

fn assignment(scope: &ScopeHandle, left: &Expr, right_value: Value) -> Value {
    let identifier = match &left.ty {
        ExprTy::Ident(ident) => ident.name.clone(),
        _ => panic!("left side of assignment must be an identifier"),
    };
    scope.update_var(identifier, right_value);
    right_value
}

pub fn eval_expression(env: &EnvHandle, scope: &ScopeHandle, expr: &Expr) -> Value {
    log!("eval_expression {:#?}", expr);
    match &expr.ty {
        ExprTy::Lit(literal) => match literal {
            Lit::Bool(b) => Value::Bool(*b),
            Lit::Int(i) => Value::Int(*i),
            Lit::UInt(u) => Value::UInt(*u),
            Lit::Float(f) => Value::Float(*f),
            Lit::Double(d) => Value::Double(*d),
            _ => todo!(),
        },
        ExprTy::Ident(ident) => {
            // TODO: it seems like Ident can appear in in different contexts, not just variable access
            let value = scope
                .get_var(&ident.name)
                .expect(&format!("Variable '{}' not found in scope", ident.name));

            // if value is Nothing, it means the variable was declared but not initialized
            if let Value::Nothing = value {
                panic!(
                    "Tried to access variable '{}' which was declared but not initialized",
                    ident.name
                );
            }
            value.clone()
        }
        ExprTy::Binary { left, op, right } => {
            // TODO: what binary operators would right be optional for?
            let right_value = eval_expression(env, scope, right.as_deref().unwrap());

            // log!("Binary operation {:?} {:?} {:?}", left, op, right);

            // first, let's handle mutation operators
            match op.ty {
                // Eq is '=', in other words, assignment
                BinOpTy::Eq => return assignment(scope, left, right_value),
                // AddEq is '+='
                BinOpTy::AddEq => {
                    let left_value = eval_expression(env, scope, left);
                    let result: Value = builtins::addition(&left_value, &right_value);
                    return assignment(scope, left, result);
                }
                BinOpTy::SubEq => {
                    todo!()
                }
                BinOpTy::MulEq => {
                    todo!()
                }
                BinOpTy::DivEq => {
                    todo!()
                }
                _ => {} // handled below
            }

            let left_value = eval_expression(env, scope, &*left);
            match op.ty {
                BinOpTy::Add => builtins::addition(&left_value, &right_value),
                BinOpTy::Sub => builtins::subtraction(&left_value, &right_value),
                BinOpTy::Mul => builtins::multiplication(&left_value, &right_value),
                BinOpTy::Div => builtins::division(&left_value, &right_value),
                BinOpTy::Rem => builtins::remainder(&left_value, &right_value),
                // TODO: implement logical operators, e.g. AndAnd, OrOr
                // TODO: implement comparison operators, e.g. Eq, Ne, Lt, Le, Gt, Ge
                // TODO: implement bitwise operators, e.g. And, Or, Xor
                // TODO: implement shift operators, e.g. LShift, RShift
                _ => todo!(),
            }
        }

        ExprTy::Prefix { op, expr } => {
            let expr_unboxed = expr.as_ref().unwrap();
            let right_value = eval_expression(env, scope, expr_unboxed);
            match op.ty {
                // logical not
                PreOpTy::Neg => match right_value {
                    Value::Bool(b) => Value::Bool(!b),
                    _ => panic!("Logical not operator applied to non-boolean value"),
                },
                PreOpTy::Flip | PreOpTy::Sub => {
                    // sign flip
                    match right_value {
                        Value::Int(i) => Value::Int(-i),
                        Value::Float(f) => Value::Float(-f),
                        Value::Double(d) => Value::Double(-d),
                        _ => panic!("Sign flip operator applied to non-numeric value"),
                    }
                }

                _ => todo!(),
            }
        }

        ExprTy::FnCall { ident, args } => {
            let args_for_fn = args
                .iter()
                .map(|arg| eval_expression(env, scope, arg))
                .collect();
            match env.call_function(&ident.name, args_for_fn, scope) {
                ControlFlow::Return(value) => value.unwrap(),
                _ => panic!("Function did not return a valid ControlFlow value"),
            }
        }

        ExprTy::ObjAccess { obj, leaf } => {
            log!("Object access {:#?} {:#?}", obj, leaf);
            let obj_value = eval_expression(env, scope, obj);
            let leaf_expr = leaf.as_ref().unwrap();
            let leaf_unboxed = leaf_expr.as_ref();
            // in this context the parser produces a structure like Expr(Ident), which needs to
            // be evaluated differently in this context to apply it as an property access
            let leaf_value = match &leaf_unboxed.ty {
                ExprTy::Ident(ident) => {
                    // interpret based on what is being accessed upon
                    // TODO: implement swizzled access for vectors
                    match obj_value {
                        Value::Vec2(vec2) => match ident.name.as_str() {
                            "x" => Value::Float(vec2.x),
                            "y" => Value::Float(vec2.y),
                            "xx" => Value::Vec2(vec2.xx()),
                            "xy" => Value::Vec2(vec2.xy()),
                            "yx" => Value::Vec2(vec2.yx()),
                            "yy" => Value::Vec2(vec2.yy()),
                            _ => {
                                // parse swizzles/rg naming
                                let swizzle = ident.name.as_str();
                                let mut swizzled = Vec2::ZERO;

                                // check that this is a valid swizzle by checking that each character is valid
                                let mut is_swizzle = true;
                                for c in swizzle.chars() {
                                    match c {
                                        'x' | 'y' | 'r' | 'g' => {}
                                        _ => {
                                            is_swizzle = false;
                                            break;
                                        }
                                    }
                                }

                                if is_swizzle {
                                    for (i, c) in swizzle.chars().enumerate() {
                                        match c {
                                            'x' => swizzled[i] = vec2.x,
                                            'y' => swizzled[i] = vec2.y,
                                            'r' => swizzled[i] = vec2.x,
                                            'g' => swizzled[i] = vec2.y,
                                            _ => panic!("Invalid swizzle character '{}'", c),
                                        }
                                    }
                                    Value::Vec2(swizzled)
                                } else {
                                    panic!("Invalid property access on Vec2: '{}'", ident.name);
                                }
                            }
                        },
                        Value::Vec3(vec3) => match ident.name.as_str() {
                            "x" => Value::Float(vec3.x),
                            "y" => Value::Float(vec3.y),
                            "z" => Value::Float(vec3.z),
                            // optimize the most common swizzles for now
                            "xy" => Value::Vec2(vec3.xy()),
                            "yz" => Value::Vec2(vec3.yz()),
                            "xz" => Value::Vec2(vec3.xz()),
                            "xx" => Value::Vec2(vec3.xx()),
                            "yy" => Value::Vec2(vec3.yy()),
                            "zz" => Value::Vec2(vec3.zz()),
                            "xyz" => Value::Vec3(vec3),
                            "rgb" => Value::Vec3(vec3),

                            _ => {
                                // parse swizzles
                                let swizzle = ident.name.as_str();
                                let mut swizzled = Vec3::ZERO;

                                // check that this is a valid swizzle by checking that each character is valid
                                let mut is_swizzle = true;
                                for c in swizzle.chars() {
                                    match c {
                                        'x' | 'y' | 'z' | 'r' | 'g' | 'b' => {}
                                        _ => {
                                            is_swizzle = false;
                                            break;
                                        }
                                    }
                                }

                                if is_swizzle {
                                    for (i, c) in swizzle.chars().enumerate() {
                                        match c {
                                            'x' => swizzled[i] = vec3.x,
                                            'y' => swizzled[i] = vec3.y,
                                            'z' => swizzled[i] = vec3.z,
                                            'r' => swizzled[i] = vec3.x,
                                            'g' => swizzled[i] = vec3.y,
                                            'b' => swizzled[i] = vec3.z,
                                            _ => panic!("Invalid swizzle character '{}'", c),
                                        }
                                    }
                                    Value::Vec3(swizzled)
                                } else {
                                    panic!("Invalid property access on Vec3: '{}'", ident.name);
                                }
                            }
                        },
                        Value::Vec4(vec4) => {
                            match ident.name.as_str() {
                                "x" => Value::Float(vec4.x),
                                "y" => Value::Float(vec4.y),
                                "z" => Value::Float(vec4.z),
                                "w" => Value::Float(vec4.w),
                                "xy" => Value::Vec2(vec4.xy()),
                                "xyz" => Value::Vec3(vec4.xyz()),
                                "rgb" => Value::Vec3(vec4.xyz()),
                                "rgba" => Value::Vec4(vec4),
                                _ => {
                                    // parse swizzles
                                    let swizzle = ident.name.as_str();
                                    let mut swizzled = Vec4::ZERO;

                                    // check that this is a valid swizzle by checking that each character is valid
                                    let mut is_swizzle = true;
                                    for c in swizzle.chars() {
                                        match c {
                                            'x' | 'y' | 'z' | 'w' | 'r' | 'g' | 'b' | 'a' => {}
                                            _ => {
                                                is_swizzle = false;
                                                break;
                                            }
                                        }
                                    }

                                    if is_swizzle {
                                        for (i, c) in swizzle.chars().enumerate() {
                                            match c {
                                                'x' => swizzled[i] = vec4.x,
                                                'y' => swizzled[i] = vec4.y,
                                                'z' => swizzled[i] = vec4.z,
                                                'w' => swizzled[i] = vec4.w,
                                                'r' => swizzled[i] = vec4.x,
                                                'g' => swizzled[i] = vec4.y,
                                                'b' => swizzled[i] = vec4.z,
                                                'a' => swizzled[i] = vec4.w,
                                                _ => panic!("Invalid swizzle character '{}'", c),
                                            }
                                        }
                                        Value::Vec4(swizzled)
                                    } else {
                                        panic!("Invalid property access on Vec4: '{}'", ident.name);
                                    }
                                }
                            }
                        }
                        Value::Mat3(mat3) => {
                            panic!("Object access not implemented for Mat3")
                        }
                        Value::Mat4(mat4) => {
                            panic!("Object access not implemented for Mat4")
                        }
                        _ => {
                            panic!("Object access not implemented for {:?}", obj_value)
                        }
                    }
                }
                _ => {
                    todo!(
                        "Object access leaf expression type not implemented: {:#?}",
                        leaf_unboxed
                    )
                }
            };

            log!(
                "Object access: {:?} {:?} calculated value {:#?}",
                obj,
                leaf,
                leaf_value
            );

            leaf_value // return the value of the accessed object
        }

        _ => {
            log!("tried to eval unimplemented expression type: {:#?}", expr);
            todo!()
        }
    }
}

fn get_io_qualifier(qualifiers: &[Qualifier]) -> Option<QualifierTy> {
    for qualifer in qualifiers {
        match &qualifer.ty {
            QualifierTy::In => return Some(QualifierTy::In),
            QualifierTy::Out => return Some(QualifierTy::Out),
            QualifierTy::Uniform => return Some(QualifierTy::Uniform),
            _ => {}
        }
    }
    None
}

fn unwrap_omittable<T>(omittable: &Omittable<T>) -> &T {
    match omittable {
        Omittable::Some(value) => value,
        Omittable::None => {
            panic!("Expected Omittable::Some, got Omittable::None")
        }
    }
}

// records an input or output variable in the environment. does not define the variable in the scope.
fn record_io_var(env: &EnvHandle, name: &String, type_: &Type, qualifier_type: &QualifierTy) {
    match qualifier_type {
        QualifierTy::In | QualifierTy::Uniform | QualifierTy::Out => {
            env.add_io_var(&name, &type_.ty, &type_.qualifiers, qualifier_type.clone());
        }
        _ => panic!("Invalid qualifier type {:?}", qualifier_type),
    }
}

fn declare_var(env: &EnvHandle, scope: &ScopeHandle, name: &String, type_: &Type) {
    // first, handle input and output qualifiers. this doesn't define the variable in the scope
    // but it does record it in the environment

    let io_qualifier = get_io_qualifier(&type_.qualifiers);
    match io_qualifier {
        Some(qualifier_type) => {
            if scope.has_parent() {
                panic!(
                    "Cannot declare input or output variables in a scope other than the top level"
                );
            }
            record_io_var(env, name, &type_, &qualifier_type);

            match qualifier_type {
                QualifierTy::In | QualifierTy::Uniform => {
                    // check if the input variable was actually provided.
                    // could move this to access time, but it's probably clearer to catch this statically
                    if scope.get_var(name.as_str()) == None {
                        panic!(
                            "Variable '{}' is declared as an {:?} variable but not provided",
                            name, qualifier_type
                        );
                    }

                    // return early because we don't need to define the variable in the scope for inputs
                    // as we expect them to be provided when initializing the execution context
                    return;
                }
                _ => {}
            }
        }
        None => {}
    }

    // then, define the variable in the scope

    // check that there isn't already a binding for the output variable
    if scope.get_var(name.as_str()) == None {
        // define a binding for the output variable
        // this ensures that assignments to the output variable
        // will find this scope
        scope.set_var(name.clone(), Value::Nothing);
    } else {
        panic!("Variable {} is declared but already defined", name);
    }
}

// for a variable which has already been declared, initialise it with a value
fn initialise_var(env: &EnvHandle, scope: &ScopeHandle, name: &String, value: &Option<Expr>) {
    let value = match value {
        // TODO: enforce that the type of the expression matches the type of the variable
        // TODO: enforce that the expression is constant for top level variables (except in GLSL 4.3+)
        Some(expr) => eval_expression(env, scope, expr),
        None => Value::Nothing,
    };
    scope.update_var(name.clone(), value);
}

pub fn eval_statements(env: &EnvHandle, scope: &ScopeHandle, statements: &[Node]) -> ControlFlow {
    let at_top_level = !scope.has_parent();
    for stmt in statements {
        log!("eval_statement {:?}", stmt);
        match &stmt.ty {
            NodeTy::VersionDirective {
                version: _,
                profile: _,
            } => {
                // ignore for now
            }
            NodeTy::VarDef { type_, ident } => {
                declare_var(env, scope, &ident.name, type_);
            }

            // A variable definition with initialization, e.g. `int i = 0;`.
            // VarDefInit {type_: Type,ident: Ident,value: Option<Expr>},
            NodeTy::VarDefInit {
                type_,
                ident,
                value: maybe_intializer_expr,
            } => {
                declare_var(env, scope, &ident.name, type_);

                initialise_var(env, scope, &ident.name, maybe_intializer_expr);
            }

            // A variable definition containing multiple variables, e.g. `int i, j, k;`.
            // VarDefs(Vec<(Type, Ident)>),
            NodeTy::VarDefs(vars) => {
                for (type_, ident) in vars {
                    declare_var(env, scope, &ident.name, type_);
                }
            }

            // A variable definition with initialization, containing multiple variables, e.g. `int i, j, k = 0;`.
            // VarDefInits(Vec<(Type, Ident)>, Option<Expr>),
            NodeTy::VarDefInits(vars, initial_value) => {
                for (type_, ident) in vars {
                    declare_var(env, scope, &ident.name, type_);
                    initialise_var(env, scope, &ident.name, initial_value);
                }
            }

            // An expression statement, e.g. `5 + 1;` or `i++;`.
            NodeTy::Expr(expr) => {
                if at_top_level {
                    // ignore top level expressions
                    log!("Ignoring top level expression {:?}", expr);
                    continue;
                }
                eval_expression(env, scope, expr);
            }

            // A function declaration, e.g. `int foo(int i);`.
            NodeTy::FnDecl {
                return_type: _,
                ident: _,
                params: _,
            } => {
                // ignore for now
            }
            NodeTy::FnDef {
                return_type,
                ident,
                params,
                body,
            } => {
                let function = Function {
                    name: ident.name.clone(),
                    return_type: return_type.ty.clone(),
                    params: params
                        .iter()
                        .map(|param| Param {
                            name: unwrap_omittable(&param.ident).name.clone(),
                            type_: param.type_.ty.clone(),
                        })
                        .collect(),
                    body: body.contents.clone(),
                };
                env.add_function(function);
            }

            // A block of statements, e.g. `{ int i = 0; i++; }`.
            NodeTy::Block(statements_node) => {
                let block_scope = ScopeHandle::new(Scope::new(Some(scope.clone())));

                if let cf @ (ControlFlow::Return(_) | ControlFlow::Break | ControlFlow::Continue) =
                    eval_statements(env, &block_scope, &statements_node.contents)
                {
                    // propagate control flow
                    return cf;
                }
            }

            NodeTy::Discard => {
                todo!("discard statement");
            }

            // A return statement, e.g. `return 0;`.
            NodeTy::Return { value } => {
                let return_value = match value {
                    Omittable::Some(expr) => Some(eval_expression(env, scope, expr)),
                    Omittable::None => None,
                };
                return ControlFlow::Return(return_value);
            }

            _ => {
                // log!("Evaluating statement: {:#?}", stmt);
                todo!();
            }
        };
    }
    ControlFlow::Next
}

pub fn eval_ast(ast: &[Node], input_vars: HashMap<String, Value>) -> HashMap<String, Value> {
    let env = EnvHandle::new(Env::new());
    let root_scope = ScopeHandle::new(Scope::new_with_vars(None, input_vars));

    // filter top level statements. in particular, any top level expression which is of Ident type is
    // probably a keyword which the parser didn't recognise, so we should ignore it.

    // log!("eval_ast");
    eval_statements(&env, &root_scope, ast);

    // call the main function
    let main_function = env.get_function(&"main".to_string()).unwrap();
    let main_scope = ScopeHandle::new(Scope::new(Some(root_scope.clone())));
    eval_statements(&env, &main_scope, &main_function.body);

    let mut output_vars: HashMap<String, Value> = HashMap::new();

    // copy the output variables from the root scope to the output_vars

    let output_var_names: Vec<String> = env.with(|env| {
        env.out_vars
            .iter()
            .map(|out_var| out_var.name.clone())
            .collect()
    });

    for name in output_var_names {
        // log!("Getting output variable {}", name);
        let value = root_scope.get_var(&name).unwrap().clone();
        output_vars.insert(name, value);
    }

    output_vars
}
