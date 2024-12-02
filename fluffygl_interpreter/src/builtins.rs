use std::collections::HashMap;

use crate::interpreter::Value;

use glam::{BVec2, BVec3, BVec4};
use macaw::{Mat2, Mat3, Mat4, Vec2, Vec2Swizzles, Vec3, Vec3Swizzles, Vec4, Vec4Swizzles};

// implements arbitrary swizzling of vectors. to reduce the amount of code required,
// for now i'm just implementing Vec4 swizzling, so the other types will need to be
// converted to Vec4 before being passed to this function. this function also doesn't
// validate the combination of swizzle characters, so it's possible to create invalid
// swizzles which combine the xyzw and rgba component names.
pub fn swizzle_vec4(input: Vec4, swizzle: &str) -> Value {
    let swizzle_size = swizzle.len();
    match swizzle_size {
        1 => {
            // single component swizzle
            let c = swizzle.chars().nth(0).unwrap();
            let swizzled = match c {
                'x' => input.x,
                'y' => input.y,
                'r' => input.x,
                'g' => input.y,
                _ => panic!("Invalid swizzle character '{}'", c),
            };
            Value::Float(swizzled)
        }
        2 => {
            let mut swizzled = Vec2::ZERO;

            for (i, c) in swizzle.chars().enumerate() {
                match c {
                    'x' => swizzled[i] = input.x,
                    'y' => swizzled[i] = input.y,
                    'r' => swizzled[i] = input.x,
                    'g' => swizzled[i] = input.y,
                    _ => panic!("Invalid swizzle character '{}'", c),
                }
            }
            Value::Vec2(swizzled)
        }
        3 => {
            let mut swizzled = Vec3::ZERO;

            for (i, c) in swizzle.chars().enumerate() {
                match c {
                    'x' => swizzled[i] = input.x,
                    'y' => swizzled[i] = input.y,
                    'z' => swizzled[i] = input.z,
                    'r' => swizzled[i] = input.x,
                    'g' => swizzled[i] = input.y,
                    'b' => swizzled[i] = input.z,
                    _ => panic!("Invalid swizzle character '{}'", c),
                }
            }
            Value::Vec3(swizzled)
        }
        4 => {
            let mut swizzled = Vec4::ZERO;

            for (i, c) in swizzle.chars().enumerate() {
                match c {
                    'x' => swizzled[i] = input.x,
                    'y' => swizzled[i] = input.y,
                    'z' => swizzled[i] = input.z,
                    'w' => swizzled[i] = input.w,
                    'r' => swizzled[i] = input.x,
                    'g' => swizzled[i] = input.y,
                    'b' => swizzled[i] = input.z,
                    'a' => swizzled[i] = input.w,
                    _ => panic!("Invalid swizzle character '{}'", c),
                }
            }
            Value::Vec4(swizzled)
        }
        _ => panic!("Invalid swizzle size {}", swizzle_size),
    }
}

pub fn vec_property_access(input: Value, property: &str) -> Value {
    match input {
        Value::Vec2(vec2) => match property {
            "x" => Value::Float(vec2.x),
            "y" => Value::Float(vec2.y),
            "xx" => Value::Vec2(vec2.xx()),
            "xy" => Value::Vec2(vec2.xy()),
            "yx" => Value::Vec2(vec2.yx()),
            "yy" => Value::Vec2(vec2.yy()),
            _ => {
                // parse swizzles
                let swizzle = property;

                // check that this is a swizzle by checking that each character is a component
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
                    swizzle_vec4(Vec4::from((vec2, 0.0, 0.0)), swizzle)
                } else {
                    panic!("Invalid property access on Vec2: '{}'", property);
                }
            }
        },
        Value::Vec3(vec3) => match property {
            "x" => Value::Float(vec3.x),
            "y" => Value::Float(vec3.y),
            "z" => Value::Float(vec3.z),
            "r" => Value::Float(vec3.x),
            "g" => Value::Float(vec3.y),
            "b" => Value::Float(vec3.z),
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
                let swizzle = property;

                // check that this is a swizzle by checking that each character is a component
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
                    swizzle_vec4(Vec4::from((vec3, 0.0)), swizzle)
                } else {
                    panic!("Invalid property access on Vec3: '{}'", property);
                }
            }
        },
        Value::Vec4(vec4) => {
            match property {
                "x" => Value::Float(vec4.x),
                "y" => Value::Float(vec4.y),
                "z" => Value::Float(vec4.z),
                "w" => Value::Float(vec4.w),
                "r" => Value::Float(vec4.x),
                "g" => Value::Float(vec4.y),
                "b" => Value::Float(vec4.z),
                "a" => Value::Float(vec4.w),
                "xy" => Value::Vec2(vec4.xy()),
                "xyz" => Value::Vec3(vec4.xyz()),
                "rgb" => Value::Vec3(vec4.xyz()),
                "rgba" => Value::Vec4(vec4),
                _ => {
                    // parse swizzles
                    let swizzle = property;

                    // check that this is a swizzle by checking that each character is a component
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
                        swizzle_vec4(vec4, swizzle)
                    } else {
                        panic!("Invalid property access on Vec4: '{}'", property);
                    }
                }
            }
        }
        _ => panic!("Expected a Vec type"),
    }
}

fn mat3_broadcast_scalar(scalar: f32) -> Mat3 {
    Mat3::from_cols_array(&[
        scalar, scalar, scalar, //
        scalar, scalar, scalar, //
        scalar, scalar, scalar, //
    ])
}

fn mat4_broadcast_scalar(scalar: f32) -> Mat4 {
    Mat4::from_cols_array(&[
        scalar, scalar, scalar, scalar, //
        scalar, scalar, scalar, scalar, //
        scalar, scalar, scalar, scalar, //
        scalar, scalar, scalar, scalar, //
    ])
}

pub fn addition(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
        (Value::UInt(l), Value::UInt(r)) => Value::UInt(l + r),
        (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
        (Value::Double(l), Value::Double(r)) => Value::Double(l + r),
        (Value::Vec2(l), Value::Vec2(r)) => Value::Vec2(*l + *r),
        (Value::Vec3(l), Value::Vec3(r)) => Value::Vec3(*l + *r),
        (Value::Vec4(l), Value::Vec4(r)) => Value::Vec4(*l + *r),
        (Value::Mat3(l), Value::Mat3(r)) => Value::Mat3(*l + *r),
        (Value::Mat4(l), Value::Mat4(r)) => Value::Mat4(*l + *r),

        // vec + scalar
        (Value::Vec2(l), Value::Float(r)) => Value::Vec2(*l + *r),
        (Value::Vec3(l), Value::Float(r)) => Value::Vec3(*l + *r),
        (Value::Vec4(l), Value::Float(r)) => Value::Vec4(*l + *r),

        // scalar + vec
        (Value::Float(l), Value::Vec2(r)) => Value::Vec2(*l + *r),
        (Value::Float(l), Value::Vec3(r)) => Value::Vec3(*l + *r),
        (Value::Float(l), Value::Vec4(r)) => Value::Vec4(*l + *r),

        // 'broadcast' addition for matrices
        // TODO: test that these work as expected
        // mat + scalar
        (Value::Mat3(l), Value::Float(r)) => Value::Mat3(*l + mat3_broadcast_scalar(*r)),
        (Value::Mat4(l), Value::Float(r)) => Value::Mat4(*l + mat4_broadcast_scalar(*r)),

        // scalar + mat
        (Value::Float(l), Value::Mat3(r)) => Value::Mat3(mat3_broadcast_scalar(*l) + *r),
        (Value::Float(l), Value::Mat4(r)) => Value::Mat4(mat4_broadcast_scalar(*l) + *r),
        _ => todo!(),
    }
}

pub fn subtraction(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
        (Value::UInt(l), Value::UInt(r)) => Value::UInt(l - r),
        (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
        (Value::Double(l), Value::Double(r)) => Value::Double(l - r),
        (Value::Vec2(l), Value::Vec2(r)) => Value::Vec2(*l - *r),
        (Value::Vec3(l), Value::Vec3(r)) => Value::Vec3(*l - *r),
        (Value::Vec4(l), Value::Vec4(r)) => Value::Vec4(*l - *r),
        (Value::Mat3(l), Value::Mat3(r)) => Value::Mat3(*l - *r),
        (Value::Mat4(l), Value::Mat4(r)) => Value::Mat4(*l - *r),

        // vec - scalar
        (Value::Vec2(l), Value::Float(r)) => Value::Vec2(*l - *r),
        (Value::Vec3(l), Value::Float(r)) => Value::Vec3(*l - *r),
        (Value::Vec4(l), Value::Float(r)) => Value::Vec4(*l - *r),

        // scalar - vec
        (Value::Float(l), Value::Vec2(r)) => Value::Vec2(*l - *r),
        (Value::Float(l), Value::Vec3(r)) => Value::Vec3(*l - *r),
        (Value::Float(l), Value::Vec4(r)) => Value::Vec4(*l - *r),

        // 'broadcast' subtraction for matrices
        // mat - scalar
        (Value::Mat3(l), Value::Float(r)) => Value::Mat3(*l - mat3_broadcast_scalar(*r)),
        (Value::Mat4(l), Value::Float(r)) => Value::Mat4(*l - mat4_broadcast_scalar(*r)),
        // scalar - mat
        (Value::Float(l), Value::Mat3(r)) => Value::Mat3(mat3_broadcast_scalar(*l) - *r),
        (Value::Float(l), Value::Mat4(r)) => Value::Mat4(mat4_broadcast_scalar(*l) - *r),
        _ => todo!(),
    }
}

pub fn multiplication(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
        (Value::UInt(l), Value::UInt(r)) => Value::UInt(l * r),
        (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
        (Value::Double(l), Value::Double(r)) => Value::Double(l * r),
        (Value::Vec2(l), Value::Vec2(r)) => Value::Vec2(*l * *r),
        (Value::Vec3(l), Value::Vec3(r)) => Value::Vec3(*l * *r),
        (Value::Vec4(l), Value::Vec4(r)) => Value::Vec4(*l * *r),
        (Value::Mat3(l), Value::Mat3(r)) => Value::Mat3(*l * *r),
        (Value::Mat4(l), Value::Mat4(r)) => Value::Mat4(*l * *r),

        // vec * scalar
        (Value::Vec2(l), Value::Float(r)) => Value::Vec2(*l * *r),
        (Value::Vec3(l), Value::Float(r)) => Value::Vec3(*l * *r),
        (Value::Vec4(l), Value::Float(r)) => Value::Vec4(*l * *r),

        // scalar * vec
        (Value::Float(l), Value::Vec2(r)) => Value::Vec2(*l * *r),
        (Value::Float(l), Value::Vec3(r)) => Value::Vec3(*l * *r),
        (Value::Float(l), Value::Vec4(r)) => Value::Vec4(*l * *r),

        // mat * vec
        (Value::Mat3(l), Value::Vec3(r)) => Value::Vec3((*l).mul_vec3(*r)),
        (Value::Mat4(l), Value::Vec4(r)) => Value::Vec4((*l).mul_vec4(*r)),
        // vec * mat
        // leaving out for now, as it only works with row-major matrices, which are uncommon

        // 'broadcast' multiplication for matrices
        // mat * scalar
        (Value::Mat3(l), Value::Float(r)) => Value::Mat3((*l).mul_scalar(*r)),
        (Value::Mat4(l), Value::Float(r)) => Value::Mat4((*l).mul_scalar(*r)),

        // scalar * mat
        (Value::Float(l), Value::Mat3(r)) => Value::Mat3((*r).mul_scalar(*l)),
        (Value::Float(l), Value::Mat4(r)) => Value::Mat4((*r).mul_scalar(*l)),
        _ => todo!(),
    }
}

pub fn division(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
        (Value::UInt(l), Value::UInt(r)) => Value::UInt(l / r),
        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
        (Value::Double(l), Value::Double(r)) => Value::Double(l / r),
        (Value::Vec2(l), Value::Vec2(r)) => Value::Vec2(*l / *r),
        (Value::Vec3(l), Value::Vec3(r)) => Value::Vec3(*l / *r),
        (Value::Vec4(l), Value::Vec4(r)) => Value::Vec4(*l / *r),
        // mat / mat doesn't make sense

        // vec / scalar
        (Value::Vec2(l), Value::Float(r)) => Value::Vec2(*l / *r),
        (Value::Vec3(l), Value::Float(r)) => Value::Vec3(*l / *r),
        (Value::Vec4(l), Value::Float(r)) => Value::Vec4(*l / *r),

        // scalar / vec
        (Value::Float(l), Value::Vec2(r)) => Value::Vec2(*l / *r),
        (Value::Float(l), Value::Vec3(r)) => Value::Vec3(*l / *r),
        (Value::Float(l), Value::Vec4(r)) => Value::Vec4(*l / *r),

        // 'broadcast' division for matrices
        // mat / scalar
        (Value::Mat3(l), Value::Float(r)) => Value::Mat3((*l).mul_scalar(1.0 / (*r))),
        (Value::Mat4(l), Value::Float(r)) => Value::Mat4((*l).mul_scalar(1.0 / (*r))),
        // scalar / mat
        (Value::Float(l), Value::Mat3(r)) => Value::Mat3((*r).mul_scalar(1.0 / (*l))),
        (Value::Float(l), Value::Mat4(r)) => Value::Mat4((*r).mul_scalar(1.0 / (*l))),
        _ => todo!(),
    }
}

pub fn remainder(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
        (Value::UInt(l), Value::UInt(r)) => Value::UInt(l % r),
        (Value::Float(l), Value::Float(r)) => Value::Float(l % r),
        (Value::Double(l), Value::Double(r)) => Value::Double(l % r),

        // don't think other combinations make sense
        _ => unimplemented!(),
    }
}

// geometric function helpers
fn face_forward_vec2(n: Vec2, i: Vec2, nref: Vec2) -> Vec2 {
    if n.dot(i) < 0.0 {
        nref
    } else {
        -nref
    }
}

fn face_forward_vec3(n: Vec3, i: Vec3, nref: Vec3) -> Vec3 {
    if n.dot(i) < 0.0 {
        nref
    } else {
        -nref
    }
}

fn face_forward_vec4(n: Vec4, i: Vec4, nref: Vec4) -> Vec4 {
    if n.dot(i) < 0.0 {
        nref
    } else {
        -nref
    }
}

fn reflect_vec2(i: Vec2, n: Vec2) -> Vec2 {
    i - 2.0 * n * n.dot(i)
}

fn reflect_vec3(i: Vec3, n: Vec3) -> Vec3 {
    i - 2.0 * n * n.dot(i)
}

fn reflect_vec4(i: Vec4, n: Vec4) -> Vec4 {
    i - 2.0 * n * n.dot(i)
}

fn refract_vec2(i: Vec2, n: Vec2, eta: f32) -> Vec2 {
    let dot = n.dot(i);
    let k = 1.0 - eta * eta * (1.0 - dot * dot);
    if k < 0.0 {
        Vec2::ZERO
    } else {
        eta * i - (eta * dot + k.sqrt()) * n
    }
}

fn refract_vec3(i: Vec3, n: Vec3, eta: f32) -> Vec3 {
    let dot = n.dot(i);
    let k = 1.0 - eta * eta * (1.0 - dot * dot);
    if k < 0.0 {
        Vec3::ZERO
    } else {
        eta * i - (eta * dot + k.sqrt()) * n
    }
}

fn refract_vec4(i: Vec4, n: Vec4, eta: f32) -> Vec4 {
    let dot = n.dot(i);
    let k = 1.0 - eta * eta * (1.0 - dot * dot);
    if k < 0.0 {
        Vec4::ZERO
    } else {
        eta * i - (eta * dot + k.sqrt()) * n
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinFn {
    pub name: &'static str,
    pub args: &'static [&'static str],
    pub func: fn(&[Value]) -> Value,
}

#[derive(Clone, Debug)]
pub struct BuiltinFns {
    pub fns: HashMap<&'static str, BuiltinFn>,
}

impl BuiltinFns {
    pub fn get(&self, name: &str) -> Option<&BuiltinFn> {
        self.fns.get(name)
    }
    pub fn new() -> Self {
        let mut fns = HashMap::new();

        // see https://registry.khronos.org/OpenGL/specs/es/3.0/GLSL_ES_Specification_3.00.pdf
        // section 8.3

        // Vector constructors

        fns.insert(
            "vec2",
            BuiltinFn {
                name: "vec2",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // single float
                        [Value::Float(x)] => Value::Vec2(Vec2::new(*x, *x)),
                        // two floats
                        [Value::Float(x), Value::Float(y)] => Value::Vec2(Vec2::new(*x, *y)),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(*v),
                        // vec3 (drop z)
                        [Value::Vec3(v)] => Value::Vec2(Vec2::new(v.x, v.y)),
                        // vec4 (drop z, w)
                        [Value::Vec4(v)] => Value::Vec2(Vec2::new(v.x, v.y)),
                        _ => panic!("vec2 called with invalid arguments"),
                    }
                },
            },
        );
        fns.insert(
            "vec3",
            BuiltinFn {
                name: "vec3",
                args: &["x", "y", "z"],
                func: |args| {
                    match args {
                        // single float
                        [Value::Float(x)] => Value::Vec3(Vec3::new(*x, *x, *x)),
                        // two floats
                        [Value::Float(x), Value::Float(y)] => Value::Vec3(Vec3::new(*x, *y, 0.0)),
                        // three floats
                        [Value::Float(x), Value::Float(y), Value::Float(z)] => {
                            Value::Vec3(Vec3::new(*x, *y, *z))
                        }
                        // vec2 + float
                        [Value::Vec2(v), Value::Float(z)] => Value::Vec3(Vec3::new(v.x, v.y, *z)),
                        // float + vec2
                        [Value::Float(x), Value::Vec2(v)] => Value::Vec3(Vec3::new(*x, v.x, v.y)),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(*v),
                        // vec4 (drop w)
                        [Value::Vec4(v)] => Value::Vec3(Vec3::new(v.x, v.y, v.z)),
                        _ => panic!("vec3 called with invalid arguments"),
                    }
                },
            },
        );
        fns.insert(
            "vec4",
            BuiltinFn {
                name: "vec4",
                args: &["x", "y", "z", "w"],
                func: |args| {
                    match args {
                        // single float
                        [Value::Float(x)] => Value::Vec4(Vec4::new(*x, *x, *x, *x)),
                        // two floats
                        [Value::Float(x), Value::Float(y)] => {
                            Value::Vec4(Vec4::new(*x, *y, 0.0, 0.0))
                        }
                        // three floats
                        [Value::Float(x), Value::Float(y), Value::Float(z)] => {
                            Value::Vec4(Vec4::new(*x, *y, *z, 0.0))
                        }
                        // four floats
                        [Value::Float(x), Value::Float(y), Value::Float(z), Value::Float(w)] => {
                            Value::Vec4(Vec4::new(*x, *y, *z, *w))
                        }
                        // vec2 + float + float
                        [Value::Vec2(v), Value::Float(z), Value::Float(w)] => {
                            Value::Vec4(Vec4::new(v.x, v.y, *z, *w))
                        }
                        // float + vec2 + float
                        [Value::Float(x), Value::Vec2(v), Value::Float(w)] => {
                            Value::Vec4(Vec4::new(*x, v.x, v.y, *w))
                        }
                        // float + float + vec2
                        [Value::Float(x), Value::Float(y), Value::Vec2(v)] => {
                            Value::Vec4(Vec4::new(*x, *y, v.x, v.y))
                        }
                        // vec2 + vec2
                        [Value::Vec2(v1), Value::Vec2(v2)] => {
                            Value::Vec4(Vec4::new(v1.x, v1.y, v2.x, v2.y))
                        }
                        // vec3 + float
                        [Value::Vec3(v), Value::Float(w)] => {
                            Value::Vec4(Vec4::new(v.x, v.y, v.z, *w))
                        }
                        // float + vec3
                        [Value::Float(x), Value::Vec3(v)] => {
                            Value::Vec4(Vec4::new(*x, v.x, v.y, v.z))
                        }
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(*v),
                        _ => panic!("vec4 called with invalid arguments {:?}", args),
                    }
                },
            },
        );

        // Matrix constructors
        // To initialize a matrix by specifying vectors or scalars, the components are assigned to the matrix elements
        // in column-major order.

        fns.insert(
            "mat2",
            BuiltinFn {
                name: "mat2",
                args: &["c0", "c1"],
                func: |args| {
                    match args {
                        // 4 scalar components as floats, in column-major order
                        [Value::Float(c00), Value::Float(c01), Value::Float(c10), Value::Float(c11)] => {
                            Value::Mat2(Mat2::from_cols_array(&[*c00, *c01, *c10, *c11]))
                        }
                        // vec2 columns (one column per argument)
                        [Value::Vec2(c0), Value::Vec2(c1)] => Value::Mat2(Mat2::from_cols(*c0, *c1)),
                        // mat2
                        [Value::Mat2(m)] => Value::Mat2(*m),
                        _ => panic!("mat2 called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "mat3",
            BuiltinFn {
                name: "mat3",
                args: &["c0", "c1", "c2"],
                func: |args| {
                    match args {
                        // 9 scalar components as floats, in column-major order
                        [Value::Float(c00), Value::Float(c01), Value::Float(c02), Value::Float(c10), Value::Float(c11), Value::Float(c12), Value::Float(c20), Value::Float(c21), Value::Float(c22)] => {
                            Value::Mat3(Mat3::from_cols_array(
                                &[*c00, *c01, *c02, *c10, *c11, *c12, *c20, *c21, *c22],
                            ))
                        }
                        // vec3 columns (one column per argument)
                        [Value::Vec3(c0), Value::Vec3(c1), Value::Vec3(c2)] => {
                            Value::Mat3(Mat3::from_cols(*c0, *c1, *c2))
                        }
                        // mat3
                        [Value::Mat3(m)] => Value::Mat3(*m),
                        _ => panic!("mat3 called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "mat4",
            BuiltinFn {
                name: "mat4",
                args: &["c0", "c1", "c2", "c3"],
                func: |args| {
                    match args {
                        // 16 scalar components as floats, in column-major order
                        [Value::Float(c00), Value::Float(c01), Value::Float(c02), Value::Float(c03), Value::Float(c10), Value::Float(c11), Value::Float(c12), Value::Float(c13), Value::Float(c20), Value::Float(c21), Value::Float(c22), Value::Float(c23), Value::Float(c30), Value::Float(c31), Value::Float(c32), Value::Float(c33)] => {
                            Value::Mat4(Mat4::from_cols_array(
                                &[*c00, *c01, *c02, *c03, *c10, *c11, *c12, *c13, *c20, *c21, *c22, *c23, *c30, *c31, *c32, *c33],
                            ))
                        }
                        // vec4 columns (one column per argument)
                        [Value::Vec4(c0), Value::Vec4(c1), Value::Vec4(c2), Value::Vec4(c3)] => {
                            Value::Mat4(Mat4::from_cols(*c0, *c1, *c2, *c3))
                        }
                        // mat4
                        [Value::Mat4(m)] => Value::Mat4(*m),
                        _ => panic!("mat4 called with invalid arguments"),
                    }
                },
            },
        );

        // Trigonometric functions

        fns.insert(
            "radians",
            BuiltinFn {
                name: "radians",
                args: &["degrees"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(degrees)] => Value::Float(degrees.to_radians()),
                        // vec2
                        [Value::Vec2(v)] => {
                            Value::Vec2(Vec2::new(v.x.to_radians(), v.y.to_radians()))
                        }
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(
                            v.x.to_radians(),
                            v.y.to_radians(),
                            v.z.to_radians(),
                        )),
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.to_radians(),
                            v.y.to_radians(),
                            v.z.to_radians(),
                            v.w.to_radians(),
                        )),
                        _ => panic!("radians called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "degrees",
            BuiltinFn {
                name: "degrees",
                args: &["radians"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(radians)] => Value::Float(radians.to_degrees()),
                        // vec2
                        [Value::Vec2(v)] => {
                            Value::Vec2(Vec2::new(v.x.to_degrees(), v.y.to_degrees()))
                        }
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(
                            v.x.to_degrees(),
                            v.y.to_degrees(),
                            v.z.to_degrees(),
                        )),
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.to_degrees(),
                            v.y.to_degrees(),
                            v.z.to_degrees(),
                            v.w.to_degrees(),
                        )),
                        _ => panic!("degrees called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "sin",
            BuiltinFn {
                name: "sin",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.sin()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.sin(), v.y.sin())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.sin(), v.y.sin(), v.z.sin())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.sin(), v.y.sin(), v.z.sin(), v.w.sin()))
                        }
                        _ => panic!("sin called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "cos",
            BuiltinFn {
                name: "cos",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.cos()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.cos(), v.y.cos())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.cos(), v.y.cos(), v.z.cos())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.cos(), v.y.cos(), v.z.cos(), v.w.cos()))
                        }
                        _ => panic!("cos called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "tan",
            BuiltinFn {
                name: "tan",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.tan()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.tan(), v.y.tan())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.tan(), v.y.tan(), v.z.tan())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.tan(), v.y.tan(), v.z.tan(), v.w.tan()))
                        }
                        _ => panic!("tan called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "asin",
            BuiltinFn {
                name: "asin",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.asin()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.asin(), v.y.asin())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.asin(), v.y.asin(), v.z.asin()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.asin(), v.y.asin(), v.z.asin(), v.w.asin()))
                        }
                        _ => panic!("asin called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "acos",
            BuiltinFn {
                name: "acos",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.acos()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.acos(), v.y.acos())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.acos(), v.y.acos(), v.z.acos()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.acos(), v.y.acos(), v.z.acos(), v.w.acos()))
                        }
                        _ => panic!("acos called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "atan",
            BuiltinFn {
                name: "atan",
                args: &["x"],
                func: |args| {
                    match args {
                        // atan(y_over_x)
                        // float
                        [Value::Float(x)] => Value::Float(x.atan()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.atan(), v.y.atan())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.atan(), v.y.atan(), v.z.atan()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.atan(), v.y.atan(), v.z.atan(), v.w.atan()))
                        }
                        // atan(y, x)
                        // float
                        [Value::Float(y), Value::Float(x)] => Value::Float(y.atan2(*x)),
                        // vec2
                        [Value::Vec2(y), Value::Vec2(x)] => {
                            Value::Vec2(Vec2::new(y.y.atan2(y.x), x.y.atan2(x.x)))
                        }
                        // vec3
                        [Value::Vec3(y), Value::Vec3(x)] => {
                            Value::Vec3(Vec3::new(y.y.atan2(y.x), x.y.atan2(x.x), x.z.atan2(x.z)))
                        }
                        // vec4
                        [Value::Vec4(y), Value::Vec4(x)] => Value::Vec4(Vec4::new(
                            y.y.atan2(y.x),
                            x.y.atan2(x.x),
                            x.z.atan2(x.z),
                            x.w.atan2(x.w),
                        )),
                        _ => panic!("atan called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "sinh",
            BuiltinFn {
                name: "sinh",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.sinh()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.sinh(), v.y.sinh())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.sinh(), v.y.sinh(), v.z.sinh()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.sinh(), v.y.sinh(), v.z.sinh(), v.w.sinh()))
                        }
                        _ => panic!("sinh called with invalid arguments"),
                    }
                },
            },
        );

        // Exponential functions

        fns.insert(
            "pow",
            BuiltinFn {
                name: "pow",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Float(x.powf(*y)),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::Vec2(Vec2::new(v.x.powf(w.x), v.y.powf(w.y)))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::Vec3(Vec3::new(v.x.powf(w.x), v.y.powf(w.y), v.z.powf(w.z)))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => Value::Vec4(Vec4::new(
                            v.x.powf(w.x),
                            v.y.powf(w.y),
                            v.z.powf(w.z),
                            v.w.powf(w.w),
                        )),
                        _ => panic!("pow called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "exp",
            BuiltinFn {
                name: "exp",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.exp()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.exp(), v.y.exp())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.exp(), v.y.exp(), v.z.exp())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.exp(), v.y.exp(), v.z.exp(), v.w.exp()))
                        }
                        _ => panic!("exp called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "log",
            BuiltinFn {
                name: "log",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.ln()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.ln(), v.y.ln())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.ln(), v.y.ln(), v.z.ln())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.ln(), v.y.ln(), v.z.ln(), v.w.ln()))
                        }
                        _ => panic!("log called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "exp2",
            BuiltinFn {
                name: "exp2",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.exp2()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.exp2(), v.y.exp2())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.exp2(), v.y.exp2(), v.z.exp2()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.exp2(), v.y.exp2(), v.z.exp2(), v.w.exp2()))
                        }
                        _ => panic!("exp2 called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "log2",
            BuiltinFn {
                name: "log2",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.log2()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.log2(), v.y.log2())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.log2(), v.y.log2(), v.z.log2()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.log2(), v.y.log2(), v.z.log2(), v.w.log2()))
                        }
                        _ => panic!("log2 called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "sqrt",
            BuiltinFn {
                name: "sqrt",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.sqrt()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.sqrt(), v.y.sqrt())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.sqrt(), v.y.sqrt(), v.z.sqrt()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.sqrt(), v.y.sqrt(), v.z.sqrt(), v.w.sqrt()))
                        }
                        _ => panic!("sqrt called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "inversesqrt",
            BuiltinFn {
                name: "inversesqrt",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.sqrt().recip()),
                        // vec2
                        [Value::Vec2(v)] => {
                            Value::Vec2(Vec2::new(v.x.sqrt().recip(), v.y.sqrt().recip()))
                        }
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(
                            v.x.sqrt().recip(),
                            v.y.sqrt().recip(),
                            v.z.sqrt().recip(),
                        )),
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.sqrt().recip(),
                            v.y.sqrt().recip(),
                            v.z.sqrt().recip(),
                            v.w.sqrt().recip(),
                        )),
                        _ => panic!("inversesqrt called with invalid arguments"),
                    }
                },
            },
        );

        // Common math functions
        fns.insert(
            "abs",
            BuiltinFn {
                name: "abs",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.abs()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.abs(), v.y.abs())),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(Vec3::new(v.x.abs(), v.y.abs(), v.z.abs())),
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.abs(), v.y.abs(), v.z.abs(), v.w.abs()))
                        }
                        _ => panic!("abs called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "floor",
            BuiltinFn {
                name: "floor",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.floor()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.floor(), v.y.floor())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.floor(), v.y.floor(), v.z.floor()))
                        }
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.floor(),
                            v.y.floor(),
                            v.z.floor(),
                            v.w.floor(),
                        )),
                        _ => panic!("floor called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "ceil",
            BuiltinFn {
                name: "ceil",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.ceil()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.ceil(), v.y.ceil())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.ceil(), v.y.ceil(), v.z.ceil()))
                        }
                        // vec4
                        [Value::Vec4(v)] => {
                            Value::Vec4(Vec4::new(v.x.ceil(), v.y.ceil(), v.z.ceil(), v.w.ceil()))
                        }
                        _ => panic!("ceil called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "round",
            BuiltinFn {
                name: "round",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.round()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.round(), v.y.round())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.round(), v.y.round(), v.z.round()))
                        }
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.round(),
                            v.y.round(),
                            v.z.round(),
                            v.w.round(),
                        )),
                        _ => panic!("round called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "fract",
            BuiltinFn {
                name: "fract",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.fract()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.fract(), v.y.fract())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.fract(), v.y.fract(), v.z.fract()))
                        }
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.fract(),
                            v.y.fract(),
                            v.z.fract(),
                            v.w.fract(),
                        )),
                        _ => panic!("fract called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "mod",
            BuiltinFn {
                name: "mod",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Float(x % y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::Vec2(Vec2::new(v.x % w.x, v.y % w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::Vec3(Vec3::new(v.x % w.x, v.y % w.y, v.z % w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::Vec4(Vec4::new(v.x % w.x, v.y % w.y, v.z % w.z, v.w % w.w))
                        }
                        _ => panic!("mod called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "min",
            BuiltinFn {
                name: "min",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Float(x.min(*y)),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::Vec2(Vec2::new(v.x.min(w.x), v.y.min(w.y)))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::Vec3(Vec3::new(v.x.min(w.x), v.y.min(w.y), v.z.min(w.z)))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => Value::Vec4(Vec4::new(
                            v.x.min(w.x),
                            v.y.min(w.y),
                            v.z.min(w.z),
                            v.w.min(w.w),
                        )),
                        _ => panic!("min called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "max",
            BuiltinFn {
                name: "max",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Float(x.max(*y)),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::Vec2(Vec2::new(v.x.max(w.x), v.y.max(w.y)))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::Vec3(Vec3::new(v.x.max(w.x), v.y.max(w.y), v.z.max(w.z)))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => Value::Vec4(Vec4::new(
                            v.x.max(w.x),
                            v.y.max(w.y),
                            v.z.max(w.z),
                            v.w.max(w.w),
                        )),
                        _ => panic!("max called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "clamp",
            BuiltinFn {
                name: "clamp",
                args: &["x", "minVal", "maxVal"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(min), Value::Float(max)] => {
                            Value::Float(x.clamp(*min, *max))
                        }
                        // vec2
                        [Value::Vec2(v), Value::Vec2(min), Value::Vec2(max)] => {
                            Value::Vec2(Vec2::new(v.x.clamp(min.x, max.x), v.y.clamp(min.y, max.y)))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(min), Value::Vec3(max)] => {
                            Value::Vec3(Vec3::new(
                                v.x.clamp(min.x, max.x),
                                v.y.clamp(min.y, max.y),
                                v.z.clamp(min.z, max.z),
                            ))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(min), Value::Vec4(max)] => {
                            Value::Vec4(Vec4::new(
                                v.x.clamp(min.x, max.x),
                                v.y.clamp(min.y, max.y),
                                v.z.clamp(min.z, max.z),
                                v.w.clamp(min.w, max.w),
                            ))
                        }
                        _ => panic!("clamp called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "mix",
            BuiltinFn {
                name: "mix",
                args: &["x", "y", "a"],
                func: |args| {
                    match *args {
                        // float
                        [Value::Float(x), Value::Float(y), Value::Float(a)] => {
                            Value::Float(x * (1.0 - a) + y * a)
                        }
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w), Value::Vec2(a)] => {
                            Value::Vec2(v * (Vec2::splat(1.0) - a) + w * a)
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w), Value::Vec3(a)] => {
                            Value::Vec3(v * (Vec3::splat(1.0) - a) + w * a)
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w), Value::Vec4(a)] => {
                            Value::Vec4(v * (Vec4::splat(1.0) - a) + w * a)
                        }
                        _ => panic!("mix called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "step",
            BuiltinFn {
                name: "step",
                args: &["edge", "x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(edge), Value::Float(x)] => {
                            Value::Float(if x < edge { 0.0 } else { 1.0 })
                        }
                        // vec2
                        [Value::Vec2(edge), Value::Vec2(x)] => Value::Vec2(Vec2::new(
                            if x.x < edge.x { 0.0 } else { 1.0 },
                            if x.y < edge.y { 0.0 } else { 1.0 },
                        )),
                        // vec3
                        [Value::Vec3(edge), Value::Vec3(x)] => Value::Vec3(Vec3::new(
                            if x.x < edge.x { 0.0 } else { 1.0 },
                            if x.y < edge.y { 0.0 } else { 1.0 },
                            if x.z < edge.z { 0.0 } else { 1.0 },
                        )),
                        // vec4
                        [Value::Vec4(edge), Value::Vec4(x)] => Value::Vec4(Vec4::new(
                            if x.x < edge.x { 0.0 } else { 1.0 },
                            if x.y < edge.y { 0.0 } else { 1.0 },
                            if x.z < edge.z { 0.0 } else { 1.0 },
                            if x.w < edge.w { 0.0 } else { 1.0 },
                        )),
                        _ => panic!("step called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "smoothstep",
            BuiltinFn {
                name: "smoothstep",
                args: &["edge0", "edge1", "x"],
                func: |args| {
                    match *args {
                        // float
                        [Value::Float(edge0), Value::Float(edge1), Value::Float(x)] => {
                            let t = (x - edge0) / (edge1 - edge0);
                            Value::Float(t * t * (3.0 - 2.0 * t))
                        }
                        // vec2
                        [Value::Vec2(edge0), Value::Vec2(edge1), Value::Vec2(x)] => {
                            let t = (x - edge0) / (edge1 - edge0);
                            Value::Vec2(Vec2::new(
                                t.x * t.x * (3.0 - 2.0 * t.x),
                                t.y * t.y * (3.0 - 2.0 * t.y),
                            ))
                        }
                        // vec3
                        [Value::Vec3(edge0), Value::Vec3(edge1), Value::Vec3(x)] => {
                            let t = (x - edge0) / (edge1 - edge0);
                            Value::Vec3(Vec3::new(
                                t.x * t.x * (3.0 - 2.0 * t.x),
                                t.y * t.y * (3.0 - 2.0 * t.y),
                                t.z * t.z * (3.0 - 2.0 * t.z),
                            ))
                        }
                        // vec4
                        [Value::Vec4(edge0), Value::Vec4(edge1), Value::Vec4(x)] => {
                            let t = (x - edge0) / (edge1 - edge0);
                            Value::Vec4(Vec4::new(
                                t.x * t.x * (3.0 - 2.0 * t.x),
                                t.y * t.y * (3.0 - 2.0 * t.y),
                                t.z * t.z * (3.0 - 2.0 * t.z),
                                t.w * t.w * (3.0 - 2.0 * t.w),
                            ))
                        }
                        _ => panic!("smoothstep called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "sign",
            BuiltinFn {
                name: "sign",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.signum()),
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(Vec2::new(v.x.signum(), v.y.signum())),
                        // vec3
                        [Value::Vec3(v)] => {
                            Value::Vec3(Vec3::new(v.x.signum(), v.y.signum(), v.z.signum()))
                        }
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(Vec4::new(
                            v.x.signum(),
                            v.y.signum(),
                            v.z.signum(),
                            v.w.signum(),
                        )),
                        _ => panic!("sign called with invalid arguments"),
                    }
                },
            },
        );

        // Geometric functions

        fns.insert(
            "length",
            BuiltinFn {
                name: "length",
                args: &["x"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x)] => Value::Float(x.abs()),
                        // vec2
                        [Value::Vec2(v)] => Value::Float(v.length()),
                        // vec3
                        [Value::Vec3(v)] => Value::Float(v.length()),
                        // vec4
                        [Value::Vec4(v)] => Value::Float(v.length()),
                        _ => panic!("length called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "distance",
            BuiltinFn {
                name: "distance",
                args: &["p0", "p1"],
                func: |args| {
                    match *args {
                        // float
                        [Value::Float(p0), Value::Float(p1)] => Value::Float((p0 - p1).abs()),
                        // vec2
                        [Value::Vec2(p0), Value::Vec2(p1)] => Value::Float((p0 - p1).length()),
                        // vec3
                        [Value::Vec3(p0), Value::Vec3(p1)] => Value::Float((p0 - p1).length()),
                        // vec4
                        [Value::Vec4(p0), Value::Vec4(p1)] => Value::Float((p0 - p1).length()),
                        _ => panic!("distance called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "dot",
            BuiltinFn {
                name: "dot",
                args: &["x", "y"],
                func: |args| {
                    match *args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Float(x * y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => Value::Float(v.dot(w)),
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => Value::Float(v.dot(w)),
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => Value::Float(v.dot(w)),
                        _ => panic!("dot called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "cross",
            BuiltinFn {
                name: "cross",
                args: &["x", "y"],
                func: |args| {
                    match *args {
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => Value::Vec3(v.cross(w)),
                        _ => panic!("cross called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "normalize",
            BuiltinFn {
                name: "normalize",
                args: &["x"],
                func: |args| {
                    match args {
                        // vec2
                        [Value::Vec2(v)] => Value::Vec2(v.normalize()),
                        // vec3
                        [Value::Vec3(v)] => Value::Vec3(v.normalize()),
                        // vec4
                        [Value::Vec4(v)] => Value::Vec4(v.normalize()),
                        _ => panic!("normalize called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "faceforward",
            BuiltinFn {
                name: "faceforward",
                args: &["N", "I", "Nref"],
                func: |args| {
                    match args {
                        // vec2
                        [Value::Vec2(N), Value::Vec2(I), Value::Vec2(Nref)] => {
                            Value::Vec2(face_forward_vec2(*N, *I, *Nref))
                        }
                        // vec3
                        [Value::Vec3(N), Value::Vec3(I), Value::Vec3(Nref)] => {
                            Value::Vec3(face_forward_vec3(*N, *I, *Nref))
                        }
                        // vec4
                        [Value::Vec4(N), Value::Vec4(I), Value::Vec4(Nref)] => {
                            Value::Vec4(face_forward_vec4(*N, *I, *Nref))
                        }
                        _ => panic!("faceforward called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "reflect",
            BuiltinFn {
                name: "reflect",
                args: &["I", "N"],
                func: |args| {
                    match args {
                        // vec2
                        [Value::Vec2(I), Value::Vec2(N)] => Value::Vec2(reflect_vec2(*I, *N)),
                        // vec3
                        [Value::Vec3(I), Value::Vec3(N)] => Value::Vec3(reflect_vec3(*I, *N)),
                        // vec4
                        [Value::Vec4(I), Value::Vec4(N)] => Value::Vec4(reflect_vec4(*I, *N)),
                        _ => panic!("reflect called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "refract",
            BuiltinFn {
                name: "refract",
                args: &["I", "N", "eta"],
                func: |args| {
                    match args {
                        // vec2
                        [Value::Vec2(I), Value::Vec2(N), Value::Float(eta)] => {
                            Value::Vec2(refract_vec2(*I, *N, *eta))
                        }
                        // vec3
                        [Value::Vec3(I), Value::Vec3(N), Value::Float(eta)] => {
                            Value::Vec3(refract_vec3(*I, *N, *eta))
                        }
                        // vec4
                        [Value::Vec4(I), Value::Vec4(N), Value::Float(eta)] => {
                            Value::Vec4(refract_vec4(*I, *N, *eta))
                        }
                        _ => panic!("refract called with invalid arguments"),
                    }
                },
            },
        );

        // Matrix functions

        fns.insert(
            "matrixCompMult",
            BuiltinFn {
                name: "matrixCompMult",
                args: &["x", "y"],
                func: |args| {
                    todo!("matrixCompMult()");
                },
            },
        );

        fns.insert(
            "outerProduct",
            BuiltinFn {
                name: "outerProduct",
                args: &["c", "r"],
                func: |args| {
                    todo!("outerProduct()");
                },
            },
        );

        fns.insert(
            "transpose",
            BuiltinFn {
                name: "transpose",
                args: &["x"],
                func: |args| {
                    match args {
                        // mat2
                        [Value::Mat2(x)] => Value::Mat2(x.transpose()),
                        // mat3
                        [Value::Mat3(x)] => Value::Mat3(x.transpose()),
                        // mat4
                        [Value::Mat4(x)] => Value::Mat4(x.transpose()),
                        _ => panic!("transpose called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "determinant",
            BuiltinFn {
                name: "determinant",
                args: &["x"],
                func: |args| {
                    match args {
                        // mat2
                        [Value::Mat2(x)] => Value::Float(x.determinant()),
                        // mat3
                        [Value::Mat3(x)] => Value::Float(x.determinant()),
                        // mat4
                        [Value::Mat4(x)] => Value::Float(x.determinant()),
                        _ => panic!("determinant called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "inverse",
            BuiltinFn {
                name: "inverse",
                args: &["x"],
                func: |args| {
                    match args {
                        // mat2
                        [Value::Mat2(x)] => Value::Mat2(x.inverse()),
                        // mat3
                        [Value::Mat3(x)] => Value::Mat3(x.inverse()),
                        // mat4
                        [Value::Mat4(x)] => Value::Mat4(x.inverse()),
                        _ => panic!("inverse called with invalid arguments"),
                    }
                },
            },
        );

        // Vector relational functions

        fns.insert(
            "lessThan",
            BuiltinFn {
                name: "lessThan",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x < y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x < w.x, v.y < w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x < w.x, v.y < w.y, v.z < w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x < w.x, v.y < w.y, v.z < w.z, v.w < w.w))
                        }
                        _ => panic!("lessThan called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "lessThanEqual",
            BuiltinFn {
                name: "lessThanEqual",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x <= y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x <= w.x, v.y <= w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x <= w.x, v.y <= w.y, v.z <= w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x <= w.x, v.y <= w.y, v.z <= w.z, v.w <= w.w))
                        }
                        _ => panic!("lessThanEqual called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "greaterThan",
            BuiltinFn {
                name: "greaterThan",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x > y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x > w.x, v.y > w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x > w.x, v.y > w.y, v.z > w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x > w.x, v.y > w.y, v.z > w.z, v.w > w.w))
                        }
                        _ => panic!("greaterThan called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "greaterThanEqual",
            BuiltinFn {
                name: "greaterThanEqual",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x >= y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x >= w.x, v.y >= w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x >= w.x, v.y >= w.y, v.z >= w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x >= w.x, v.y >= w.y, v.z >= w.z, v.w >= w.w))
                        }
                        _ => panic!("greaterThanEqual called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "equal",
            BuiltinFn {
                name: "equal",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x == y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x == w.x, v.y == w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x == w.x, v.y == w.y, v.z == w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x == w.x, v.y == w.y, v.z == w.z, v.w == w.w))
                        }
                        _ => panic!("equal called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "notEqual",
            BuiltinFn {
                name: "notEqual",
                args: &["x", "y"],
                func: |args| {
                    match args {
                        // float
                        [Value::Float(x), Value::Float(y)] => Value::Bool(x != y),
                        // vec2
                        [Value::Vec2(v), Value::Vec2(w)] => {
                            Value::BVec2(BVec2::new(v.x != w.x, v.y != w.y))
                        }
                        // vec3
                        [Value::Vec3(v), Value::Vec3(w)] => {
                            Value::BVec3(BVec3::new(v.x != w.x, v.y != w.y, v.z != w.z))
                        }
                        // vec4
                        [Value::Vec4(v), Value::Vec4(w)] => {
                            Value::BVec4(BVec4::new(v.x != w.x, v.y != w.y, v.z != w.z, v.w != w.w))
                        }
                        _ => panic!("notEqual called with invalid arguments"),
                    }
                },
            },
        );

        // Logical functions

        fns.insert(
            "all",
            BuiltinFn {
                name: "all",
                args: &["x"],
                func: |args| {
                    match args {
                        // bvec2
                        [Value::BVec2(x)] => Value::Bool(x.x && x.y),
                        // bvec3
                        [Value::BVec3(x)] => Value::Bool(x.x && x.y && x.z),
                        // bvec4
                        [Value::BVec4(x)] => Value::Bool(x.x && x.y && x.z && x.w),
                        _ => panic!("all called with invalid arguments"),
                    }
                },
            },
        );

        fns.insert(
            "any",
            BuiltinFn {
                name: "any",
                args: &["x"],
                func: |args| {
                    match args {
                        // bvec2
                        [Value::BVec2(x)] => Value::Bool(x.x || x.y),
                        // bvec3
                        [Value::BVec3(x)] => Value::Bool(x.x || x.y || x.z),
                        // bvec4
                        [Value::BVec4(x)] => Value::Bool(x.x || x.y || x.z || x.w),
                        _ => panic!("any called with invalid arguments"),
                    }
                },
            },
        );

        // Texture lookup functions

        fns.insert(
            "textureSize",
            BuiltinFn {
                name: "textureSize",
                args: &["sampler", "lod"],
                func: |args| {
                    // can't implement this until ivec types are implemented, as the return type is ivec2/ivec3
                    todo!("textureSize()")
                },
            },
        );

        fns.insert(
            "texture",
            BuiltinFn {
                name: "texture",
                args: &["sampler", "coord"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("texture()");
                },
            },
        );

        fns.insert(
            "textureProj",
            BuiltinFn {
                name: "textureProj",
                args: &["sampler", "coord"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureProj()");
                },
            },
        );

        fns.insert(
            "textureLod",
            BuiltinFn {
                name: "textureLod",
                args: &["sampler", "coord", "lod"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureLod()");
                },
            },
        );

        fns.insert(
            "textureOffset",
            BuiltinFn {
                name: "textureOffset",
                args: &["sampler", "coord", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureOffset()");
                },
            },
        );

        fns.insert(
            "texelFetch",
            BuiltinFn {
                name: "texelFetch",
                args: &["sampler", "coord", "lod"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("texelFetch()");
                },
            },
        );

        fns.insert(
            "texelFetchOffset",
            BuiltinFn {
                name: "texelFetchOffset",
                args: &["sampler", "coord", "lod", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("texelFetchOffset()");
                },
            },
        );

        fns.insert(
            "textureGrad",
            BuiltinFn {
                name: "textureGrad",
                args: &["sampler", "coord", "dPdx", "dPdy"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureGrad()");
                },
            },
        );

        fns.insert(
            "textureGradOffset",
            BuiltinFn {
                name: "textureGradOffset",
                args: &["sampler", "coord", "dPdx", "dPdy", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureGradOffset()");
                },
            },
        );

        fns.insert(
            "textureProjOffset",
            BuiltinFn {
                name: "textureProjOffset",
                args: &["sampler", "coord", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureProjOffset()");
                },
            },
        );

        fns.insert(
            "textureLodOffset",
            BuiltinFn {
                name: "textureLodOffset",
                args: &["sampler", "coord", "lod", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureLodOffset()");
                },
            },
        );

        fns.insert(
            "textureProjLod",
            BuiltinFn {
                name: "textureProjLod",
                args: &["sampler", "coord", "lod"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureProjLod()");
                },
            },
        );

        fns.insert(
            "textureProjLodOffset",
            BuiltinFn {
                name: "textureProjLodOffset",
                args: &["sampler", "coord", "lod", "offset"],
                func: |args| {
                    // can't implement this until sampling textures is implemented
                    todo!("textureProjLodOffset()");
                },
            },
        );

        Self { fns }
    }
}
