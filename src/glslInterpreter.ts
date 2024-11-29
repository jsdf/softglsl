/*
example ast:

```
[
  {
    "ty": {
      "VersionDirective": {
        "version": [
          300,
          {
            "start": 10,
            "end": 13
          }
        ],
        "profile": {
          "Some": [
            "Es",
            {
              "start": 14,
              "end": 16
            }
          ]
        }
      }
    },
    "span": {
      "start": 1,
      "end": 16
    }
  },
  {
    "ty": {
      "FnDef": {
        "return_type": {
          "ty": {
            "Single": {
              "Scalar": "Void"
            }
          },
          "qualifiers": [],
          "span": {
            "start": 17,
            "end": 21
          }
        },
        "ident": {
          "name": "main",
          "span": {
            "start": 22,
            "end": 26
          }
        },
        "params": [],
        "body": {
          "contents": [],
          "span": {
            "start": 29,
            "end": 31
          }
        }
      }
    },
    "span": {
      "start": 17,
      "end": 31
    }
  }
]
```

rust ast:
```
//! Abstract syntax tree types and functionality.
//!
//! There are a lot of types used to represent specific things. Some common ones worth mentioning:
//! - [`Node`] and [`NodeTy`] - A node representing a statement.
//! - [`Expr`] and [`ExprTy`] - A node representing an expression; this will never be found standalone but part of
//!   a `Node` of some kind.
//! - [`Ident`] - A general identifier of some kind.
//! - [`Omittable`] - A type representing optional grammar elements.
//!
//! In general, types are laid out as follows:
//! ```ignore
//! pub struct _LangFeature_ {
//!     /// The specific type of this node.
//!     pub ty: _LangFeature_Ty,
//!     /// A span of the entire node.
//!     pub span: Span
//! }
//!
//! pub enum _LangFeature_Ty {
//!      
//!      
//! }
//! ```
//!
//! Since conditional compilation is resolved before the AST is generated, conditional compilation directives are
//! not part of the AST.

use crate::{
	diag::Syntax,
	lexer::{NumType, Token},
	Either, Span, Spanned,
};

use serde::{Deserialize, Serialize};

/// This type represents a value which can be omitted in accordance to the GLSL specification.
///
/// This type is equivalent to [`Option`]. The reason for the two types is to differentiate when a node's field is
/// empty because it can legally be omitted (this type), and when a node's field is empty because the parser used
/// an error recovery strategy due to a syntax error (`Option`).
///
/// This type implements the [`From`] trait for conversions to/from [`Option`], as well as a handful of helper
/// methods which match the equivalent `Option` signature.
#[derive(
	Debug,
	Clone,
	Copy,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	Hash,
	Serialize,
	Deserialize,
)]
pub enum Omittable<T> {
	/// Some value of type `T`.
	Some(T),
	/// No value.
	None,
}

/// An identifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ident {
	pub name: String,
	pub span: Span,
}

/// A node within the abstract syntax tree.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Node {
	pub ty: NodeTy,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NodeTy {
	/// An empty statement, e.g. `;`.
	Empty,
	/// An expression statement, e.g. `5 + 1;` or `i++;`.
	Expr(Expr),
	/// A block scope, e.g. `{ int i; }`.
	Block(Scope),
	/// A variable definition, e.g. `int i;`.
	VarDef { type_: Type, ident: Ident },
	/// A variable definition containing multiple variables, e.g. `int i, j, k;`.
	VarDefs(Vec<(Type, Ident)>),
	/// A variable definition with initialization, e.g. `int i = 0;`.
	VarDefInit {
		type_: Type,
		ident: Ident,
		value: Option<Expr>,
	},
	/// A variable definition with initialization, containing multiple variables, e.g. `int i, j, k = 0;`.
	VarDefInits(Vec<(Type, Ident)>, Option<Expr>),
	/// An interface block definition, e.g. `out V { vec2 pos; } v_out;`.
	InterfaceDef {
		qualifiers: Vec<Qualifier>,
		ident: Ident,
		body: Scope,
		instance: Omittable<Expr>,
	},
	/// A list of qualifiers, e.g. `layout(points) in;`.
	Qualifiers(Vec<Qualifier>),
	/// A function declaration, e.g. `int foo(int i);`.
	FnDecl {
		return_type: Type,
		ident: Ident,
		params: Vec<Param>,
	},
	/// A function definition, e.g. `int foo(int i) { return i + 1; }`.
	FnDef {
		return_type: Type,
		ident: Ident,
		params: Vec<Param>,
		body: Scope,
	},
	/// A subroutine type declaration, e.g. `subroutine int foo(int i);`.
	SubroutineTypeDecl {
		return_type: Type,
		ident: Ident,
		params: Vec<Param>,
	},
	/// A subroutine associated function definition, e.g. `subroutine(foo) int foo_1(int i) {...}`.
	SubroutineFnDef {
		associations: Vec<Ident>,
		return_type: Type,
		ident: Ident,
		params: Vec<Param>,
		body: Option<Scope>,
	},
	/// A subroutine uniform definition, e.g. `subroutine uniform foo my_foo;`.
	SubroutineUniformDef { type_: Type, ident: Ident },
	/// A struct declaration, e.g. `struct FooBar;`. This is an illegal GLSL statement.
	StructDecl {
		qualifiers: Vec<Qualifier>,
		ident: Ident,
	},
	/// A struct definition, e.g. `struct FooBar { mat4 m; };`.
	StructDef {
		qualifiers: Vec<Qualifier>,
		ident: Ident,
		body: Scope,
		instance: Omittable<Ident>,
	},
	/// An if statement, e.g. `if (true) {...} else {...}`.
	If(Vec<IfBranch>),
	/// A switch statement, e.g. `switch (true) { default: return; }`.
	Switch {
		cond: Option<Expr>,
		cases: Vec<SwitchCase>,
	},
	/// A for loop, e.g. `for (int i = 0; i<5; i++) {...}`.
	For {
		init: Option<Box<Node>>,
		cond: Option<Box<Node>>,
		inc: Option<Box<Node>>,
		body: Option<Scope>,
	},
	/// A while loop, e.g `while (true) {...}`.
	While { cond: Option<Expr>, body: Scope },
	/// A do-while loop, e.g. `do {...} while (true);`.
	DoWhile { body: Scope, cond: Option<Expr> },
	/// A break statement, e.g. `break;`.
	Break,
	/// A continue statement, e.g. `continue;`.
	Continue,
	/// A discard statement, e.g. `discard;`.
	Discard,
	/// A return statement, e.g. `return 5;`.
	Return { value: Omittable<Expr> },
	/// An empty directive, i.e. just a `#` on it's own line.
	EmptyDirective,
	/// A version directive, e.g. `#version 450 core`.
	VersionDirective {
		version: Option<Spanned<usize>>,
		profile: Omittable<Spanned<ProfileTy>>,
	},
	/// An extension directive, e.g. `#extension all : enable`.
	ExtensionDirective {
		name: Option<Spanned<String>>,
		behaviour: Option<Spanned<BehaviourTy>>,
	},
	/// A line directive, e.g. `#line 1`.
	LineDirective {
		line: Option<Spanned<usize>>,
		src_str_num: Omittable<Spanned<usize>>,
	},
	/// A define directive, e.g. `#define TOGGLE 1`.
	DefineDirective {
		macro_: Macro,
		replacement_tokens: Vec<Spanned<Token>>,
	},
	/// An undef directive, e.g. `#undef TOGGLE`.
	UndefDirective {
		/// The name of the macro to un-define.
		name: Omittable<Ident>,
	},
	/// An error directive, e.g. `#error foo bar`.
	ErrorDirective { message: Omittable<Spanned<String>> },
	/// A pragma directive, e.g. `#pragma strict`.
	PragmaDirective { options: Omittable<Spanned<String>> },
}

/// A scope of nodes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Scope {
	pub contents: Vec<Node>,
	pub span: Span,
}

/// A parameter in a function definition/declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Param {
	pub type_: Type,
	pub ident: Omittable<Ident>,
	pub span: Span,
}

/// An if-statement branch.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfBranch {
	pub condition: Spanned<IfCondition>,
	pub body: Option<Scope>,
	pub span: Span,
}

/// The condition of an if-statement branch.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IfCondition {
	If(Option<Expr>),
	ElseIf(Option<Expr>),
	Else,
}

/// A switch case.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SwitchCase {
	pub expr: Either<Option<Expr>, ()>,
	pub body: Option<Scope>,
	pub span: Span,
}

/// A type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Type {
	pub ty: TypeTy,
	pub qualifiers: Vec<Qualifier>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeTy {
	/// A type which has only a single value.
	Single(Primitive),
	/// An array type which contains zero or more values.
	Array(Primitive, ArrSize),
	/// A 2D array type which contains zero or more values.
	///
	/// - `1` - Size of the outer array.
	/// - `2` - Size of each inner array.
	Array2D(Primitive, ArrSize, ArrSize),
	/// An n-dimensional array type which contains zero or more values.
	///
	/// - `1` - Vec containing the sizes of arrays, starting with the outer-most array.
	ArrayND(Primitive, Vec<ArrSize>),
}

/// An array size.
pub type ArrSize = Omittable<Expr>;

/// A primitive language type.
///
/// The reason for the separation of this enum and the [`Fundamental`] enum is that all fundamental types (aside
/// from `void`) can be either a scalar or an n-dimensional vector. Furthermore, any of the types in this enum can
/// be on their own or as part of a n-dimensional array.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Primitive {
	/// A scalar primitive type.
	Scalar(Fundamental),
	/// A n-dimensional primitive type, where `2 <= n <= 4`.
	Vector(Fundamental, usize),
	/// A float matrix type.
	///
	/// - `0` - Column count.
	/// - `1` - Row count.
	Matrix(usize, usize),
	/// A double matrix type.
	///
	/// - `0` - Column count.
	/// - `1` - Row count.
	DMatrix(usize, usize),
	/// A struct type.
	Struct(Ident),
	/// A sampler type.
	///
	/// - `0` - Data type.
	/// - `1` - Texture type.
	///
	/// # Invariants
	/// - The data type is guaranteed to be one of `Fundamental::Float|Int|UInt`.
	Sampler(Fundamental, TexType),
	/// An image type.
	///
	/// - `0` - Data type.
	/// - `1` - Texture type.
	///
	/// # Invariants
	/// - The data type is guaranteed to be one of `Fundamental::Float|Int|UInt`.
	/// - The texture type is guaranteed to be none of the `TexType::Shadow*` variants.
	Image(Fundamental, TexType),
	/// An atomic counter type.
	Atomic,
}

/// A fundamental type.
///
/// These are the most fundamental types in the language, on which all other types are composed.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Fundamental {
	Void,
	Bool,
	Int,
	UInt,
	Float,
	Double,
}

/// The texture type of a `sampler_`/`isampler_`/`usampler_` or `image_`/`iimage_` primitive type.
///
/// The names of the variants match the type name suffixes, but any 1D/2D/3D letters are flipped because Rust
/// typenames cannot begin with a digit.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TexType {
	/// `_1D`
	D1,
	/// `_2D`
	D2,
	/// `_3D`
	D3,
	/// `_Cube`
	Cube,
	/// `_2DRect`
	D2Rect,
	/// `_1DArray`
	D1Array,
	/// `_2DArray`
	D2Array,
	/// `_CubeArray`
	CubeArray,
	/// `_Buffer`
	Buffer,
	/// `_2DMS`
	D2Multisample,
	/// `_2DMSArray`
	D2MultisampleArray,
	/// `_1DShadow`
	D1Shadow,
	/// `_2DShadow`
	D2Shadow,
	/// `_3DShadow`
	D3Shadow,
	/// `_CubeShadow`
	CubeShadow,
	/// `_2DRectShadow`
	D2RectShadow,
	/// `_1DArrayShadow`
	D1ArrayShadow,
	/// `_2DArrayShadow`
	D2ArrayShadow,
	/// `_CubeArrayShadow`
	CubeArrayShadow,
}

/// A type qualifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Qualifier {
	pub ty: QualifierTy,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum QualifierTy {
	Const,
	In,
	Out,
	InOut,
	Attribute,
	Uniform,
	Varying,
	Buffer,
	Shared,
	Centroid,
	Sample,
	Patch,
	Layout(Vec<Layout>),
	Flat,
	Smooth,
	NoPerspective,
	HighP,
	MediumP,
	LowP,
	Invariant,
	Precise,
	Coherent,
	Volatile,
	Restrict,
	Readonly,
	Writeonly,
}

/// An individual layout property within a layout qualifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Layout {
	pub ty: LayoutTy,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LayoutTy {
	Shared,
	Packed,
	Std140,
	Std430,
	RowMajor,
	ColumnMajor,
	Binding(Option<Expr>),
	Offset(Option<Expr>),
	Align(Option<Expr>),
	Location(Option<Expr>),
	Component(Option<Expr>),
	Index(Option<Expr>),
	Points,
	Lines,
	Isolines,
	Triangles,
	Quads,
	EqualSpacing,
	FractionalEvenSpacing,
	FractionalOddSpacing,
	Clockwise,
	CounterClockwise,
	PointMode,
	LineAdjacency,
	TriangleAdjacency,
	Invocations(Option<Expr>),
	OriginUpperLeft,
	PixelCenterInteger,
	EarlyFragmentTests,
	LocalSizeX(Option<Expr>),
	LocalSizeY(Option<Expr>),
	LocalSizeZ(Option<Expr>),
	XfbBuffer(Option<Expr>),
	XfbStride(Option<Expr>),
	XfbOffset(Option<Expr>),
	Vertices(Option<Expr>),
	LineStrip,
	TriangleStrip,
	MaxVertices(Option<Expr>),
	Stream(Option<Expr>),
	DepthAny,
	DepthGreater,
	DepthLess,
	DepthUnchanged,
}

/// A GLSL profile.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProfileTy {
	Core,
	Compatability,
	Es,
}

/// The behaviour of a GLSL extension.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BehaviourTy {
	Require,
	Enable,
	Warn,
	Disable,
}

/// A macro definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Macro {
	/// An object-like macro.
	Object { ident: Ident },
	/// A function-like macro.
	Function { ident: Ident, params: Vec<Ident> },
}
*/

/*
 serde serializes rust types in a form like

{
    [typename]: type data (e.g. fields)
}

we would like to convert it to the unist format (https://github.com/syntax-tree/unist) 
```
## Nodes

Syntactic units in unist syntax trees are called nodes, and implement the
**[Node][dfn-node]** interface.

### `Node`

```idl
interface Node {
  type: string
  data: Data?
  position: Position?
}
```

The `type` field is a non-empty string representing the variant of a node.
This field can be used to determine the *[type][term-type]* a node implements.

The `data` field represents information from the ecosystem.
The value of the `data` field implements the **[Data][dfn-data]** interface.

The `position` field represents the location of a node in a source document.
The value of the `position` field implements the **[Position][dfn-position]**
interface.
The `position` field must not be present if a node is
*[generated][term-generated]*.

Specifications implementing unist are encouraged to define more fields.
Ecosystems can define fields on **[Data][dfn-data]**.

Any value in unist **must** be expressible in JSON values: `string`, `number`,
`object`, `array`, `true`, `false`, or `null`.
This means that the syntax tree should be able to be converted to and from JSON
and produce the same tree.
For example, in JavaScript, a tree can be passed through
`JSON.parse(JSON.stringify(tree))` and result in the same tree.

#### `Position`

```idl
interface Position {
  start: Point
  end: Point
}
```

**Position** represents the location of a node in a source *[file][term-file]*.

The `start` field of **Position** represents the place of the first character of
the parsed source region.
The `end` field of **Position** represents the place of the first character
after the parsed source region, whether it exists or not.
The value of the `start` and `end` fields implement the **[Point][dfn-point]**
interface.

If the syntactic unit represented by a node is not present in the source
*[file][term-file]* at the time of parsing, the node is said to be
*[generated][term-generated]* and it must not have positional information.

For example, if the following value was represented as unist:

```markdown
alpha
bravo
```

…the first word (`alpha`) would start at line `1`, column `1`, offset `0`, and
end at line `1`, column `6`, offset `5`.
The line feed would start at line `1`, column `6`, offset `5`, and end at line
`2`, column `1`, offset `6`.
The last word (`bravo`) would start at line `2`, column `1`, offset `6`, and end
at line `2`, column `6`, offset `11`.

#### `Point`

```idl
interface Point {
  line: number >= 1
  column: number >= 1
  offset: number >= 0?
}
```

**Point** represents one place in a source *[file][term-file]*.

The `line` field (1-indexed integer) represents a line in a source file.
The `column` field (1-indexed integer) represents a column in a source file.
The `offset` field (0-indexed integer) represents a character in a source file.

The term character means a (UTF-16) code unit which is defined in the
[Web IDL][webidl] specification.

#### `Data`

```idl
interface Data { }
```

**Data** represents information associated by the ecosystem with the node.

This space is guaranteed to never be specified by unist or specifications
implementing unist.

### `Parent`

```idl
interface Parent <: Node {
  children: [Node]
}
```

Nodes containing other nodes (said to be *[children][term-child]*) extend the
abstract interface **Parent** (**[Node][dfn-node]**).

The `children` field is a list representing the children of a node.

### `Literal`

```idl
interface Literal <: Node {
  value: any
}
```

Nodes containing a value extend the abstract interface **Literal**
(**[Node][dfn-node]**).

The `value` field can contain any value.

```
*/

type Position = {
  start: Point;
  end: Point;
};

type NodeWithPosition = {};

type Point = {
  line: number;
  column: number;
  offset: number;
};

type VersionDirective = {
  type: 'VersionDirective';
  version: number;
  profile: string;
};

type Empty = {
  type: 'Empty';
};

type FnDef = {
  type: 'FnDef';
  return_type: Type;
  ident: Ident;
  params: Param[];
  body: Scope;
};

enum TypeType {
  /// A type which has only a single value.
  // Single(Primitive),
  // /// An array type which contains zero or more values.
  // Array(Primitive, ArrSize),
  // /// A 2D array type which contains zero or more values.
  // ///
  // /// - `1` - Size of the outer array.
  // /// - `2` - Size of each inner array.
  // Array2D(Primitive, ArrSize, ArrSize),
  // /// An n-dimensional array type which contains zero or more values.
  // ///
  // /// - `1` - Vec containing the sizes of arrays, starting with the outer-most array.
  // ArrayND(Primitive, Vec<ArrSize>),

  Single,
  Array,
  Array2D,
  ArrayND,
}

enum QualifierType {
  Const,
  In,
  Out,
  InOut,
  Attribute,
  Uniform,
  Varying,
  Buffer,
  Shared,
  Centroid,
  Sample,
  Patch,
  Layout,
  Flat,
  Smooth,
  NoPerspective,
  HighP,
  MediumP,
  LowP,
  Invariant,
  Precise,
  Coherent,
  Volatile,
  Restrict,
  Readonly,
  Writeonly,
}

type Type = {
  type: TypeType;
  qualifiers: QualifierType[];
};

type Omittable<T> = T | void;

// pub enum Lit {
// 	Bool(bool),
// 	Int(i64),
// 	UInt(u64),
// 	Float(f32),
// 	Double(f64),
// 	/// A [`Token::Num`] which could not be parsed into a valid number.
// 	///
// 	/// This could be because:
// 	/// - The number is too large to be represented by the relevant GLSL type, e.g.
// 	///   `10000000000000000000000000000000000000`.
// 	/// - The number has an illegal suffix, e.g. `150abc`.
// 	/// - The number has no digits, e.g. `0xU`.
// 	InvalidNum,
// }

type Lit =
  | {
      type: 'Bool';
      data: {value: boolean};
    }
  | {
      type: 'Int';
      data: {value: number};
    }
  | {
      type: 'UInt';
      data: {value: number};
    }
  | {
      type: 'Float';
      data: {value: number};
    }
  | {
      type: 'Double';
      data: {value: number};
    }
  | {
      type: 'InvalidNum';
      data: {};
    };

// pub struct Ident {
// 	pub name: String,
// 	pub span: Span,
// }

type Ident = {
  type: 'Ident';
  name: string;
};

// pub enum ExprTy {
// 	/// A literal constant.
// 	Lit(Lit),
// 	/// An identifier.
// 	Ident(Ident),
// 	/// A prefix operator.
// 	Prefix { op: PreOp, expr: Option<Box<Expr>> },
// 	/// A postfix operator.
// 	Postfix { expr: Box<Expr>, op: PostOp },
// 	/// A binary expression.
// 	Binary {
// 		left: Box<Expr>,
// 		op: BinOp,
// 		right: Option<Box<Expr>>,
// 	},
// 	/// A ternary expression.
// 	Ternary {
// 		cond: Box<Expr>,
// 		true_: Option<Box<Expr>>,
// 		false_: Option<Box<Expr>>,
// 	},
// 	/// A set of parenthesis.
// 	Parens { expr: Option<Box<Expr>> },
// 	/// Object access.
// 	ObjAccess {
// 		obj: Box<Expr>,
// 		leaf: Option<Box<Expr>>,
// 	},
// 	/// An index operator.
// 	Index {
// 		item: Box<Expr>,
// 		i: Option<Box<Expr>>,
// 	},
// 	/// A function call.
// 	FnCall { ident: Ident, args: Vec<Expr> },
// 	/// An initializer list.
// 	InitList { args: Vec<Expr> },
// 	/// An array constructor.
// 	ArrConstructor {
// 		/// Contains the first part of an array constructor, e.g. `int[3]`.
// 		arr: Box<Expr>,
// 		args: Vec<Expr>,
// 	},
// 	/// A general list expression, e.g. `a, b`.
// 	List { items: Vec<Expr> },
// 	/// A separator.
// 	///
// 	/// This node only exists during the execution of the expression parser. It will not occur in the final AST.
// 	Separator,
// }
type Expr =
  | {
      type: 'Lit';
      data: Lit;
    }
  | {
      type: 'Ident';
      data: Ident;
    }
  | {
      type: 'Prefix';
      data: {
        op: string;
        expr: Omittable<Expr>;
      };
    }
  | {
      type: 'Postfix';
      data: {
        expr: Expr;
        op: string;
      };
    }
  | {
      type: 'Binary';
      data: {
        left: Expr;
        op: string;
        right: Omittable<Expr>;
      };
    }
  | {
      type: 'Ternary';
      data: {
        cond: Expr;
        true_: Omittable<Expr>;
        false_: Omittable<Expr>;
      };
    }
  | {
      type: 'Parens';
      data: {
        expr: Omittable<Expr>;
      };
    }
  | {
      type: 'ObjAccess';
      data: {
        obj: Expr;
        leaf: Omittable<Expr>;
      };
    }
  | {
      type: 'Index';
      data: {
        item: Expr;
        i: Omittable<Expr>;
      };
    }
  | {
      type: 'FnCall';
      data: {
        ident: Ident;
        args: Expr[];
      };
    }
  | {
      type: 'InitList';
      data: {
        args: Expr[];
      };
    }
  | {
      type: 'ArrConstructor';
      data: {
        arr: Expr;
        args: Expr[];
      };
    }
  | {
      type: 'List';
      data: {
        items: Expr[];
      };
    }
  | {
      type: 'Separator';
      data: {};
    };

type Span = {
  start: number;
  end: number;
};
type Block = {
  children: Node[];
};

// pub struct Scope {
//     pub contents: Vec<Node>,
//     pub span: Span,
// }

type Scope = {
  contents: Node[];
};

// pub struct Param {
// 	pub type_: Type,
// 	pub ident: Omittable<Ident>,
// 	pub span: Span,
// }

type Param = {
  type_: Type;
  ident: Omittable<Ident>;
};

type VarDef = {
  type: 'VarDef';
  data: {
    type_: Type;
    ident: Ident;
  };
};

type VarDefs = {
  type: 'VarDefs';
  data: {
    vars: [Type, Ident][];
  };
};

type VarDefInit = {
  type: 'VarDefInit';
  data: {
    type_: Type;
    ident: Ident;
    value: Omittable<Expr>;
  };
};

type VarDefInits = {
  type: 'VarDefInits';
  data: {
    vars: [Type, Ident][];
    value: Omittable<Expr>;
  };
};

type InterfaceDef = {
  type: 'InterfaceDef';
  data: {
    qualifiers: QualifierType[];
    ident: Ident;
    body: Scope;
    instance: Omittable<Expr>;
  };
};

type Qualifiers = {
  type: 'Qualifiers';
  data: {
    qualifiers: QualifierType[];
  };
};

type FnDecl = {
  type: 'FnDecl';
  data: {
    return_type: Type;
    ident: Ident;
    params: Param[];
  };
};

type SubroutineTypeDecl = {
  type: 'SubroutineTypeDecl';
  data: {
    return_type: Type;
    ident: Ident;
    params: Param[];
  };
};

type SubroutineFnDef = {
  type: 'SubroutineFnDef';
  data: {
    associations: Ident[];
    return_type: Type;
    ident: Ident;
    params: Param[];
    body: Omittable<Scope>;
  };
};

type SubroutineUniformDef = {
  type: 'SubroutineUniformDef';
  data: {
    type_: Type;
    ident: Ident;
  };
};

type StructDecl = {
  type: 'StructDecl';
  data: {
    qualifiers: QualifierType[];
    ident: Ident;
  };
};

type StructDef = {
  type: 'StructDef';
  data: {
    qualifiers: QualifierType[];
    ident: Ident;
    body: Scope;
    instance: Omittable<Ident>;
  };
};

type Spanned<T> = {
  value: T;
  span: Span;
};

type IfCondition = {
  type: 'If' | 'ElseIf' | 'Else';
  value: Omittable<Expr>;
};

type IfBranch = {
  condition: Spanned<IfCondition>;
  body: Omittable<Scope>;
};

type If = {
  type: 'If';
  data: {
    branches: IfBranch[];
  };
};

type SwitchCase = {
  expr: Omittable<Expr>;
  body: Omittable<Scope>;
};

type Switch = {
  cond: Omittable<Expr>;
  cases: SwitchCase[];
};

type For = {
  init: Omittable<Node>;
  cond: Omittable<Node>;
  inc: Omittable<Node>;
  body: Omittable<Scope>;
};

type While = {
  cond: Omittable<Expr>;
  body: Scope;
};

type DoWhile = {
  body: Scope;
  cond: Omittable<Expr>;
};

type Break = {};

type Continue = {};

type Discard = {};

type Return = {
  value: Omittable<Expr>;
};

type EmptyDirective = {};

enum BehaviourType {
  Require,
  Enable,
  Warn,
  Disable,
}

type ExtensionDirective = {
  name: Omittable<Spanned<string>>;
  behaviour: Omittable<Spanned<BehaviourType>>;
};

type LineDirective = {
  type: 'LineDirective';
  data: {
    line: Omittable<Spanned<number>>;
    src_str_num: Omittable<Spanned<number>>;
  };
};

type Macro =
  | {
      type: 'Object';
      data: {
        ident: Ident;
      };
    }
  | {
      type: 'Function';
      data: {
        ident: Ident;
        params: Ident[];
      };
    };

type Token = any;

type DefineDirective = {
  macro_: Macro;
  replacement_tokens: Spanned<Token>[];
};

type UndefDirective = {
  name: Omittable<Ident>;
};

type ErrorDirective = {
  message: Omittable<Spanned<string>>;
};

type PragmaDirective = {
  options: Omittable<Spanned<string>>;
};

type Node =
  | VersionDirective
  | FnDef
  | Empty
  | Expr
  | Block
  | VarDef
  | VarDefs
  | VarDefInit
  | VarDefInits
  | InterfaceDef
  | Qualifiers
  | FnDecl
  | SubroutineTypeDecl
  | SubroutineFnDef
  | SubroutineUniformDef
  | StructDecl
  | StructDef
  | If
  | Switch
  | For
  | While
  | DoWhile
  | Break
  | Continue
  | Discard
  | Return
  | EmptyDirective
  | ExtensionDirective
  | LineDirective
  | DefineDirective
  | UndefDirective
  | ErrorDirective
  | PragmaDirective;

function serdeTypeToParts(serdeType: unknown): [string, unknown] {
  if (serdeType == null || typeof serdeType !== 'object') {
    throw new Error(`Expected object, got ${serdeType}`);
  }
  const entries = Object.entries(serdeType);

  if (entries.length !== 1) {
    throw new Error(`Expected exactly 1 entry, got ${entries.length}`);
  }
  const [typeName, data] = entries[0];
  if (typeof typeName !== 'string') {
    throw new Error(`Expected string, got ${typeName}`);
  }
  return [typeName, data];
}

type UnistNode = {
  type: string;
  data: any;
  position?: Position;
};

function rewriteAstNode(astNode: unknown): UnistNode {
  const {span, ty} = astNode as any;
  const [typeName, dataUnknown] = serdeTypeToParts(ty);

  const data = dataUnknown as any;

  // visit children

  let newData = Object.entries(data).map(([key, value]) => {
    if (Array.isArray(value)) {
      return [key, value.map(rewriteAstNode)];
    } else {
      return [key, rewriteAstNode(value)];
    }
  });

  const node = {
    type: typeName,
    data: newData,
  } as UnistNode;

  return node;
}

export function rewriteAst(ast: unknown[]) {
  const nodes = ast.map(rewriteAstNode);
  return nodes;
}

class EvalScope {
  vars = new Map<string, any>();
  parent: EvalScope | null = null;

  constructor(parent: EvalScope | null) {
    this.parent = parent;
  }

  get(name: string): any {
    if (this.vars.has(name)) {
      return this.vars.get(name);
    }
    if (this.parent) {
      return this.parent.get(name);
    }
    return undefined;
  }

  set(name: string, value: any) {
    this.vars.set(name, value);
  }
}

export function evalAst(ast: any[]) {
  const rootScope = new EvalScope(null);
  for (const node of ast) {
    switch (node.ty) {
      case 'VarDef': {
        const {type_, ident} = node.data;
        rootScope.set(ident.name, undefined);
        break;
      }
    }
  }
}
