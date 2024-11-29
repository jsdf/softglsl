{
  // Helper functions to build AST nodes
  function buildBinaryExpression(operator, left, right) {
    return {
      type: "BinaryExpression",
      operator: operator,
      left: left,
      right: right
    };
  }

  function buildLogicalExpression(operator, left, right) {
    return {
      type: "LogicalExpression",
      operator: operator,
      left: left,
      right: right
    };
  }

  function reduceExpressions(initial, rest, builder) {
    return rest.reduce((acc, curr) => {
      return builder(curr[1], acc, curr[3]);
    }, initial);
  }
}

/////////////////////
// Start Rule
/////////////////////

start
  = _ translation_unit _ EOF {
      return {
        type: "Program",
        body: $2
      };
    }

/////////////////////
// Translation Unit
/////////////////////

translation_unit
  = external_declaration+

external_declaration
  = function_definition
  / declaration

/////////////////////
// Function Definition
/////////////////////

function_definition
  = function_prototype _ compound_statement_no_new_scope {
      return {
        type: "FunctionDeclaration",
        returnType: $1.returnType,
        name: $1.name,
        parameters: $1.parameters,
        body: $3
      };
    }

function_prototype
  = function_header_with_parameters _ RIGHT_PAREN {
      return $1;
    }
  / function_header_no_parameters _ RIGHT_PAREN {
      return $1;
    }

function_header_with_parameters
  = function_header _ parameter_declaration (_ COMMA _ parameter_declaration)* {
      return {
        type: "FunctionPrototype",
        returnType: $1.returnType,
        name: $1.name,
        parameters: [$3].concat($5.map(e => e[4]))
      };
    }

function_header_no_parameters
  = function_header {
      return {
        type: "FunctionPrototype",
        returnType: $1.returnType,
        name: $1.name,
        parameters: []
      };
    }

function_header
  = fully_specified_type _ IDENTIFIER _ LEFT_PAREN {
      return {
        returnType: $1,
        name: $3
      };
    }

parameter_declaration
  = parameter_qualifier? fully_specified_type _ IDENTIFIER {
      return {
        type: "Parameter",
        qualifier: $1 || null,
        dataType: $2,
        name: $4
      };
    }

parameter_qualifier
  = INOUT { return "inout"; }
  / IN { return "in"; }
  / OUT { return "out"; }

fully_specified_type
  = type_specifier

/////////////////////
// Type Specifiers
/////////////////////

type_specifier
  = VOID { return "void"; }
  / BOOL { return "bool"; }
  / INT { return "int"; }
  / FLOAT { return "float"; }
  / VEC2 { return "vec2"; }
  / VEC3 { return "vec3"; }
  / VEC4 { return "vec4"; }
  / MAT2 { return "mat2"; }
  / MAT3 { return "mat3"; }
  / MAT4 { return "mat4"; }
  / TYPE_NAME { return $1; }

TYPE_NAME
  = IDENTIFIER

/////////////////////
// Compound Statement
/////////////////////

compound_statement_no_new_scope
  = LEFT_BRACE _ statement_list? _ RIGHT_BRACE {
      return {
        type: "BlockStatement",
        body: $3 || []
      };
    }

statement_list
  = statement+

statement
  = declaration_statement
  / expression_statement
  / compound_statement_no_new_scope
  / selection_statement
  / iteration_statement
  / jump_statement

/////////////////////
// Declarations
/////////////////////

declaration_statement
  = declaration

declaration
  = init_declarator_list _ SEMICOLON {
      return {
        type: "VariableDeclaration",
        declarations: $1
      };
    }

init_declarator_list
  = single_declaration ( _ COMMA _ single_declaration )* {
      return [$1].concat($2.map(e => e[3]));
    }

single_declaration
  = fully_specified_type _ IDENTIFIER _ EQUAL _ initializer {
      return {
        type: "VariableDeclarator",
        dataType: $1,
        name: $3,
        initializer: $5
      };
    }
  / fully_specified_type _ IDENTIFIER {
      return {
        type: "VariableDeclarator",
        dataType: $1,
        name: $3,
        initializer: null
      };
    }

initializer
  = assignment_expression

/////////////////////
// Statements
/////////////////////

expression_statement
  = expression? _ SEMICOLON {
      return {
        type: "ExpressionStatement",
        expression: $1 || null
      };
    }

selection_statement
  = IF _ LEFT_PAREN _ expression _ RIGHT_PAREN _ statement ( _ ELSE _ statement )? {
      return {
        type: "IfStatement",
        test: $5,
        consequent: $8,
        alternate: $10 ? $10[3] : null
      };
    }

iteration_statement
  = WHILE _ LEFT_PAREN _ expression _ RIGHT_PAREN _ statement {
      return {
        type: "WhileStatement",
        test: $5,
        body: $8
      };
    }
  / FOR _ LEFT_PAREN _ for_init_statement _ for_condition? _ SEMICOLON _ for_iteration? _ RIGHT_PAREN _ statement {
      return {
        type: "ForStatement",
        init: $5,
        test: $6 || null,
        update: $8 || null,
        body: $11
      };
    }

for_init_statement
  = declaration_statement
  / expression_statement

for_condition
  = expression

for_iteration
  = expression

jump_statement
  = CONTINUE _ SEMICOLON {
      return {
        type: "ContinueStatement"
      };
    }
  / BREAK _ SEMICOLON {
      return {
        type: "BreakStatement"
      };
    }
  / RETURN _ expression? _ SEMICOLON {
      return {
        type: "ReturnStatement",
        argument: $3 || null
      };
    }
  / DISCARD _ SEMICOLON {
      return {
        type: "DiscardStatement"
      };
    }

/////////////////////
// Expressions
/////////////////////

expression
  = assignment_expression ( _ COMMA _ assignment_expression )* {
      if ($2.length) {
        return {
          type: "SequenceExpression",
          expressions: [$1].concat($2.map(e => e[3]))
        };
      } else {
        return $1;
      }
    }

assignment_expression
  = conditional_expression
  / unary_expression _ assignment_operator _ assignment_expression {
      return {
        type: "AssignmentExpression",
        operator: $3,
        left: $1,
        right: $5
      };
    }

conditional_expression
  = logical_or_expression ( _ QUESTION _ expression _ COLON _ assignment_expression )? {
      if ($2) {
        return {
          type: "ConditionalExpression",
          test: $1,
          consequent: $5,
          alternate: $9
        };
      } else {
        return $1;
      }
    }

logical_or_expression
  = logical_xor_expression ( _ OR_OP _ logical_xor_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildLogicalExpression) : $1;
    }

logical_xor_expression
  = logical_and_expression ( _ XOR_OP _ logical_and_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildLogicalExpression) : $1;
    }

logical_and_expression
  = inclusive_or_expression ( _ AND_OP _ inclusive_or_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildLogicalExpression) : $1;
    }

inclusive_or_expression
  = exclusive_or_expression ( _ VERTICAL_BAR _ exclusive_or_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

exclusive_or_expression
  = and_expression ( _ CARET _ and_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

and_expression
  = equality_expression ( _ AMPERSAND _ equality_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

equality_expression
  = relational_expression ( _ ( EQ_OP / NE_OP ) _ relational_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

relational_expression
  = shift_expression ( _ ( LEFT_ANGLE / RIGHT_ANGLE / LE_OP / GE_OP ) _ shift_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

shift_expression
  = additive_expression ( _ ( LEFT_OP / RIGHT_OP ) _ additive_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

additive_expression
  = multiplicative_expression ( _ ( PLUS / DASH ) _ multiplicative_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

multiplicative_expression
  = unary_expression ( _ ( STAR / SLASH / PERCENT ) _ unary_expression )* {
      return $2.length ? reduceExpressions($1, $2, buildBinaryExpression) : $1;
    }

unary_expression
  = ( INC_OP / DEC_OP / unary_operator ) _ unary_expression {
      return {
        type: "UnaryExpression",
        operator: $1,
        argument: $3,
        prefix: true
      };
    }
  / postfix_expression

postfix_expression
  = primary_expression postfix_part* {
      return $2.reduce((acc, curr) => {
        if (curr.type === "UpdateExpression") {
          return {
            type: "UpdateExpression",
            operator: curr.operator,
            argument: acc,
            prefix: false
          };
        } else if (curr.type === "MemberExpression") {
          return {
            type: "MemberExpression",
            object: acc,
            property: curr.property,
            computed: curr.computed
          };
        } else if (curr.type === "CallExpression") {
          return {
            type: "CallExpression",
            callee: acc,
            arguments: curr.arguments
          };
        }
        return acc;
      }, $1);
    }

postfix_part
  = _ LEFT_BRACKET _ expression _ RIGHT_BRACKET {
      return {
        type: "MemberExpression",
        property: $5,
        computed: true
      };
    }
  / _ DOT _ FIELD_SELECTION {
      return {
        type: "MemberExpression",
        property: {
          type: "Identifier",
          name: $4
        },
        computed: false
      };
    }
  / function_call_arguments {
      return {
        type: "CallExpression",
        arguments: $1
      };
    }
  / _ ( INC_OP / DEC_OP ) {
      return {
        type: "UpdateExpression",
        operator: $2,
        prefix: false
      };
    }

primary_expression
  = variable_identifier
  / constant
  / _ LEFT_PAREN _ expression _ RIGHT_PAREN {
      return $4;
    }

variable_identifier
  = IDENTIFIER {
      return {
        type: "Identifier",
        name: $1
      };
    }

constant
  = INTCONSTANT
  / UINTCONSTANT
  / FLOATCONSTANT
  / BOOLCONSTANT

function_call_arguments
  = _ LEFT_PAREN _ argument_expression_list? _ RIGHT_PAREN {
      return $4 || [];
    }

argument_expression_list
  = assignment_expression ( _ COMMA _ assignment_expression )* {
      return [$1].concat($2.map(e => e[3]));
    }

/////////////////////
// Helper Rules
/////////////////////

assignment_operator
  = EQUAL
  / MUL_ASSIGN
  / DIV_ASSIGN
  / MOD_ASSIGN
  / ADD_ASSIGN
  / SUB_ASSIGN
  / LEFT_ASSIGN
  / RIGHT_ASSIGN
  / AND_ASSIGN
  / XOR_ASSIGN
  / OR_ASSIGN

unary_operator
  = PLUS
  / DASH
  / BANG
  / TILDE

/////////////////////
// Tokens
/////////////////////

IDENTIFIER
  = !Keyword [a-zA-Z_] [a-zA-Z0-9_]* {
      return text();
    }

INTCONSTANT
  = [0-9]+ {
      return {
        type: "Literal",
        value: parseInt(text(), 10),
        raw: text()
      };
    }

UINTCONSTANT
  = [0-9]+ [uU] {
      return {
        type: "Literal",
        value: parseInt(text().slice(0, -1), 10),
        raw: text()
      };
    }

FLOATCONSTANT
  = [0-9]+ '.' [0-9]* ExponentPart? {
      return {
        type: "Literal",
        value: parseFloat(text()),
        raw: text()
      };
    }
  / '.' [0-9]+ ExponentPart? {
      return {
        type: "Literal",
        value: parseFloat(text()),
        raw: text()
      };
    }
  / [0-9]+ ExponentPart {
      return {
        type: "Literal",
        value: parseFloat(text()),
        raw: text()
      };
    }

ExponentPart
  = [eE] [+-]? [0-9]+

BOOLCONSTANT
  = 'true' {
      return {
        type: "Literal",
        value: true,
        raw: text()
      };
    }
  / 'false' {
      return {
        type: "Literal",
        value: false,
        raw: text()
      };
    }

FIELD_SELECTION
  = IDENTIFIER

/////////////////////
// Operators and Punctuators
/////////////////////

LEFT_OP = '<<' { return text(); }
RIGHT_OP = '>>' { return text(); }
INC_OP = '++' { return text(); }
DEC_OP = '--' { return text(); }
LE_OP = '<=' { return text(); }
GE_OP = '>=' { return text(); }
EQ_OP = '==' { return text(); }
NE_OP = '!=' { return text(); }
AND_OP = '&&' { return text(); }
OR_OP = '||' { return text(); }
XOR_OP = '^^' { return text(); }
MUL_ASSIGN = '*=' { return text(); }
DIV_ASSIGN = '/=' { return text(); }
ADD_ASSIGN = '+=' { return text(); }
MOD_ASSIGN = '%=' { return text(); }
LEFT_ASSIGN = '<<=' { return text(); }
RIGHT_ASSIGN = '>>=' { return text(); }
AND_ASSIGN = '&=' { return text(); }
XOR_ASSIGN = '^=' { return text(); }
OR_ASSIGN = '|=' { return text(); }
SUB_ASSIGN = '-=' { return text(); }
EQUAL = '=' { return text(); }

LEFT_PAREN = '('
RIGHT_PAREN = ')'
LEFT_BRACKET = '['
RIGHT_BRACKET = ']'
LEFT_BRACE = '{'
RIGHT_BRACE = '}'
DOT = '.'
COMMA = ','
COLON = ':'
SEMICOLON = ';'
BANG = '!' { return text(); }
DASH = '-' { return text(); }
TILDE = '~' { return text(); }
PLUS = '+' { return text(); }
STAR = '*' { return text(); }
SLASH = '/' { return text(); }
PERCENT = '%' { return text(); }
LEFT_ANGLE = '<' { return text(); }
RIGHT_ANGLE = '>' { return text(); }
VERTICAL_BAR = '|' { return text(); }
CARET = '^' { return text(); }
AMPERSAND = '&' { return text(); }
QUESTION = '?' { return text(); }

/////////////////////
// Whitespace and Comments
/////////////////////

_ "whitespace"
  = ( [ \t\n\r] / comment )*

comment
  = '//' [^\n]* '\n'
  / '/*' ( !'*/' . )* '*/'

/////////////////////
// End of File
/////////////////////

EOF
  = !.

/////////////////////
// Keywords
/////////////////////

Keyword
  = 'const' / 'bool' / 'float' / 'int' / 'uint'
  / 'break' / 'continue' / 'do' / 'else' / 'for' / 'if' / 'discard'
  / 'return' / 'switch' / 'case' / 'default' / 'bvec2' / 'bvec3' / 'bvec4'
  / 'ivec2' / 'ivec3' / 'ivec4' / 'uvec2' / 'uvec3' / 'uvec4' / 'vec2'
  / 'vec3' / 'vec4' / 'mat2' / 'mat3' / 'mat4' / 'centroid' / 'in'
  / 'out' / 'inout' / 'uniform' / 'flat' / 'smooth' / 'layout' / 'mat2x2'
  / 'mat2x3' / 'mat2x4' / 'mat3x2' / 'mat3x3' / 'mat3x4' / 'mat4x2'
  / 'mat4x3' / 'mat4x4' / 'sampler2D' / 'sampler3D' / 'samplerCube'
  / 'sampler2DShadow' / 'samplerCubeShadow' / 'sampler2DArray'
  / 'sampler2DArrayShadow' / 'isampler2D' / 'isampler3D' / 'isamplerCube'
  / 'isampler2DArray' / 'usampler2D' / 'usampler3D' / 'usamplerCube'
  / 'usampler2DArray' / 'struct' / 'void' / 'while' / 'invariant'
  / 'highp' / 'mediump' / 'lowp' / 'precision' / 'return' / 'continue'
  / 'break' / 'discard' / 'if' / 'else' / 'for' / 'while' / 'do' / 'switch'
  / 'case' / 'default'

