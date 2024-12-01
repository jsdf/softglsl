/*
example ast after rewriteAst:

```
[
    {
        "node": {
            "type": "VersionDirective",
            "val": {
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
        }
    },
    {
        "node": {
            "type": "VarDef",
            "val": {
                "type_": {
                    "node": {
                        "type": "Single",
                        "val": {
                            "Vector": [
                                "Float",
                                4
                            ]
                        }
                    },
                    "qualifiers": [
                        {
                            "ty": "In",
                            "span": {
                                "start": 18,
                                "end": 20
                            }
                        }
                    ]
                },
                "ident": {
                    "name": "vColor"
                }
            }
        }
    },
    {
        "node": {
            "type": "VarDef",
            "val": {
                "type_": {
                    "node": {
                        "type": "Single",
                        "val": {
                            "Vector": [
                                "Float",
                                4
                            ]
                        }
                    },
                    "qualifiers": [
                        {
                            "ty": "Out",
                            "span": {
                                "start": 35,
                                "end": 38
                            }
                        }
                    ]
                },
                "ident": {
                    "name": "FragColor"
                }
            }
        }
    },
    {
        "node": {
            "type": "FnDef",
            "val": {
                "return_type": {
                    "node": {
                        "type": "Single",
                        "val": {
                            "Scalar": "Void"
                        }
                    },
                    "qualifiers": []
                },
                "ident": {
                    "name": "main"
                },
                "params": [],
                "body": {
                    "contents": [
                        {
                            "node": {
                                "type": "Expr",
                                "val": {
                                    "node": {
                                        "type": "Binary",
                                        "val": {
                                            "left": {
                                                "node": {
                                                    "type": "Ident",
                                                    "val": {
                                                        "name": "FragColor"
                                                    }
                                                }
                                            },
                                            "op": {
                                                "node": {
                                                    "type": "0",
                                                    "val": "E"
                                                }
                                            },
                                            "right": {
                                                "node": {
                                                    "type": "Ident",
                                                    "val": {
                                                        "name": "vColor"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    ]
                }
            }
        }
    }
]
```
*/

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

function rewriteAst(ast: any): any {
  if (Array.isArray(ast)) {
    // map over array
    return ast.map(rewriteAst);
  } else if (ast != null && typeof ast === 'object') {
    if (ast.ty && typeof ast.ty === 'object') {
      const {ty, span, ...rest} = ast;
      // convert ty property to node object
      const typeName = Object.keys(ast.ty)[0];
      if (rest.qualifiers) {
        rest.qualifiers = rest.qualifiers.map((qual: {ty: string}) => {
          return qual.ty;
        });
      }
      return {
        node: {
          type: typeName,
          val: rewriteAst(ast.ty[typeName]),
        },
        ...rest,
      };
    } else {
      // map over object, excluding span
      return Object.fromEntries(
        Object.entries(ast).reduce((memo, [key, value]) => {
          if (key !== 'span') {
            memo.push([key, rewriteAst(value)]);
          }
          return memo;
        }, [] as [string, any][])
      );
    }
  } else {
    // return primitive
    return ast;
  }
}

// function evalExpr(node: any, scope: EvalScope) {
//   switch (node.type) {
//     case 'Ident': {
//       return scope.get(node.name);
//     }
//     case 'Binary': {
//       const left = evalExpr(node.left, scope);
//       const right = evalExpr(node.right, scope);
//       switch (node.op) {
//         case '+':
//           return left + right;
//         case '-':
//           return left - right;
//         case '*':
//           return left * right;
//         case '/':
//           return left / right;
//       }
//     }
//   }
// }

function evalStatement(node: any, scope: EvalScope) {
  switch (node.type) {
    case 'VersionDirective': {
      // ignore for now
      break;
    }
    case 'FnDef': {
      // ignore for now
      break;
    }
    case 'VarDef': {
      scope.set(node.ident.name, null);
      break;
    }
  }
}

export function evalAst(rawAst: any[]) {
  const ast = rewriteAst(rawAst);

  console.log(ast);
  const rootScope = new EvalScope(null);
  for (const node of ast) {
    evalStatement(node, rootScope);
  }
}
