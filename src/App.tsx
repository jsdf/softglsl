import initFluffyInterpreter, {
  parse_str_to_ast_json,
  eval_from_str,
  init_panic_hook,
} from '../lib/fluffygl_interpreter/fluffygl_interpreter.js';

import complexFragmentShader from '../glsl_examples/complex_frag.glsl';
import WebGLRenderer from './WebGLRenderer';
import {useEffect, useState} from 'react';

const frag = `#version 300 es
precision highp float;

uniform vec4 color;
out vec4 fragColor;    

void main () {
  fragColor = vec4(gl_FragCoord.xy / 800.0, color.b, color.a);
}`;

const shaderSource = frag;

export default function App() {
  return (
    <div className="two-column">
      <WebGLRenderer shaderSource={shaderSource} />
      <FluffyGL shaderSource={shaderSource} />
    </div>
  );
}

function FluffyGL(props: {shaderSource: string}) {
  const {shaderSource} = props;
  const [ready, setReady] = useState(false);

  useEffect(() => {
    interpreterReady.then(() => {
      setReady(true);
    });
  }, []);

  useEffect(() => {
    if (ready) {
      const astJSON = parse_str_to_ast_json(shaderSource);

      let ast = JSON.parse(astJSON);
      console.log(slimAst(ast));

      let fragColor = eval_from_str(
        shaderSource.replace(
          //strip precision specifier because it's not supported by fluffy
          /precision (?:high|medium|low)p float;\n/,
          ''
        )
      );
      console.log({fragColor});
    }
  }, [ready, shaderSource]);

  return (
    <div>
      <pre>{shaderSource}</pre>
    </div>
  );
}

const interpreterReady = initFluffyInterpreter()
  .then(() => {
    console.log('fluffy initialized');

    init_panic_hook(); // install panic hook
  })
  .catch((e) => {
    console.error(e);
  });

function slimAst(ast: any): any {
  if (Array.isArray(ast)) {
    // map over array
    return ast.map(slimAst);
  } else if (ast != null && typeof ast === 'object') {
    // map over object, excluding span
    return Object.fromEntries(
      Object.entries(ast).reduce((memo, [key, value]) => {
        if (key !== 'span') {
          memo.push([key, slimAst(value)]);
        }
        return memo;
      }, [] as [string, any][])
    );
  } else {
    // return primitive
    return ast;
  }
}
