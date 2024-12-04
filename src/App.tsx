import React from 'react';
import initFluffyInterpreter, {
  parse_str_to_ast_json,
  eval_from_str,
  get_iovars_from_str,
  init_panic_hook,
} from '../lib/fluffygl_interpreter/fluffygl_interpreter.js';

(window as any).eval_from_str = eval_from_str;

import WebGLRenderer from './WebGLRenderer';
import {useEffect, useState} from 'react';

const vert = `#version 300 es
precision highp float;

in vec2 position;

uniform vec4 uColor;
out vec4 vColor;

void main () {
  vColor = uColor;
  gl_Position = vec4(position, 0, 1);
}`;

const frag = `#version 300 es
precision highp float;

uniform vec2 uMousePos;
uniform vec4 uColor;
in vec4 vColor;

out vec4 fragColor;    

void main () {
  fragColor = vec4(gl_FragCoord.xy / 800.0, vColor.b, uMousePos.x / 800.0);
}`;

function hashString(str: string): number {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = (hash << 5) - hash + str.charCodeAt(i);
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}

type Value = {
  type: string;
  value: unknown;
};

type IOVar = {
  name: string;
  ty: string;
};

type ValueMap = {[key: string]: Value};

export default function App() {
  const [fragShaderSource, setFragShaderSource] = useState(frag);

  const [inputs, setInputs] = useState({
    uniforms: {
      uMousePos: {
        type: 'Vec2',
        value: [400, 100],
      },
      uColor: {
        type: 'Vec4',
        value: [1, 0, 0, 1],
      },
    } as ValueMap,
    in_vars: {
      vColor: {
        type: 'Vec4',
        value: [1, 0, 0, 1],
      },
    } as ValueMap,
  });

  const [evalError, setEvalError] = useState<Error | null>(null);

  const [iovars, setIOVars] = useState<{
    uniforms: IOVar[];
    in_vars: IOVar[];
    out_vars: IOVar[];
  }>({
    uniforms: [],
    in_vars: [],
    out_vars: [],
  });

  const [outputs, setOutputs] = useState({});

  const [ready, setReady] = useState(false);

  useEffect(() => {
    interpreterReady.then(() => {
      setReady(true);
    });
  }, []);

  useEffect(() => {
    if (!ready) {
      return;
    }
    // const astJSON = parse_str_to_ast_json(fragShaderSource);

    // let ast = JSON.parse(astJSON);
    // console.log(slimAst(ast));

    try {
      let iovars = JSON.parse(get_iovars_from_str(fragShaderSource));
      setIOVars(iovars);
    } catch (e) {
      setEvalError(e as Error);
    }

    try {
      let outputs = JSON.parse(
        eval_from_str(
          fragShaderSource.replace(
            //strip precision specifier because it's not supported by fluffy
            /precision (?:high|medium|low)p float;\n/,
            ''
          ),
          JSON.stringify({
            uniforms: toFluffyValueMap(inputs.uniforms),
            in_vars: toFluffyValueMap(inputs.in_vars),
            globals: toFluffyValueMap({
              gl_FragCoord: {
                type: 'Vec4',
                value: [300, 400, 0, 0],
              },
            }),
          })
        )
      );
      setOutputs(outputs);
    } catch (e) {
      setEvalError(e as Error);
    }

    setEvalError(null);
  }, [ready, fragShaderSource, inputs]);

  console.log('rendering', inputs);

  return (
    <div className="two-column">
      <div>
        <ErrorBoundary key={hashString(fragShaderSource)}>
          <WebGLRenderer
            fragShaderSource={fragShaderSource}
            vertShaderSource={vert}
            inputs={{
              uniforms: mapObject(inputs.uniforms, (input) => input.value),
            }}
          />
        </ErrorBoundary>
      </div>
      <div
        style={{
          width: '50vw',
        }}
      >
        <ErrorBoundary>
          <textarea
            value={fragShaderSource}
            onChange={(e) => setFragShaderSource(e.target.value)}
            style={{width: '90%', height: 200, fontFamily: 'monospace'}}
          />
          {evalError && (
            <div
              style={{
                color: 'red',
                padding: 10,
                border: '1px solid red',
                borderRadius: 5,
              }}
            >
              {evalError.message}
            </div>
          )}
          <div>
            <h2>Uniforms</h2>
            <InputsList
              vars={iovars.uniforms}
              valueMap={inputs.uniforms}
              onChange={(uniforms) => setInputs({...inputs, uniforms})}
            />

            <h2>Fragment shader inputs (varyings)</h2>
            <InputsList
              vars={iovars.in_vars}
              valueMap={inputs.in_vars}
              onChange={(in_vars) => setInputs({...inputs, in_vars})}
            />

            <h2>Outputs</h2>
            <pre>{JSON.stringify(outputs, null, 2)}</pre>

            <details>
              <summary>Parsed Input/Output Schema from shader</summary>
              <pre>{iovars && JSON.stringify(iovars, null, 2)}</pre>
            </details>
          </div>
        </ErrorBoundary>
      </div>
    </div>
  );
}

function InputField({
  value,
  onChange,
}: {
  value: Value;
  onChange: (value: Value) => void;
}) {
  let valueAsString = value.value == null ? '' : String(value.value);

  switch (value.type) {
    case 'Float':
      return (
        <input
          type="number"
          step="0.01"
          value={valueAsString}
          onChange={(e) =>
            onChange({type: 'Float', value: parseFloat(e.target.value)})
          }
        />
      );
    case 'Int':
      return (
        <input
          type="number"
          step="1"
          value={valueAsString}
          onChange={(e) =>
            onChange({type: 'Int', value: parseInt(e.target.value)})
          }
        />
      );

    case 'Bool':
      return (
        <input
          type="checkbox"
          checked={value.value as boolean}
          onChange={(e) => onChange({type: 'Bool', value: e.target.checked})}
        />
      );
    case 'Vec2':
    case 'Vec3':
    case 'Vec4':
      return <VecInputField value={value} onChange={onChange} />;
  }
}

const componentNames = ['x', 'y', 'z', 'w'];
function VecInputField({
  value,
  onChange,
}: {
  value: Value;
  onChange: (value: Value) => void;
}) {
  let size = 2;
  switch (value.type) {
    case 'Vec2':
      size = 2;
      break;
    case 'Vec3':
      size = 3;
      break;
    case 'Vec4':
      size = 4;
      break;
  }

  function handleChange(newValue: number, index: number) {
    const newValueArray = [...(value.value as number[])];
    newValueArray[index] = newValue;
    onChange({type: value.type, value: newValueArray});
  }

  return (
    <div>
      {Array.from({length: size}, (_, index) => (
        <label key={index}>
          {componentNames[index]}:
          <input
            key={index}
            type="number"
            step="0.01"
            value={(value.value as number[])[index]}
            onChange={(e) => handleChange(parseFloat(e.target.value), index)}
          />
        </label>
      ))}
      {size >= 3 && (
        <input
          type="color"
          value={
            '#' +
            (value.value as number[])
              .slice(0, 3)
              .map((v) => Math.floor(v * 255))
              .map((v) => v.toString(16).padStart(2, '0'))
              .join('')
          }
          onChange={(e) => {
            const color = e.target.value;
            const rgb = [
              parseInt(color.slice(1, 3), 16) / 255,
              parseInt(color.slice(3, 5), 16) / 255,
              parseInt(color.slice(5, 7), 16) / 255,
            ];
            if (size === 4) {
              rgb.push((value.value as number[])[3]);
            }
            debugger;
            onChange({type: value.type, value: rgb});
          }}
        />
      )}
    </div>
  );
}

function InputsList({
  vars,
  valueMap,
  onChange,
}: {
  vars: IOVar[];
  valueMap: ValueMap;
  onChange: (inputs: ValueMap) => void;
}) {
  return (
    <div>
      {vars.map((varDef) => {
        const key = varDef.name;
        const value: Value = valueMap[key] || {
          type: varDef.ty,
          value: getDefaultValueForType(varDef.ty),
        };
        const type = varDef.ty;
        return (
          <div key={key}>
            <label>
              {key} ({type}): <br />
              <InputField
                value={value}
                onChange={(newValue) =>
                  onChange({...valueMap, [key]: newValue})
                }
              />
            </label>
          </div>
        );
      })}
    </div>
  );
}

function getDefaultValueForType(type: string): unknown {
  switch (type) {
    case 'Float':
      return 0;
    case 'Int':
      return 0;
    case 'Bool':
      return false;
    case 'Vec2':
      return [0, 0];
    case 'Vec3':
      return [0, 0, 0];
    case 'Vec4':
      return [0, 0, 0, 0];
  }
}

function parseValueFromJSON(json: string, type: string): Value {
  let value = JSON.parse(json);
  // assert type matches
  switch (type) {
    case 'Float':
      if (typeof value !== 'number') {
        throw new Error('expected number');
      }
      break;
    case 'Int':
      if (typeof value !== 'number') {
        throw new Error('expected number');
      }
      value = Math.floor(value);
      break;
    case 'Bool':
      if (typeof value !== 'boolean') {
        throw new Error('expected boolean');
      }
      break;
    case 'Vec2':
      if (!Array.isArray(value) || value.length !== 2) {
        throw new Error('expected array of length 2');
      }
      break;
    case 'Vec3':
      if (!Array.isArray(value) || value.length !== 3) {
        throw new Error('expected array of length 3');
      }
      break;
    case 'Vec4':
      if (!Array.isArray(value) || value.length !== 4) {
        throw new Error('expected array of length 4');
      }
      break;
  }

  return {type, value};
}

function toFluffyValue(value: {type: string; value: unknown}): {
  [key: string]: unknown;
} {
  return {[value.type]: value.value};
}

function toFluffyValueMap(value: ValueMap): {
  [key: string]: {
    [key: string]: unknown;
  };
} {
  return Object.fromEntries(
    Object.entries(value).map(([key, value]) => {
      return [key, toFluffyValue(value)];
    })
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

class ErrorBoundary extends React.Component<
  {
    children: React.ReactNode;
  },
  {hasError: boolean; error: Error | null}
> {
  constructor(props: {children: React.ReactNode}) {
    super(props);
    this.state = {hasError: false, error: null};
  }

  componentDidUpdate(
    prevProps: Readonly<{children: React.ReactNode}>,
    prevState: Readonly<{hasError: boolean; error: Error | null}>,
    snapshot?: any
  ): void {
    if (prevProps.children !== this.props.children) {
      this.setState({hasError: false, error: null});
    }
  }

  static getDerivedStateFromError(error: Error) {
    return {hasError: true, error};
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo): void {
    console.error('ErrorBoundary caught an error', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        <div
          style={{
            color: 'red',
            padding: 10,
            border: '1px solid red',
            borderRadius: 5,
          }}
        >
          <p>{this.state.error?.message}</p>
        </div>
      );
    }

    return this.props.children;
  }
}

function mapObject<TIn, TOut>(
  obj: Record<string, TIn>,
  fn: (value: TIn) => TOut
): Record<string, TOut> {
  return Object.fromEntries(
    Object.entries(obj).map(([key, value]) => [key, fn(value)])
  );
}
