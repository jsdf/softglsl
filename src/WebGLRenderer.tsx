import {useEffect, useRef} from 'react';
import REGL from 'regl';

type Inputs = {
  uniforms: {[key: string]: any};
};

export default function WebGLRenderer(props: {
  fragShaderSource: string;
  vertShaderSource: string;
  inputs: Inputs;
}) {
  const {fragShaderSource, vertShaderSource, inputs} = props;
  const canvasRef = useRef<HTMLCanvasElement>(null);

  console.log({inputs});
  useEffect(() => {
    if (!canvasRef.current) {
      return;
    }
    console.log({inputs});
    const regl = init(canvasRef.current, {
      fragShaderSource,
      vertShaderSource,
      inputs,
    });

    return () => {
      regl.destroy();
    };
  }, [canvasRef, fragShaderSource, vertShaderSource, inputs]);

  return <canvas id="canvas" width="800" height="800" ref={canvasRef} />;
}

function init(
  canvas: HTMLCanvasElement,
  {
    fragShaderSource,
    vertShaderSource,
    inputs,
  }: {
    fragShaderSource: string;
    vertShaderSource: string;
    inputs: Inputs;
  }
) {
  const gl = canvas.getContext('webgl2');
  const regl = REGL({
    gl: gl as any,
  });

  // In regl, draw operations are specified declaratively using. Each JSON
  // command is a complete description of all state. This removes the need to
  // .bind() things like buffers or shaders. All the boilerplate of setting up
  // and tearing down state is automated.

  const drawShaderQuad = regl({
    // In a draw call, we can pass the shader source code to regl
    frag: fragShaderSource,

    vert: vertShaderSource,

    attributes: {
      // two triangles that form full screen quad
      position: [
        [-1, -1],
        [1, -1],
        [1, 1],
        [-1, -1],
        [1, 1],
        [-1, 1],
      ],
    },

    uniforms: inputs.uniforms,

    count: 6,
  });

  // draw once
  function draw() {
    regl.clear({
      color: [0, 0, 0, 1],
      depth: 1,
    });
    drawShaderQuad();
  }
  draw();

  return regl;
}
