import {useEffect, useRef} from 'react';
import REGL from 'regl';

export default function WebGLRenderer(props: {shaderSource: string}) {
  const {shaderSource} = props;
  const canvasRef = useRef<HTMLCanvasElement>(null);
  useEffect(() => {
    if (!canvasRef.current) {
      return;
    }
    const regl = init(canvasRef.current, shaderSource);

    return () => {
      regl.destroy();
    };
  }, [canvasRef, shaderSource]);

  return <canvas id="canvas" width="800" height="800" ref={canvasRef} />;
}

function init(canvas: HTMLCanvasElement, shaderSource: string) {
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
    frag: shaderSource,

    vert: `#version 300 es
    precision highp float;
    
    in vec2 position;
 
    void main () {
      gl_Position = vec4(position, 0, 1);
    }`,

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

    uniforms: {
      color: [1, 0, 0, 1],
    },

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
