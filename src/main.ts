import './style.css';
import typescriptLogo from './typescript.svg';
import viteLogo from '/vite.svg';
import {setupCounter} from './counter.ts';

import iniGlast, {
  parse_str_to_ast_json,
  init_panic_hook,
} from '../lib/glast/glast.js';

import {rewriteAst} from './glslInterpreter.ts';

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <a href="https://vite.dev" target="_blank">
      <img src="${viteLogo}" class="logo" alt="Vite logo" />
    </a>
    <a href="https://www.typescriptlang.org/" target="_blank">
      <img src="${typescriptLogo}" class="logo vanilla" alt="TypeScript logo" />
    </a>
    <h1>Vite + TypeScript</h1>
    <div class="card">
      <button id="counter" type="button"></button>
    </div>
    <p class="read-the-docs">
      Click on the Vite and TypeScript logos to learn more
    </p>
  </div>
`;

setupCounter(document.querySelector<HTMLButtonElement>('#counter')!);

iniGlast()
  .then(() => {
    console.log('glast initialized');

    init_panic_hook(); // install panic hook

    const astJSON = parse_str_to_ast_json(`
#version 300 es
void main() {}
`);
    console.log(astJSON);

    const ast = JSON.parse(astJSON);

    console.log(rewriteAst(ast));
  })
  .catch((e) => {
    console.error(e);
  });
