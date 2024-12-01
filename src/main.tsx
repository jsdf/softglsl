import './style.css';
import * as React from 'react';
import * as ReactDOM from 'react-dom/client';

import App from './App';

const root = document.querySelector<HTMLDivElement>('#app')!;

export function mountApp(rootElement: HTMLDivElement) {
  const root = ReactDOM.createRoot(rootElement);
  root.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>
  );
}
mountApp(root);
