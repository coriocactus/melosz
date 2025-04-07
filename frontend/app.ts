// import { initializeApp } from './App';

console.log("Melosz Frontend Initializing...");

const rootElement = document.getElementById('app');

if (rootElement) {
  rootElement.innerHTML = 'Initialized!!!';
  rootElement.className = 'font-bold';
  // initializeApp(rootElement);
  console.log("Application initialized.");
} else {
  console.error("Fatal Error: Root element #app-root not found.");
  document.body.innerHTML = '<p style="color: red; text-align: center; margin-top: 50px;">Application failed to load: Root element missing.</p>';
}
