import { serve } from 'bun';
import path from 'path';

import { API_BASE_URL, handleAPIProxy } from './api.ts';

// --- Configuration ---
const PORT = process.env['PORT'] || 3000;
const BUN_ENV = process.env['BUN_ENV'] || 'dev';

// --- Path Definitions ---
const scriptDir = import.meta.dir;
const buildDir = path.resolve(scriptDir, 'dist');
const indexHtmlPath = path.join(buildDir, 'index.html');

// --- Server Logic ---
const server = serve({
  port: PORT,
  async fetch(req) {
    const url = new URL(req.url);
    const requestedPath = url.pathname;
    let filePath = '';
    let fileToServe: ReturnType<typeof Bun.file> | null = null;

    console.log(`[${BUN_ENV}] Request: ${requestedPath}`);

    const proxyResponse = await handleAPIProxy(req, API_BASE_URL);
    if (proxyResponse) {
      return proxyResponse;
    }

    try {
      // Construct potential file path within buildDir
      filePath = path.join(buildDir, requestedPath);

      // Security check: Prevent path traversal
      if (!filePath.startsWith(buildDir)) {
        console.warn(`[${BUN_ENV}] Forbidden access attempt: ${requestedPath}`);
        return new Response("Forbidden", { status: 403 });
      }

      // Check if the exact file exists and is not a directory
      const potentialFile = Bun.file(filePath);
      if (await potentialFile.exists()) {
        const stats = await Bun.file(filePath).stat();
        if (!stats.isDirectory()) {
          console.log(`[${BUN_ENV}] Serving file: ${filePath}`);
          fileToServe = potentialFile;
        }
      }

      if (!fileToServe) {
        console.log(`[${BUN_ENV}] Path not found or directory: ${requestedPath}. Serving SPA fallback: ${indexHtmlPath}`);
        fileToServe = Bun.file(indexHtmlPath);
      }

      // --- Serve the determined file ---
      if (fileToServe && await fileToServe.exists()) {
        const contentType = fileToServe.type; // Bun often infers this
        return new Response(fileToServe, { headers: { 'Content-Type': contentType } });
      } else {
        console.error(`[${BUN_ENV}] Final fallback: File not found for ${requestedPath}. Looked for: ${filePath}, Fallback: ${indexHtmlPath}`);
        return new Response("Not Found", { status: 404 });
      }

    } catch (error) {
      console.error(`[${BUN_ENV}] Error processing request ${requestedPath}:`, error);
      return new Response("Internal Server Error", { status: 500 });
    }
  },
  error(error: Error) {
    console.error(`[${BUN_ENV}] Server error:`, error);
    return new Response(`Uh oh!!\n${error.toString()}`, { status: 500 });
  },
});

console.log(`[${BUN_ENV}] === === === Running melosz frontend === === ===`);
console.log(`[${BUN_ENV}] Listening: http://${server.hostname}:${server.port}`);
