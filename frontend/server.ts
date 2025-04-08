import { serve } from 'bun';
import path from 'path';

import { API_BASE_URL, handleAPIProxy } from './api';

// --- Configuration ---
const PORT = process.env['PORT'] || 3000;
const BUN_ENV = process.env['BUN_ENV'] || 'dev';
const isProduction = BUN_ENV === 'prod';

// --- Path Definitions ---
const scriptDir = import.meta.dir;
const publicDir = path.resolve(scriptDir);
const buildDir = path.resolve(scriptDir, 'dist');

// Determine the root directory and index file based on environment
const rootDir = isProduction ? buildDir : publicDir;
const indexHtmlPath = path.join(rootDir, 'index.html');

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
      if (isProduction) {
        // --- Production Logic ---

        // Construct potential file path within buildDir
        filePath = path.join(buildDir, requestedPath);

        // Security check: Prevent path traversal
        if (!filePath.startsWith(buildDir)) {
          console.warn(`[prod] Forbidden access attempt: ${requestedPath}`);
          return new Response("Forbidden", { status: 403 });
        }

        // Check if the exact file exists and is not a directory
        const potentialFile = Bun.file(filePath);
        if (await potentialFile.exists()) {
          const stats = await Bun.file(filePath).stat();
          if (!stats.isDirectory()) {
            console.log(`[prod] Serving file: ${filePath}`);
            fileToServe = potentialFile;
          }
        }

        if (!fileToServe) {
          console.log(`[prod] Path not found or directory: ${requestedPath}. Serving SPA fallback: ${indexHtmlPath}`);
          fileToServe = Bun.file(indexHtmlPath);
        }

      } else {
        // --- Development Logic ---

        // Special case: /dist/styles.css always comes from buildDir
        if (requestedPath === '/dist/styles.css') {
          filePath = path.join(buildDir, 'styles.css');
          console.log(`[dev] Serving specific built asset: ${filePath}`);
          fileToServe = Bun.file(filePath);
        }
        // Handle root or directory-like requests by serving index.html from publicDir
        else if (requestedPath === '/' || requestedPath.endsWith('/')) {
          filePath = indexHtmlPath; // indexHtmlPath points to publicDir/index.html in dev
          console.log(`[dev] Serving root/directory index: ${filePath}`);
          fileToServe = Bun.file(filePath);
        }
        // Try serving other requested files directly from publicDir (e.g., index.html, dev .ts files if needed)
        else {
          filePath = path.join(publicDir, requestedPath);
          const potentialFile = Bun.file(filePath);
          if (await potentialFile.exists()) {
            console.log(`[dev] Serving file from publicDir: ${filePath}`);
            fileToServe = potentialFile;
          }
          // Check for other assets in buildDir if needed
          else {
             const filePathInDist = path.join(buildDir, requestedPath);
             const potentialDistFile = Bun.file(filePathInDist);
             if (await potentialDistFile.exists()) {
                filePath = filePathInDist;
                fileToServe = potentialDistFile;
                console.log(`[dev] Serving asset found in buildDir: ${filePath}`);
             }
          }
        }

        // Development Fallback: If no specific file found, serve publicDir/index.html (SPA behavior)
        if (!fileToServe) {
          console.log(`[dev] File not found: ${filePath}. Serving fallback: ${indexHtmlPath}`);
          fileToServe = Bun.file(indexHtmlPath);
        }
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

console.log(`[${BUN_ENV}] === === === Running server === === ===`);
console.log(`[${BUN_ENV}] * Root: ${rootDir}`);
console.log(`[${BUN_ENV}] * URL: http://${server.hostname}:${server.port}`);
console.log(`[${BUN_ENV}] * API: ${API_BASE_URL}`);

