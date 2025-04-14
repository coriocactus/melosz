import { serve } from "bun";
import path from "path";
import process from "node:process";

import {
  API_BASE_URL,
  AUTH_COOKIE_NAME,
  AUTH_COOKIE_PERMANENT_HOURS,
  type AuthHash,
  handleAPIProxy,
} from "./api.ts";

// --- Configuration ---
const PORT = process.env["PORT"] || 3000;
const BUN_ENV = process.env["BUN_ENV"] || "dev";

// --- Path Definitions ---
const scriptDir = import.meta.dir;
const buildDir = path.resolve(scriptDir, "dist");
const indexHtmlPath = path.join(buildDir, "index.html");

// --- Server Logic ---
const server = serve({
  port: PORT,
  async fetch(req) {
    const url = new URL(req.url);
    const requestedPath = url.pathname;
    let filePath = "";
    let fileToServe: ReturnType<typeof Bun.file> | null = null;

    console.log(`[${BUN_ENV}] Request: ${req.method} ${requestedPath}`);

    // --- API Proxy Handling ---
    const proxyResponse = await handleAPIProxy(req, API_BASE_URL);
    if (proxyResponse) {
      return proxyResponse;
    }

    // --- Specific Route Handling (Auth Confirmation Link) ---
    const authMatch = requestedPath.match(/^\/auth\/([a-zA-Z0-9._=\-]+)$/);
    if (authMatch && req.method === "GET") {
      const token = authMatch[1];
      console.log(
        `[${BUN_ENV}] Handling auth confirmation for token: ${token}`,
      );
      try {
        const targetAuthUrl = `${API_BASE_URL}/auth/${token}`;
        console.log(
          `[${BUN_ENV}] Proxying auth confirmation to: ${targetAuthUrl}`,
        );
        const authApiReq = new Request(targetAuthUrl, {
          method: "GET",
          headers: { "Accept": "application/json" },
        });
        const backendResponse = await fetch(authApiReq);

        if (backendResponse.ok) {
          const authData = await backendResponse.json() as AuthHash;
          if (authData && authData.hash) {
            console.log(
              `[${BUN_ENV}] Auth success. Received hash: ${
                authData.hash.substring(0, 10)
              }...`,
            );
            const headers = new Headers();
            const cookieString =
              `${AUTH_COOKIE_NAME}=${authData.hash}; Path=/; Max-Age=${
                AUTH_COOKIE_PERMANENT_HOURS * 60 * 60
              }; SameSite=Lax`; // Add "; Secure" if using HTTPS
            headers.set("Set-Cookie", cookieString);
            headers.set("Location", "/"); // Redirect to main app page
            return new Response("Authentication successful. Redirecting...", {
              status: 302,
              headers,
            });
          } else {
            console.error(
              `[${BUN_ENV}] Auth confirmation succeeded but response format was unexpected:`,
              authData,
            );
            const headers = new Headers();
            headers.set("Location", "/#login?error=auth_response_invalid");
            return new Response("Redirecting...", { status: 302, headers });
            // return new Response("Authentication failed (Invalid Response).", { status: 500 });
          }
        } else {
          const errorBody = await backendResponse.text();
          console.error(
            `[${BUN_ENV}] Auth confirmation failed (Backend Status ${backendResponse.status}): ${errorBody}`,
          );
          const headers = new Headers();
          headers.set(
            "Location",
            `/#login?error=auth_failed&status=${backendResponse.status}`,
          );
          return new Response("Redirecting...", { status: 302, headers });
          // return new Response(`Authentication failed: ${errorBody || backendResponse.statusText}`, { status: backendResponse.status });
        }
      } catch (error) {
        console.error(
          `[${BUN_ENV}] Error processing auth confirmation for ${token}:`,
          error,
        );
        const headers = new Headers();
        headers.set("Location", "/#login?error=internal_server_error");
        return new Response("Redirecting...", { status: 302, headers });
        // return new Response("Internal Server Error during authentication.", { status: 500 });
      }
    }

    // --- Static File & SPA Fallback Handling ---
    try {
      filePath = path.join(
        buildDir,
        requestedPath.startsWith("/dist/")
          ? requestedPath.substring("/dist".length)
          : requestedPath,
      );

      // Security check: Prevent path traversal outside build directory
      if (!filePath.startsWith(buildDir)) {
        // Potentially risky path, fall through to SPA index
        console.warn(
          `[${BUN_ENV}] Access attempt potentially outside build directory: ${requestedPath}`,
        );
      } else {
        const potentialFile = Bun.file(filePath);
        if (await potentialFile.exists()) {
          const stats = await Bun.file(filePath).stat();
          if (!stats.isDirectory()) {
            console.log(`[${BUN_ENV}] Serving static asset: ${filePath}`);
            fileToServe = potentialFile;
          }
        }
      }

      // If no specific asset found, serve the SPA index.html for all other paths
      if (!fileToServe) {
        console.log(
          `[${BUN_ENV}] Path not matched to asset: ${requestedPath}. Serving SPA fallback: ${indexHtmlPath}`,
        );
        fileToServe = Bun.file(indexHtmlPath);
      }

      // --- Serve the determined file ---
      if (fileToServe && await fileToServe.exists()) {
        return new Response(fileToServe);
      } else {
        console.error(
          `[${BUN_ENV}] Critical Error: Could not find index.html fallback: ${indexHtmlPath}`,
        );
        return new Response("Not Found", { status: 404 });
      }
    } catch (error) {
      console.error(
        `[${BUN_ENV}] Error processing request ${requestedPath}:`,
        error,
      );
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
