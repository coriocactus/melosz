// --- Type Definitions ---

export type OptionId = string;
export type UserId = string;

export interface Option {
  id: OptionId;
  name: string;
  src: string;
}

export interface UserSession {
  usNextPair: [Option, Option] | null;
  usRankings: [Option, number][];
}

export interface ComparisonSubmission {
  winnerId: OptionId;
  loserId: OptionId;
}

export interface APIError {
  error?: string;
  errorMessage?: string;
}

// --- Cookie Helpers ---

export function setCookie(name: string, value: string, hours: number | null) {
  let expires = "";

  if (hours) {
    if (hours < 0) {
      // Delete cookie
      expires = "; expires=Thu, 01 Jan 1970 00:00:00 GMT";
    } else {
      const date = new Date();
      date.setTime(date.getTime() + (hours * 60 * 60 * 1000));
      expires = "; expires=" + date.toUTCString();
    }
  }
  // Added Secure and HttpOnly flags are generally good, but HttpOnly prevents JS access.
  // SameSite=Lax is a reasonable default. Secure should be added if served over HTTPS.
  const cookieString = name + "=" + (value || "") + expires +
    "; path=/; SameSite=Lax"; // Add "; Secure" if using HTTPS
  document.cookie = cookieString;
}

export function getCookie(name: string): string | null {
  const nameEQ = name + "=";
  const ca = document.cookie.split(";");
  for (let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == " ") c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}

// --- Auth API Definitions ---

export interface AuthHash {
  hash: string;
}

// For GET /login and GET /register - response from backend
export interface AuthTokenResponse {
  token: string;
}

// For POST /login and POST /register - payload sent to backend
export interface AuthPayload {
  token: string;
  email: string;
}

// --- API Proxy ---

export async function handleAPIProxy(
  req: Request,
  targetApiUrl: string,
): Promise<Response | null> {
  const url = new URL(req.url);
  const pathname = url.pathname;

  // Only proxy requests intended for the backend API
  if (!pathname.startsWith("/api/")) {
    return null; // Let the frontend server handle other requests
  }

  // Construct the target URL for the backend API
  const target = `${targetApiUrl}${
    pathname.substring("/api".length)
  }${url.search}`;

  console.log(`Proxying API request: ${req.method} ${pathname} -> ${target}`);

  // Prepare headers for the proxy request
  const proxyHeaders = new Headers(req.headers);
  // Remove headers that should not be forwarded or might cause issues
  proxyHeaders.delete("content-length"); // Let the fetch implementation handle this
  proxyHeaders.delete("host"); // Target host is determined by the `target` URL
  // Bun/fetch might automatically handle content-encoding, so avoid forwarding if possible
  // proxyHeaders.delete('content-encoding');

  // Ensure 'Accept' header is set for JSON, typically done in fetchAPI, but good practice here too
  if (!proxyHeaders.has("Accept")) {
    proxyHeaders.set("Accept", "application/json");
  }
  // Content-Type for POST/PUT should ideally be set by the originating fetch call in fetchAPI

  try {
    // Make the request to the backend API
    const proxyRes = await fetch(target, {
      method: req.method,
      headers: proxyHeaders,
      // Forward the request body only for relevant methods
      body: req.method !== "GET" && req.method !== "HEAD"
        ? req.body
        : undefined,
      redirect: "manual", // Let the browser handle redirects based on the proxy's response
    });

    // Prepare response headers to send back to the client
    const responseHeaders = new Headers(proxyRes.headers);

    // Remove headers that might interfere with the client receiving the response correctly
    responseHeaders.delete("transfer-encoding"); // Often problematic when proxying
    responseHeaders.delete("content-encoding"); // Avoid double encoding issues

    // Ensure CORS headers are present for the browser, especially during development
    // when frontend and backend are on different ports.
    if (!responseHeaders.has("Access-Control-Allow-Origin")) {
      responseHeaders.set("Access-Control-Allow-Origin", "*");
    }
    if (!responseHeaders.has("Access-Control-Allow-Headers")) {
      // Ensure necessary headers (like Authorization) are allowed
      responseHeaders.set(
        "Access-Control-Allow-Headers",
        "Content-Type, Authorization, Accept",
      );
    }
    if (!responseHeaders.has("Access-Control-Allow-Methods")) {
      // Specify allowed methods
      responseHeaders.set(
        "Access-Control-Allow-Methods",
        "GET, POST, PUT, DELETE, OPTIONS",
      );
    }

    // Return the backend's response (status, headers, body) to the original client
    return new Response(proxyRes.body, {
      status: proxyRes.status,
      statusText: proxyRes.statusText,
      headers: responseHeaders,
    });
  } catch (error) {
    // Handle network errors during the proxy request
    console.error(`API Proxy error for ${target}:`, error);
    return new Response(
      JSON.stringify({
        error: "API Proxy Error",
        details: error instanceof Error ? error.message : String(error),
      }),
      {
        status: 502, // Bad Gateway indicates the proxy couldn't reach the backend
        headers: { "Content-Type": "application/json" },
      },
    );
  }
}

// --- Helper for Fetch ---

export const API_BASE_URL = "http://localhost:8080"; // Backend API base URL
export const AUTH_COOKIE_NAME = "x-auth-hash";
export const AUTH_COOKIE_GUEST_HOURS = 1;
export const AUTH_COOKIE_PERMANENT_HOURS = 365 * 24;

async function fetchAPI<T>(
  endpoint: string,
  options: RequestInit = {},
): Promise<T> {
  const url = `${API_BASE_URL}${endpoint}`;
  const defaultHeaders: Record<string, string> = {
    "Accept": "application/json",
  };

  // Add Authorization header if auth hash cookie exists, unless explicitly cleared in options
  const authHash = getCookie(AUTH_COOKIE_NAME);
  if (authHash && options.headers?.["Authorization"] !== "") { // Check if Authorization wasn't explicitly cleared
    defaultHeaders["Authorization"] = `MELOSZ ${authHash}`;
  }

  // Merge default headers with provided options, letting options override
  const headers = new Headers({ ...defaultHeaders, ...options.headers });

  // Set Content-Type for JSON body if not already set
  if (
    options.body && typeof options.body === "string" &&
    !headers.has("Content-Type")
  ) {
    try {
      JSON.parse(options.body); // Check if body is valid JSON
      headers.set("Content-Type", "application/json");
    } catch (_e) {
      // Body is not JSON, don't set Content-Type header automatically
      console.warn(
        "fetchAPI: Body provided but not valid JSON and Content-Type not set.",
      );
    }
  }

  try {
    const response = await fetch(url, {
      ...options,
      headers: headers,
    });

    // --- Error Handling ---
    if (!response.ok) {
      const errorData: APIError = {
        errorMessage: `HTTP error ${response.status}: ${response.statusText}`,
      };
      try {
        // Try to parse error response from backend as JSON
        const parsedError = await response.json();
        if (parsedError && typeof parsedError === "object") {
          if ("error" in parsedError && typeof parsedError.error === "string") {
            errorData.errorMessage = parsedError.error; // Use specific error field if present
          } else {
            errorData.errorMessage = JSON.stringify(parsedError); // Fallback to stringify
          }
        } else if (parsedError && typeof parsedError === "string") {
          errorData.errorMessage = parsedError; // Handle plain string errors
        }
        // Include more details if available
        if (
          parsedError && typeof parsedError === "object" &&
          "details" in parsedError
        ) {
          errorData.errorMessage += ` (Details: ${parsedError.details})`;
        }
      } catch (e) {
        // If JSON parsing fails, try to get text response
        console.warn(
          `Could not parse error response JSON for ${response.status} ${url}:`,
          e,
        );
        try {
          const textError = await response.text();
          errorData.errorMessage = textError || errorData.errorMessage; // Use text error if available
        } catch (_textE) {
          // Ignore error during text reading, stick with the initial HTTP error message
        }
      }
      console.error(
        `API Error Response (${response.status} for ${url}):`,
        errorData.errorMessage,
      );

      // Specific handling for 401 Unauthorized
      if (response.status === 401 && authHash) {
        console.warn(
          "Received 401 Unauthorized with an existing auth hash. Assuming hash is invalid.",
        );
        // The App component should listen for this error and dispatch 'authError'
        // which will clear the cookie and potentially trigger re-authentication.
      }
      throw new Error(errorData.errorMessage); // Throw the structured/parsed error message
    }

    // --- Cookie Renewal ---
    if (authHash) {
      // Renew the cookie on successful API interaction using the guest duration.
      // This keeps guest sessions alive and doesn't interfere with permanent cookies
      // other than requiring re-login after inactivity if they had a permanent one.
      setCookie(AUTH_COOKIE_NAME, authHash, AUTH_COOKIE_GUEST_HOURS);
    }

    // --- Handle Successful Responses ---

    // Handle No Content response (e.g., from DELETE or some POSTs)
    if (
      response.status === 204 || response.headers.get("content-length") === "0"
    ) {
      return null as T; // Return null for No Content
    }

    // Assume JSON response for other successful statuses
    return await response.json() as T;
  } catch (error) {
    // Catch fetch errors (network issues) or errors thrown from !response.ok block
    console.error(
      `API request failed: ${options.method || "GET"} ${url}`,
      error,
    );
    // Re-throw the error so the calling code can handle it
    // Ensure it's always an Error object
    if (error instanceof Error) {
      throw error;
    } else {
      throw new Error(String(error)); // Convert non-Error throws to Error objects
    }
  }
}

// --- API Functions ---

/** Gets a guest hash from the backend */
export function getGuestHash(): Promise<AuthHash> {
  console.log(`API: Getting guest hash...`);
  // This specific call should never send an Authorization header
  return fetchAPI<AuthHash>("/guest", {
    method: "GET",
    headers: { "Authorization": "" },
  });
}

/** Fetches the next comparison pair and current rankings for the authenticated user */
export function getCompareData(): Promise<UserSession> {
  console.log(`API: Fetching compare data...`);
  return fetchAPI<UserSession>(`/compare`, { method: "GET" });
}

/** Posts the result of a comparison and gets the updated session data */
export function postComparisonResult(
  submission: ComparisonSubmission,
): Promise<UserSession> {
  console.log(`API: Posting comparison...`, submission);
  return fetchAPI<UserSession>(`/compare`, {
    method: "POST",
    body: JSON.stringify(submission),
  });
}

// --- Login/Register API Functions ---

/** Gets a temporary token required to initiate the login process */
export function getLoginToken(): Promise<AuthTokenResponse> {
  console.log(`API: Getting login token...`);
  return fetchAPI<AuthTokenResponse>("/login", {
    method: "GET",
    headers: { "Authorization": "" },
  });
}

/** Sends the email and temporary token to the backend to trigger the login email */
export function postLoginPayload(payload: AuthPayload): Promise<void> {
  console.log(`API: Posting login payload for email: ${payload.email}`);
  return fetchAPI<void>("/login", {
    method: "POST",
    headers: { "Authorization": "" },
    body: JSON.stringify(payload),
  });
}

/** Gets a temporary token required to initiate the registration process */
export function getRegisterToken(): Promise<AuthTokenResponse> {
  console.log(`API: Getting register token...`);
  return fetchAPI<AuthTokenResponse>("/register", {
    method: "GET",
    headers: { "Authorization": "" },
  });
}

/** Sends the email and temporary token to the backend to trigger the registration email */
export function postRegisterPayload(payload: AuthPayload): Promise<void> {
  console.log(`API: Posting register payload for email: ${payload.email}`);
  return fetchAPI<void>("/register", {
    method: "POST",
    headers: { "Authorization": "" },
    body: JSON.stringify(payload),
  });
}
