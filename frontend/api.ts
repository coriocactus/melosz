// --- Type Definitions ---

export type OptionId = string;
export type UserId = string;

export interface Option {
  id: OptionId;
  name: string;
  src: string;
}

export interface ComparisonStatus {
  statusProgress: [number, number, number]; // [completed, total, percent]
  statusAgreement: number;                  // 0-100
  statusConsistency: number;                // 0-100
  statusViolations: [Option, Option, Option][];
  statusIsComplete: boolean;
}

export interface UserSession {
  usNextPair: [Option, Option] | null; // Use null for Maybe Nothing
  usRankings: [Option, number][];      // Array of [Option, Rating]
  usStatus: ComparisonStatus;
}

export interface ComparisonSubmission {
  winnerId: OptionId;
  loserId: OptionId;
}

export interface APIError {
  error?: string;
  errorMessage?: string;
}

// --- API Proxy ---

export async function handleAPIProxy(req: Request, targetApiUrl: string): Promise<Response | null> {
  const url = new URL(req.url);
  const pathname = url.pathname;

  if (!pathname.startsWith('/api/')) {
    return null;
  }

  const target = `${targetApiUrl}${pathname.substring('/api'.length)}${url.search}`;

  console.log(`Proxying API request: ${req.method} ${pathname} -> ${target}`);

  const proxyHeaders = new Headers(req.headers);
  proxyHeaders.delete('content-length');
  proxyHeaders.delete('host');
  if (req.method === 'POST' && !proxyHeaders.has('Content-Type')) {
    proxyHeaders.set('Content-Type', 'application/json');
  }
  proxyHeaders.set('Accept', 'application/json');

  try {
    const proxyRes = await fetch(target, {
      method: req.method,
      headers: proxyHeaders,
      body: req.method !== 'GET' && req.method !== 'HEAD' ? req.body : undefined,
      redirect: 'manual',
    });

    const responseHeaders = new Headers(proxyRes.headers);

    responseHeaders.delete('transfer-encoding');

    // Check for CORS headers needed by the browser, even if it's the same origin due to different ports during dev
    if (!responseHeaders.has('Access-Control-Allow-Origin')) {
      // Adjust for prod if needed
      responseHeaders.set('Access-Control-Allow-Origin', '*');
    }

    return new Response(proxyRes.body, {
      status: proxyRes.status,
      statusText: proxyRes.statusText,
      headers: responseHeaders
    });
  } catch (error) {
    console.error(`API Proxy error for ${target}:`, error);
    return new Response(JSON.stringify({ error: "API Proxy Error" }), {
      status: 502,
      headers: { 'Content-Type': 'application/json' }
    });
  }
}

// --- Helper for Fetch ---

export const API_BASE_URL = 'http://localhost:8080';

async function fetchAPI<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
  const url = `${API_BASE_URL}${endpoint}`;
  const defaultHeaders = {
    'Accept': 'application/json',
  };

  const headers = new Headers({ ...defaultHeaders, ...options.headers });

  if (options.body && !headers.has('Content-Type')) {
    headers.set('Content-Type', 'application/json');
  }

  try {
    const response = await fetch(url, {
      ...options,
      headers: headers,
    });

    if (!response.ok) {
      let errorData: APIError = { errorMessage: `HTTP error ${response.status}: ${response.statusText}` };
      try {
        const parsedError = await response.json();
        if (parsedError.error) {
          errorData.errorMessage = parsedError.error;
        } else {
          errorData.errorMessage = JSON.stringify(parsedError);
        }
      } catch (e) {
        console.warn("Could not parse error response JSON:", e);
        try {
          const textError = await response.text();
          errorData.errorMessage = textError || errorData.errorMessage;
        } catch (textE) {
        }
      }
      console.error(`API Error Response (${response.status} for ${url}):`, errorData.errorMessage);
      throw new Error(errorData.errorMessage);
    }

    // Handle No Content response (e.g., from DELETE or some POSTs)
    if (response.status === 204 || response.headers.get('content-length') === '0') {
      return null as T;
    }

    // Assume JSON response for other successful statuses
    return await response.json() as T;

  } catch (error) {
    console.error(`API request failed: ${options.method || 'GET'} ${url}`, error);
    // Re-throw the error so the calling code can handle it
    // Ensure it's always an Error object
    if (error instanceof Error) {
      throw error;
    } else {
      throw new Error(String(error));
    }
  }
}

// --- API Functions ---

export function getCompareData(userId: UserId): Promise<UserSession> {
  console.log(`API: Fetching compare data for user: ${userId}`);
  return fetchAPI<UserSession>(`/compare/${encodeURIComponent(userId)}`, {
    method: 'GET',
  });
}

export function postComparisonResult(userId: UserId, submission: ComparisonSubmission): Promise<UserSession> {
  console.log(`API: Posting comparison for user: ${userId}`, submission);
  return fetchAPI<UserSession>(`/compare/${encodeURIComponent(userId)}`, {
    method: 'POST',
    body: JSON.stringify(submission),
  });
}
