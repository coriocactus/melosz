export const API_BASE_URL = process.env['API_URL'] || 'http://localhost:8080/api';

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

export interface UserSessionData {
  usdNextPair: [Option, Option] | null; // Use null for Maybe Nothing
  usdRankings: [Option, number][];      // Array of [Option, Rating]
  usdStatus: ComparisonStatus;
}

export interface ComparisonSubmission {
  winnerId: OptionId;
  loserId: OptionId;
}

export interface ApiError {
  errorMessage: string;
}

// --- API Proxy ---

export async function handleAPIProxy(req: Request, targetApiUrl: string): Promise<Response | null> {
  const url = new URL(req.url);
  const pathname = url.pathname;

  if (!pathname.startsWith('/api/')) {
    return null;
  }

  const proxyUrl = `${targetApiUrl}${pathname}${url.search}`;
  console.log(`Proxying API request: ${req.method} ${pathname} -> ${proxyUrl}`);

  const proxyReq = new Request(proxyUrl, {
    method: req.method,
    headers: req.headers,
    body: req.body,
    redirect: 'manual',
  });

  proxyReq.headers.delete('host');

  try {
    const proxyRes = await fetch(proxyReq);

    const responseHeaders = new Headers(proxyRes.headers);

    return new Response(proxyRes.body, {
      status: proxyRes.status,
      statusText: proxyRes.statusText,
      headers: responseHeaders
    });
  } catch (error) {
    console.error(`API Proxy error for ${proxyUrl}:`, error);
    return new Response("API Proxy Error", { status: 502 });
  }
}

// --- Helper for Fetch ---

async function fetchAPI<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
  const url = `${API_BASE_URL}${endpoint}`;
  const defaultHeaders = {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  };

  try {
    const response = await fetch(url, {
      ...options,
      headers: {
        ...defaultHeaders,
        ...options.headers,
      },
    });

    if (!response.ok) {
      let errorData: ApiError = { errorMessage: `HTTP error ${response.status}: ${response.statusText}` };
      try {
        errorData = await response.json();
      } catch (e) {
        console.warn("Could not parse error response JSON:", e);
      }
      throw new Error(errorData.errorMessage || `Request failed with status ${response.status}`);
    }

    if (response.status === 204) {
      return null as T;
    }

    return await response.json() as T;

  } catch (error) {
    console.error(`API request failed: ${options.method || 'GET'} ${url}`, error);
    // Re-throw the error so the calling code can handle it
    throw error;
  }
}

// --- API Functions ---

export function getCompareData(userId: UserId): Promise<UserSessionData> {
  return fetchAPI<UserSessionData>(`/compare/${encodeURIComponent(userId)}`, {
    method: 'GET',
  });
}

export function postComparisonResult(userId: UserId, submission: ComparisonSubmission): Promise<UserSessionData> {
  return fetchAPI<UserSessionData>(`/compare/${encodeURIComponent(userId)}`, {
    method: 'POST',
    body: JSON.stringify(submission),
  });
}
