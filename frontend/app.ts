import type { ComparisonSubmission, UserSession } from "./api.ts";

import {
  AUTH_COOKIE_GUEST_HOURS,
  AUTH_COOKIE_NAME,
  type AuthPayload,
  getCompareData,
  getCookie,
  getGuestHash,
  getLoginToken,
  getRegisterToken,
  postComparisonResult,
  postLoginPayload,
  postRegisterPayload,
  setCookie,
} from "./api.ts";
import { ComparisonView } from "./ComparisonView.ts";
import { RankingsView } from "./RankingsView.ts";
import { LoginPage } from "./LoginPage.ts";
import { RegisterPage } from "./RegisterPage.ts";

type AuthStatus = "unknown" | "initializing" | "guest" | "registered" | "error";
type CurrentView = "app" | "login" | "register";

export function initializeApp(rootElement: HTMLElement) {
  // --- State ---
  let currentData: UserSession | null = null;
  let isLoadingApi = false;
  let currentError: string | null = null;
  let authStatus: AuthStatus = "unknown";
  let currentView: CurrentView = "app";

  rootElement.innerHTML = `
    <div class="max-w-4xl mx-auto p-4 space-y-6">
        <h1 class="text-3xl font-bold text-center mb-4">melosz</h1>

        <div data-auth-view>
            <div data-login-page class="hidden"></div>
            <div data-register-page class="hidden"></div>
        </div>

        <div data-app-view class="hidden space-y-6">
           <div class="text-center text-sm text-gray-500">
                (<a href="#login" class="text-blue-600 hover:underline">Login</a> /
                <a href="#register" class="text-blue-600 hover:underline">Register</a>)
           </div>

          <div data-auth-status class="text-center text-gray-600 py-4 hidden"></div>
          <div data-loading class="text-center text-blue-600 py-4 hidden">Loading comparison data...</div>
          <div data-error class="text-center text-red-600 bg-red-100 border border-red-400 p-3 rounded hidden"></div>

          <div data-comparison-view class="card bg-white p-4 rounded shadow">
            <!-- ComparisonView will mount here -->
          </div>

          <div data-rankings-view class="card bg-white p-4 rounded shadow">
            <!-- RankingsView will mount here -->
          </div>
        </div>
    </div>
  `;

  // --- Element Selectors ---
  const appViewEl = rootElement.querySelector<HTMLElement>("[data-app-view]")!;
  const authViewEl = rootElement.querySelector<HTMLElement>(
    "[data-auth-view]",
  )!; // Container for auth pages
  const loginPageEl = rootElement.querySelector<HTMLElement>(
    "[data-login-page]",
  )!;
  const registerPageEl = rootElement.querySelector<HTMLElement>(
    "[data-register-page]",
  )!;
  // Selectors within App View
  const authStatusEl = appViewEl.querySelector<HTMLElement>(
    "[data-auth-status]",
  )!;
  const loadingEl = appViewEl.querySelector<HTMLElement>("[data-loading]")!;
  const errorEl = appViewEl.querySelector<HTMLElement>("[data-error]")!;
  const comparisonViewEl = appViewEl.querySelector<HTMLElement>(
    "[data-comparison-view]",
  )!;
  const rankingsViewEl = appViewEl.querySelector<HTMLElement>(
    "[data-rankings-view]",
  )!;

  // --- Mount Sub-Components ---
  ComparisonView(comparisonViewEl);
  RankingsView(rankingsViewEl);
  LoginPage(loginPageEl);
  RegisterPage(registerPageEl);

  // --- Event Listeners ---

  // Listen for comparison choices made by the user (delegated to root)
  rootElement.addEventListener(
    "comparisonMade",
    ((e: CustomEvent<ComparisonSubmission>) => {
      if (authStatus === "guest" || authStatus === "registered") {
        handleComparisonSubmit(e.detail);
      } else {
        console.warn("Cannot submit comparison, user not authenticated.");
        // Redirect to login?
        navigateTo("login");
      }
    }) as EventListener,
  );

  // Listen for requests to initialize a guest session
  // Cast handler to EventListener to satisfy TypeScript
  rootElement.addEventListener(
    "requestGuestSession",
    handleRequestGuestSession as EventListener,
  );

  // Listen for authentication status changes
  rootElement.addEventListener(
    "authStatus",
    ((
      e: CustomEvent<{ status: AuthStatus; hash?: string; error?: string }>,
    ) => {
      console.log("App: Received authStatus event", e.detail);
      const previousAuthStatus = authStatus;
      authStatus = e.detail.status;
      // currentAuthHash = e.detail.hash ?? null; // Assignment removed
      currentError = e.detail.error ?? null;

      // Clear previous data if auth fails or resets
      if (authStatus !== "guest" && authStatus !== "registered") {
        currentData = null;
      }

      // If authentication succeeded, navigate away from login/register pages
      if (
        (authStatus === "guest" || authStatus === "registered") &&
        (previousAuthStatus !== "guest" && previousAuthStatus !== "registered")
      ) {
        navigateTo("app"); // Navigate to main app view on successful auth
        fetchData(); // Fetch data immediately after successful auth
      } else {
        render(); // Update UI based on new auth status
      }
    }) as EventListener,
  );

  // Listen for authentication errors that require clearing state
  rootElement.addEventListener(
    "authError",
    ((e: CustomEvent<{ message: string }>) => {
      console.warn("App: Received authError event", e.detail.message);
      setCookie(AUTH_COOKIE_NAME, "", -1); // Clear the potentially bad cookie
      // currentAuthHash = null; // Variable removed
      // Dispatch event to update status and request a new guest session
      // Update status directly first, then optionally request guest session
      authStatus = "error";
      currentError =
        `Authentication failed: ${e.detail.message}. Session cleared.`;
      render(); // Show the error state immediately

      // Don't automatically request guest session here, let user decide (e.g., retry login or guest)
      // navigateTo('login'); // Or force navigation to login
    }) as EventListener,
  );

  // Listen for login/register requests from page components
  // Cast handlers to EventListener
  rootElement.addEventListener(
    "requestLoginLink",
    handleRequestLogin as EventListener,
  );
  rootElement.addEventListener(
    "requestRegisterLink",
    handleRequestRegister as EventListener,
  );

  // Listen for hash changes to handle routing
  globalThis.addEventListener("hashchange", handleRouteChange);

  // --- Action Handlers ---

  // Handles the 'requestGuestSession' event
  async function handleRequestGuestSession(e?: Event) { // Use base Event type, check for CustomEvent inside
    // Check if it's the correct event type before accessing detail
    const detail = (e instanceof CustomEvent)
      ? e.detail as { reason?: string }
      : undefined;

    if (authStatus === "initializing") return; // Prevent concurrent requests

    console.log("App: Handling requestGuestSession...", detail?.reason);
    authStatus = "initializing";
    currentError = detail?.reason ?? null; // Show reason if provided
    render(); // Show initializing state

    try {
      const authResponse = await getGuestHash();
      setCookie(AUTH_COOKIE_NAME, authResponse.hash, AUTH_COOKIE_GUEST_HOURS);
      rootElement.dispatchEvent(
        new CustomEvent("authStatus", {
          detail: { status: "guest", hash: authResponse.hash },
        }),
      );
    } catch (error) {
      console.error("App: Error getting guest hash:", error);
      rootElement.dispatchEvent(
        new CustomEvent("authStatus", {
          detail: {
            status: "error",
            error: `Failed to initialize session: ${
              error instanceof Error ? error.message : String(error)
            }`,
          },
        }),
      );
    }
  }

  // Handle request from LoginPage
  async function handleRequestLogin(e: Event) { // Use base Event type
    if (!(e instanceof CustomEvent)) return; // Type guard
    const email = (e.detail as { email: string }).email;

    console.log(`App: Handling requestLoginLink for ${email}`);
    // Dispatch loading state down to LoginPage
    loginPageEl.dispatchEvent(
      new CustomEvent("loginStatus", { detail: { isLoading: true } }),
    );
    try {
      const tokenResponse = await getLoginToken();
      const payload: AuthPayload = { email, token: tokenResponse.token };
      await postLoginPayload(payload);
      // Dispatch success state down
      loginPageEl.dispatchEvent(
        new CustomEvent("loginStatus", {
          detail: { isLoading: false, success: true },
        }),
      );
    } catch (error) {
      console.error("App: Error during login request:", error);
      // Dispatch error state down
      loginPageEl.dispatchEvent(
        new CustomEvent("loginStatus", {
          detail: {
            isLoading: false,
            success: false,
            message: error instanceof Error ? error.message : String(error),
          },
        }),
      );
    }
  }

  // Handle request from RegisterPage
  async function handleRequestRegister(e: Event) { // Use base Event type
    if (!(e instanceof CustomEvent)) return; // Type guard
    const email = (e.detail as { email: string }).email;

    console.log(`App: Handling requestRegisterLink for ${email}`);
    // Dispatch loading state down to RegisterPage
    registerPageEl.dispatchEvent(
      new CustomEvent("registerStatus", { detail: { isLoading: true } }),
    );
    try {
      const tokenResponse = await getRegisterToken();
      const payload: AuthPayload = { email, token: tokenResponse.token };
      await postRegisterPayload(payload);
      // Dispatch success state down
      registerPageEl.dispatchEvent(
        new CustomEvent("registerStatus", {
          detail: { isLoading: false, success: true },
        }),
      );
    } catch (error) {
      console.error("App: Error during registration request:", error);
      // Dispatch error state down
      registerPageEl.dispatchEvent(
        new CustomEvent("registerStatus", {
          detail: {
            isLoading: false,
            success: false,
            message: error instanceof Error ? error.message : String(error),
          },
        }),
      );
    }
  }

  // --- Data Fetching and State Update Functions ---
  async function fetchData() {
    if (
      isLoadingApi || (authStatus !== "guest" && authStatus !== "registered")
    ) {
      console.warn(
        "App: Skipping fetch. Loading:",
        isLoadingApi,
        "Auth Status:",
        authStatus,
      );
      return;
    }
    isLoadingApi = true;
    // Clear previous API errors before fetching
    currentError = null;
    render();

    let wasAuthError = false; // Flag to check if authError was dispatched
    try {
      console.log("App: Fetching comparison data...");
      currentData = await getCompareData();
      console.log("App: Data received:", currentData);
    } catch (error) {
      console.error("App: Error fetching data:", error);
      if (
        error instanceof Error &&
        (error.message.includes("Unauthorized") ||
          error.message.includes("401"))
      ) {
        wasAuthError = true;
        rootElement.dispatchEvent(
          new CustomEvent("authError", { detail: { message: error.message } }),
        );
      } else {
        currentError = error instanceof Error ? error.message : String(error);
        currentData = null;
      }
    } finally {
      isLoadingApi = false;
      // Render only if we didn't dispatch an authError (which triggers its own render)
      if (!wasAuthError) {
        render();
      }
    }
  }

  async function handleComparisonSubmit(submission: ComparisonSubmission) {
    if (
      isLoadingApi || (authStatus !== "guest" && authStatus !== "registered")
    ) {
      console.warn(
        "App: Skipping submit. Loading:",
        isLoadingApi,
        "Auth Status:",
        authStatus,
      );
      if (authStatus !== "guest" && authStatus !== "registered") {
        navigateTo("login");
      }
      return;
    }
    isLoadingApi = true;
    // Clear previous API error before submit
    currentError = null;
    render();

    let wasAuthError = false; // Flag to check if authError was dispatched
    try {
      console.log("App: Submitting comparison...", submission);
      currentData = await postComparisonResult(submission);
      console.log("App: Submission successful, new data:", currentData);
    } catch (error) {
      console.error("App: Error submitting comparison:", error);
      if (
        error instanceof Error &&
        (error.message.includes("Unauthorized") ||
          error.message.includes("401"))
      ) {
        wasAuthError = true;
        rootElement.dispatchEvent(
          new CustomEvent("authError", { detail: { message: error.message } }),
        );
      } else {
        currentError = error instanceof Error ? error.message : String(error);
      }
    } finally {
      isLoadingApi = false;
      // Render only if we didn't dispatch an authError
      if (!wasAuthError) {
        render();
      }
    }
  }

  // --- Routing ---
  function handleRouteChange() {
    const hash = globalThis.location.hash.split("?")[0]; // Ignore query params for routing
    console.log("App: Hash changed to:", hash);
    if (hash === "#login") {
      navigateTo("login");
    } else if (hash === "#register") {
      navigateTo("register");
    } else {
      // Default to app view
      navigateTo("app");
    }
  }

  function navigateTo(view: CurrentView) {
    if (view === currentView) return; // No change needed

    console.log("App: Navigating to view:", view);
    currentView = view;
    // Update hash only if navigating to login/register, clear for app view
    if (view === "login") globalThis.location.hash = "login";
    else if (view === "register") globalThis.location.hash = "register";
    else if (globalThis.location.hash) globalThis.location.hash = ""; // Clear hash for app view

    // Dispatch visibility events to page components
    loginPageEl.dispatchEvent(
      new CustomEvent("viewVisibility", {
        detail: { visible: view === "login" },
      }),
    );
    registerPageEl.dispatchEvent(
      new CustomEvent("viewVisibility", {
        detail: { visible: view === "register" },
      }),
    );

    // If navigating TO app view and not authenticated, trigger auth check/guest request
    if (
      view === "app" && authStatus !== "guest" && authStatus !== "registered" &&
      authStatus !== "initializing" && authStatus !== "error"
    ) {
      console.log(
        "App: Navigated to app view, but not authenticated. Requesting guest session.",
      );
      rootElement.dispatchEvent(new CustomEvent("requestGuestSession"));
    } else if (
      view === "app" &&
      (authStatus === "guest" || authStatus === "registered") && !currentData
    ) {
      // If navigating to app view and authenticated but no data, fetch it
      fetchData();
    } else {
      render(); // Re-render based on the new view
    }
  }

  // --- Rendering Function (Idempotent and Complete) ---
  function render() {
    console.log(
      "App: Rendering state - View:",
      currentView,
      "AuthStatus:",
      authStatus,
      "IsLoadingApi:",
      isLoadingApi,
      "Error:",
      !!currentError,
      "Data:",
      !!currentData,
    );

    // --- View Visibility ---
    appViewEl.classList.toggle("hidden", currentView !== "app");
    authViewEl.classList.toggle("hidden", currentView === "app");
    loginPageEl.classList.toggle("hidden", currentView !== "login");
    registerPageEl.classList.toggle("hidden", currentView !== "register");

    // --- Render only if App View is active ---
    if (currentView === "app") {
      // Render Auth Status and Loading States within App View
      authStatusEl.classList.add("hidden"); // Default hide
      loadingEl.classList.add("hidden");
      errorEl.classList.add("hidden");

      switch (authStatus) {
        case "unknown":
        case "initializing":
          authStatusEl.textContent = currentError || "Initializing session..."; // Show reason if available
          authStatusEl.classList.remove("hidden");
          break;
        case "error":
          errorEl.textContent = `Authentication Error: ${
            currentError || "Unknown authentication error."
          }`;
          errorEl.classList.remove("hidden");
          break;
        case "guest":
        case "registered":
          loadingEl.classList.toggle("hidden", !isLoadingApi);
          // Show API error if it exists (auth errors handled above)
          if (currentError) { // No need to check authStatus !== 'error' here, as that case is handled above
            errorEl.textContent = `API Error: ${currentError}`;
            errorEl.classList.remove("hidden");
          }
          break;
      }

      // Render Main Content Views within App View
      // Show content only if authenticated and not in an error state (auth or API)
      const showMainContent =
        (authStatus === "guest" || authStatus === "registered") &&
        !currentError;
      comparisonViewEl.classList.toggle(
        "hidden",
        !showMainContent || isLoadingApi,
      ); // Also hide while loading API
      rankingsViewEl.classList.toggle(
        "hidden",
        !showMainContent || isLoadingApi,
      ); // Also hide while loading API

      if (showMainContent && !isLoadingApi) {
        comparisonViewEl.dispatchEvent(
          new CustomEvent("renderComparison", {
            detail: currentData?.usNextPair ?? null,
          }),
        );
        rankingsViewEl.dispatchEvent(
          new CustomEvent("renderRankings", {
            detail: currentData?.usRankings ?? [],
          }),
        );
      } else {
        // Clear sub-components if main content is hidden or loading
        comparisonViewEl.dispatchEvent(
          new CustomEvent("renderComparison", { detail: null }),
        );
        rankingsViewEl.dispatchEvent(
          new CustomEvent("renderRankings", { detail: [] }),
        );
      }
    }
  }

  // --- Initial Check ---
  function checkInitialAuth() {
    const existingHash = getCookie(AUTH_COOKIE_NAME);
    if (existingHash) {
      console.log("App: Found existing auth cookie on initial load.");
      // Assume registered until proven otherwise by API call
      // Directly dispatch status and let routing handle view/data loading
      rootElement.dispatchEvent(
        new CustomEvent("authStatus", {
          detail: { status: "registered", hash: existingHash },
        }),
      );
    } else {
      console.log("App: No existing auth cookie.");
      // Let routing handle initial state, might trigger guest request if needed
      handleRouteChange();
    }
  }

  // --- Start Application ---
  checkInitialAuth(); // Checks cookie and dispatches authStatus or triggers routing
}
