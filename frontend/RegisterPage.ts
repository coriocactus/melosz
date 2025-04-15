export function RegisterPage(el: HTMLElement) {
  let isLoading = false;
  let message = { text: "", type: "info" }; // type: 'info', 'success', 'error'

  // Set base HTML structure
  el.innerHTML = `
        <div class="max-w-sm mx-auto card bg-white p-6 rounded shadow-md">
            <h2 class="text-2xl font-semibold text-center mb-4">Register</h2>
            <p class="text-center text-gray-600 mb-6">Enter your email address to create an account. We'll send you a confirmation link.</p>
            <form data-register-form novalidate>
                <div class="mb-4">
                    <label for="register-email" class="block text-sm font-medium text-gray-700 mb-1">Email</label>
                    <input type="email" id="register-email" name="email" required
                           class="w-full px-3 py-2 border border-gray-300 rounded shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                           placeholder="you@example.com">
                </div>
                <button type="submit" data-submit-button
                        class="w-full py-2 px-4 bg-blue-600 hover:bg-blue-700 text-white font-semibold rounded shadow-sm disabled:opacity-50">
                    Send Confirmation Link
                </button>
            </form>
            <div data-message class="mt-4 text-center text-sm hidden"></div>
             <div class="mt-6 text-center text-sm">
                <a href="/#login" class="text-blue-600 hover:underline">Already have an account? Login</a>
                 <span class="mx-2 text-gray-400">|</span>
                 <a href="/#" class="text-blue-600 hover:underline">Continue as Guest</a>
            </div>
        </div>
    `;

  // --- Element Selectors ---
  const formEl = el.querySelector<HTMLFormElement>("[data-register-form]")!;
  const emailInputEl = el.querySelector<HTMLInputElement>("#register-email")!;
  const submitButtonEl = el.querySelector<HTMLButtonElement>(
    "[data-submit-button]",
  )!;
  const messageEl = el.querySelector<HTMLDivElement>("[data-message]")!;

  // --- Event Listeners ---

  // Handle form submission
  formEl.addEventListener("submit", (e) => {
    e.preventDefault();
    if (isLoading) return;

    const email = emailInputEl.value.trim();
    if (!email || !emailInputEl.checkValidity()) {
      message = { text: "Please enter a valid email address.", type: "error" };
      update();
      return;
    }

    // Dispatch action event upwards to App
    el.dispatchEvent(
      new CustomEvent("requestRegisterLink", {
        detail: { email },
        bubbles: true, // Allow event to bubble up
      }),
    );
  });

  // Listen for status updates from App component
  el.addEventListener(
    "registerStatus",
    ((
      e: CustomEvent<
        { isLoading?: boolean; success?: boolean; message?: string }
      >,
    ) => {
      isLoading = e.detail.isLoading ?? false;
      if (e.detail.success === true) {
        message = {
          text: e.detail.message ||
            "Confirmation link sent! Check your email (including spam folder) to complete registration.",
          type: "success",
        };
        emailInputEl.value = ""; // Clear input on success
      } else if (e.detail.success === false) {
        message = {
          text: e.detail.message || "Failed to send confirmation link.",
          type: "error",
        };
      } else if (!isLoading) {
        // Clear message if just finished loading without explicit success/error
        message = { text: "", type: "info" };
      }
      update();
    }) as EventListener,
  );

  // Listen for visibility changes
  el.addEventListener(
    "viewVisibility",
    ((e: CustomEvent<{ visible: boolean }>) => {
      if (e.detail.visible) {
        // Reset state when view becomes visible
        isLoading = false;
        message = { text: "", type: "info" };
        emailInputEl.value = "";
      }
      update();
    }) as EventListener,
  );

  // --- Update Function (Idempotent) ---
  function update() {
    // Update button state
    submitButtonEl.disabled = isLoading;
    submitButtonEl.textContent = isLoading
      ? "Sending..."
      : "Send Confirmation Link";

    // Update message display
    if (message.text) {
      messageEl.textContent = message.text;
      messageEl.className = `mt-4 text-center text-sm ${
        message.type === "error"
          ? "text-red-600"
          : message.type === "success"
          ? "text-green-600"
          : "text-gray-600" // info
      }`;
      messageEl.classList.remove("hidden");
    } else {
      messageEl.classList.add("hidden");
    }
  }
}
