import type { ComparisonSubmission, UserSessionData, UserId } from './api.ts';

import { getCompareData, postComparisonResult } from './api.ts';
import { ComparisonView } from './ComparisonView.ts';
import { RankingsView } from './RankingsView.ts';
import { StatusView } from './StatusView.ts';

/**
 * Initializes the main application UI.
 * Fetches initial data and mounts sub-components.
 * Handles data flow between components and API interactions.
 * @param rootElement The root DOM element to mount the application into.
 */
export function initializeApp(rootElement: HTMLElement) {
  // --- State ---
  let currentData: UserSessionData | null = null;
  let isLoading = false;
  let currentError: string | null = null;
  const userId: UserId = "coriocactus"; // Hardcoded for now

  // --- HTML Structure ---
  rootElement.innerHTML = `
    <div class="max-w-4xl mx-auto p-4 space-y-6">
      <h1 class="text-3xl font-bold text-center">melosz</h1>

      <div data-loading class="text-center text-blue-600 py-4 hidden">Loading...</div>
      <div data-error class="text-center text-red-600 bg-red-100 border border-red-400 p-3 rounded hidden"></div>

      <div data-comparison-view class="card bg-white p-4 rounded shadow">
        <!-- ComparisonView will mount here -->
      </div>

      <div class="grid md:grid-cols-2 gap-6">
        <div data-rankings-view class="card bg-white p-4 rounded shadow">
          <!-- RankingsView will mount here -->
        </div>
        <div data-status-view class="card bg-white p-4 rounded shadow">
          <!-- StatusView will mount here -->
        </div>
      </div>
    </div>
  `;

  // --- Element Selectors ---
  const loadingEl = rootElement.querySelector<HTMLElement>('[data-loading]')!;
  const errorEl = rootElement.querySelector<HTMLElement>('[data-error]')!;
  const comparisonViewEl = rootElement.querySelector<HTMLElement>('[data-comparison-view]')!;
  const rankingsViewEl = rootElement.querySelector<HTMLElement>('[data-rankings-view]')!;
  const statusViewEl = rootElement.querySelector<HTMLElement>('[data-status-view]')!;

  // --- Mount Sub-Components ---
  ComparisonView(comparisonViewEl);
  RankingsView(rankingsViewEl);
  StatusView(statusViewEl);

  // --- Event Listener for Choices ---
  rootElement.addEventListener('comparisonMade', ((e: CustomEvent<ComparisonSubmission>) => {
    const submission = e.detail;
    handleComparisonSubmit(submission);
  }) as EventListener);

  // --- Data Fetching and State Update Functions ---
  async function fetchData() {
    if (isLoading) return;
    isLoading = true;
    currentError = null;
    render(); // Show loading state

    try {
      console.log("App: Fetching initial data...");
      currentData = await getCompareData(userId);
      console.log("App: Data received:", currentData);
    } catch (error) {
      console.error("App: Error fetching data:", error);
      currentError = error instanceof Error ? error.message : String(error);
      currentData = null; // Clear data on error
    } finally {
      isLoading = false;
      render(); // Render data or error
    }
  }

  async function handleComparisonSubmit(submission: ComparisonSubmission) {
     if (isLoading) return;
     isLoading = true;
     currentError = null;
     render(); // Can optionally show a submitting state here

     try {
       console.log("App: Submitting comparison...", submission);
       currentData = await postComparisonResult(userId, submission);
       console.log("App: Submission successful, new data:", currentData);
     } catch (error) {
       console.error("App: Error submitting comparison:", error);
       currentError = error instanceof Error ? error.message : String(error);
       // Decide if you want to revert state or keep the old one on error
     } finally {
       isLoading = false;
       render(); // Render new data or error
     }
  }


  // --- Rendering Function ---
  function render() {
    console.log("App: Rendering state...");
    // Show/hide loading indicator
    loadingEl.classList.toggle('hidden', !isLoading);

    // Show/hide error message
    if (currentError) {
      errorEl.textContent = `Error: ${currentError}`;
      errorEl.classList.remove('hidden');
    } else {
      errorEl.classList.add('hidden');
    }

    // Dispatch data to sub-components if data is available
    if (currentData) {
      comparisonViewEl.dispatchEvent(new CustomEvent('renderComparison', { detail: currentData.usdNextPair }));
      rankingsViewEl.dispatchEvent(new CustomEvent('renderRankings', { detail: currentData.usdRankings }));
      statusViewEl.dispatchEvent(new CustomEvent('renderStatus', { detail: currentData.usdStatus }));
    } else if (!isLoading) {
        // Handle state where there's no data and not loading (initial or error state)
        // You might want to clear the sub-components or show specific messages
         comparisonViewEl.dispatchEvent(new CustomEvent('renderComparison', { detail: null }));
         rankingsViewEl.dispatchEvent(new CustomEvent('renderRankings', { detail: [] }));
         // StatusView might need a default 'empty' status object if currentData is null
         // Or handle null directly within StatusView's event listener.
         // For now, let's assume StatusView can handle potentially missing status parts if needed.
    }
  }

  // --- Initial Load ---
  fetchData();
}
