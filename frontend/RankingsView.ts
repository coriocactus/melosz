import type { Option } from "./api.ts";

/**
 * Mounts the rankings view component.
 * Handles displaying the sorted list of options and their ratings.
 * @param el The container element for the rankings view.
 */
export function RankingsView(el: HTMLElement) {
  // Initial structure with Tailwind classes
  el.innerHTML = `
    <h2 class="text-xl font-semibold mb-2">Current Rankings</h2>
    <div class="overflow-x-auto">
      <table class="min-w-full bg-white border border-gray-300">
        <thead class="bg-gray-100">
          <tr>
            <th class="border px-4 py-2 text-left">Rank</th>
            <th class="border px-4 py-2 text-left">Option</th>
            <th class="border px-4 py-2 text-left">Rating</th>
          </tr>
        </thead>
        <tbody data-rankings-body>
          <!-- Ranking rows will be added here -->
        </tbody>
      </table>
    </div>
  `;

  const tbody = el.querySelector<HTMLTableSectionElement>(
    "[data-rankings-body]",
  )!;

  // Event listener to update the view when new data arrives
  el.addEventListener(
    "renderRankings",
    ((e: CustomEvent<[Option, number][]>) => {
      const rankings = e.detail;
      update(rankings);
    }) as EventListener,
  );

  function update(rankings: [Option, number][]) {
    // Clear previous rankings
    tbody.innerHTML = "";

    if (rankings.length === 0) {
      const row = tbody.insertRow();
      const cell = row.insertCell();
      cell.colSpan = 3;
      cell.textContent = "No rankings available yet.";
      cell.className = "text-center text-gray-500 py-4";
    } else {
      rankings.forEach(([option, rating], index) => {
        const rank = index + 1;
        const row = tbody.insertRow();
        row.className = "hover:bg-gray-50"; // Add hover effect

        // Rank cell
        const rankCell = row.insertCell();
        rankCell.textContent = String(rank);
        rankCell.className = "border px-4 py-2";

        // Option Name cell
        const nameCell = row.insertCell();
        nameCell.textContent = option.name;
        nameCell.className = "border px-4 py-2";

        // Rating cell
        const ratingCell = row.insertCell();
        ratingCell.textContent = rating.toFixed(2); // Format rating to 2 decimal places
        ratingCell.className = "border px-4 py-2";
      });
    }
  }
}
