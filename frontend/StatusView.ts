import type { ComparisonStatus, Option } from './api.ts';

/**
 * Mounts the status view component.
 * Displays progress, scores, and violations.
 * @param el The container element for the status view.
 */
export function StatusView(el: HTMLElement) {
  // Initial structure with data attributes for easy targeting
  el.innerHTML = `
    <h2 class="text-xl font-semibold mb-2">Session Status</h2>
    <div class="space-y-2">
      <p data-status-progress>Progress: Calculating...</p>
      <p data-status-agreement>Agreement (vs ELO): Calculating...</p>
      <p data-status-consistency>Consistency (Transitivity): Calculating...</p>
      <div>
        <h3 class="text-lg font-medium mt-3 mb-1">Violations:</h3>
        <ul data-status-violations class="list-disc pl-5 space-y-1 text-red-600">
          <!-- Violation items will be added here -->
          <li data-status-no-violations class="text-gray-500 italic">None detected.</li>
        </ul>
      </div>
       <p data-status-completion class="mt-4 font-bold text-green-700 hidden"></p>
    </div>
  `;

  const progressEl = el.querySelector<HTMLParagraphElement>('[data-status-progress]')!;
  const agreementEl = el.querySelector<HTMLParagraphElement>('[data-status-agreement]')!;
  const consistencyEl = el.querySelector<HTMLParagraphElement>('[data-status-consistency]')!;
  const violationsListEl = el.querySelector<HTMLUListElement>('[data-status-violations]')!;
  const noViolationsEl = el.querySelector<HTMLLIElement>('[data-status-no-violations]')!;
  const completionEl = el.querySelector<HTMLParagraphElement>('[data-status-completion]')!;


  // Event listener to update the view
  el.addEventListener('renderStatus', ((e: CustomEvent<ComparisonStatus>) => {
    const status = e.detail;
    update(status);
  }) as EventListener);

  function update(status: ComparisonStatus) {
    const [completed, total, percent] = status.statusProgress;
    progressEl.textContent = `Progress: ${completed}/${total} pairs compared (${percent.toFixed(1)}%)`;
    agreementEl.textContent = `Agreement (vs ELO): ${status.statusAgreement.toFixed(2)}%`;
    consistencyEl.textContent = `Consistency (Transitivity): ${status.statusConsistency.toFixed(2)}%`;

    // Clear previous violations, keeping the "None detected" item template
    while (violationsListEl.firstChild && violationsListEl.firstChild !== noViolationsEl) {
         violationsListEl.removeChild(violationsListEl.firstChild);
    }


    if (status.statusViolations.length === 0) {
      noViolationsEl.classList.remove('hidden');
    } else {
      noViolationsEl.classList.add('hidden');
      status.statusViolations.forEach(violation => {
        const listItem = document.createElement('li');
        listItem.textContent = formatViolation(violation);
        // Insert before the 'noViolationsEl' to keep it at the end
        violationsListEl.insertBefore(listItem, noViolationsEl);
      });
    }

     // Display completion status
    if(status.statusIsComplete) {
        completionEl.textContent = "Ranking appears complete and consistent!";
        completionEl.classList.remove('hidden');
    } else {
        completionEl.classList.add('hidden');
    }
  }

  function formatViolation(violation: [Option, Option, Option]): string {
    // The backend returns [a, c, b] for the cycle a > b > c > a
    // Let's keep the backend's logic and display it clearly here
    const [a, c, b] = violation;
    // Original logic: a > b, b > c, c > a
    // Display logic: Show the inconsistent preferences found
    return `${a.name} > ${b.name}, ${b.name} > ${c.name}, but ${c.name} > ${a.name}`;
  }
}
