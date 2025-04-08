import type { Option, OptionId } from './api.ts';

/**
 * Mounts the comparison view component.
 * Handles displaying the next pair and dispatching the user's choice.
 * @param el The container element for the comparison view.
 */
export function ComparisonView(el: HTMLElement) {
  let currentPair: [Option, Option] | null = null;

  // Initial structure
  el.innerHTML = `
    <div data-comparison-content class="flex justify-center items-center space-x-4 p-4 min-h-[100px]">
      <!-- Content will be dynamically added here -->
    </div>
  `;

  const contentEl = el.querySelector<HTMLElement>('[data-comparison-content]')!;

  // Event listener to update the view when new data arrives
  el.addEventListener('renderComparison', ((e: CustomEvent<[Option, Option] | null>) => {
    currentPair = e.detail;
    update();
  }) as EventListener);

  function update() {
    // Clear previous content
    contentEl.innerHTML = '';

    if (currentPair) {
      const [opt1, opt2] = currentPair;

      // Button 1
      const button1 = document.createElement('button');
      button1.textContent = opt1.name;
      button1.className = 'option-button px-6 py-3 bg-blue-500 text-white rounded hover:bg-blue-600 transition duration-150 ease-in-out';
      button1.addEventListener('click', () => handleChoice(opt1.id, opt2.id));

      // "OR" text
      const orText = document.createElement('span');
      orText.textContent = 'OR';
      orText.className = 'text-gray-500 font-semibold';

      // Button 2
      const button2 = document.createElement('button');
      button2.textContent = opt2.name;
      button2.className = 'option-button px-6 py-3 bg-green-500 text-white rounded hover:bg-green-600 transition duration-150 ease-in-out';
      button2.addEventListener('click', () => handleChoice(opt2.id, opt1.id));

      contentEl.append(button1, orText, button2);
    } else {
      // Display completion message
      const message = document.createElement('p');
      message.className = 'text-green-600 font-bold text-center';
      // Check status for a more specific message later if needed
      message.textContent = 'No more pairs to compare or ranking is complete!';
      contentEl.appendChild(message);
    }
  }

  function handleChoice(winnerId: OptionId, loserId: OptionId) {
    console.log(`Comparison choice made: Winner=${winnerId}, Loser=${loserId}`);
    // Dispatch event for the App component to handle the API call
    el.dispatchEvent(new CustomEvent('comparisonMade', {
      detail: { winnerId, loserId },
      bubbles: true, // Allow event to bubble up to the App component
    }));
  }

  // Initial render (will likely show loading or empty state until 'renderComparison' is fired)
  // No initial pair data, so let App trigger the first render
  // update();
}
