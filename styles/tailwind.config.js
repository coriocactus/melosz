/** @type {import('tailwindcss').Config} */
module.exports = {
  content: {
    relative: true,
    files: ['./app/**/*.hs'],
    extract: {
      hs: (content) => {
        const classMatches = content.match(/class_\s*"([^"]*)"/g) || [];
        return classMatches.map(match => {
          const extracted = match.match(/class_\s*"([^"]*)"/);
          return extracted ? extracted[1].split(' ') : [];
        }).flat();
      }
    }
  },
}
