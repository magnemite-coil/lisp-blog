// Highlight.js initialization for Tokyo Night theme
// Automatically highlights all code blocks on the page

document.addEventListener('DOMContentLoaded', (event) => {
  // Highlight all <pre><code> blocks
  document.querySelectorAll('pre code').forEach((block) => {
    hljs.highlightElement(block);
  });

  console.log('Syntax highlighting initialized');
});
