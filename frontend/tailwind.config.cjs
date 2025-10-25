/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        'tumblr-blue': '#00b8ff',
        'tumblr-blue-dark': '#0090cc',
        'tumblr-navy': '#001935',
        'tumblr-navy-light': '#003d5c',
      },
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}
