/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./index.html",
    "./src/*.{js,ts,jsx,tsx,purs}",
  ],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
}

