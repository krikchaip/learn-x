import { defineConfig } from "vite";
import path from "path";
import react from "@vitejs/plugin-react";

const { compilerOptions } = require("./tsconfig.json");

// https://vitejs.dev/config
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "~": path.resolve(__dirname, compilerOptions.baseUrl),
    },
  },
});
