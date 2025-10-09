const { defineConfig } = require('@playwright/test');

module.exports = defineConfig({
  timeout: 120000,
  expect: {
    timeout: 20000,
  },
  use: {
    baseURL: process.env.HS_STARTER_BASE_URL || 'http://127.0.0.1:8080',
    headless: true,
    trace: 'retain-on-failure',
    ignoreHTTPSErrors: true,
  },
  reporter: [['list']],
});
