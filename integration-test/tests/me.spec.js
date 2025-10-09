const { test, expect } = require('@playwright/test');

const CONFIG_ENDPOINT = '/firebase/config';

test('frontend fetches firebase configuration on load', async ({ page }) => {
  const configResponsePromise = page.waitForResponse((response) => {
    return response.url().endsWith(CONFIG_ENDPOINT) && response.request().method() === 'GET';
  });

  await page.goto('/');

  const configResponse = await configResponsePromise;
  expect(configResponse.ok(), `expected ${CONFIG_ENDPOINT} to respond OK`).toBeTruthy();

  const payload = await configResponse.json();
  expect(payload.projectId).toBeTruthy();
  expect(payload.apiKey).toBeTruthy();
});
