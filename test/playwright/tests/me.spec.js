const { test, expect } = require('@playwright/test');

const GOOGLE_HOST = 'googleapis.com';

function isGooglePost(response) {
  try {
    const request = response.request();
    return request.method() === 'POST' && response.url().includes(GOOGLE_HOST);
  } catch (err) {
    return false;
  }
}

async function waitForGooglePost(context, timeoutMs) {
  return context.waitForEvent('response', {
    predicate: (response) => isGooglePost(response),
    timeout: timeoutMs,
  });
}

test('fetching /me triggers Firebase login flow', async ({ context, page }) => {
  page.on('console', (msg) => {
    console.log(`[console][${msg.type()}] ${msg.text()}`);
  });
  page.on('pageerror', (err) => {
    console.log(`[pageerror] ${err}`);
  });
  context.on('request', (request) => {
    const url = request.url();
    if (url.includes('google') || url.includes('firebase')) {
      console.log(`[request] ${request.method()} ${url}`);
    }
  });
  context.on('response', (response) => {
    const url = response.url();
    if (url.includes('google') || url.includes('firebase')) {
      console.log(`[response] ${response.request().method()} ${response.status()} ${url}`);
    }
  });

  const timeoutMs = Number(process.env.HS_STARTER_PLAYWRIGHT_TIMEOUT_MS || 60000);
  const googleResponsePromise = waitForGooglePost(context, timeoutMs);

  await page.goto('/me');

  const googleResponse = await googleResponsePromise;
  const statusText = `${googleResponse.status()} ${googleResponse.statusText()}`;

  expect(googleResponse.ok(), `expected successful POST to ${GOOGLE_HOST}, saw ${statusText}`).toBeTruthy();
});
