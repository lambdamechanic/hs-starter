<script lang="ts">
  import { onMount } from 'svelte';
  import { getApps, initializeApp, type FirebaseApp } from 'firebase/app';
  import {
    getAuth,
    GoogleAuthProvider,
    signInWithPopup,
    signOut,
    type Auth
  } from 'firebase/auth';
  import type { FirebaseClientConfig, UserProfileResponse } from '$lib/types';

  let firebaseApp: FirebaseApp | null = null;
  let auth: Auth | null = null;
  let firebaseReady = false;

  let loading = true;
  let profile: UserProfileResponse | null = null;
  let firebaseError: string | null = null;
  let configError: string | null = null;
  let refreshingProfile = false;

  let configPromise: Promise<FirebaseClientConfig> | null = null;

  const provider = new GoogleAuthProvider();
  provider.setCustomParameters({ prompt: 'select_account' });

  async function fetchFirebaseConfig(): Promise<FirebaseClientConfig> {
    if (!configPromise) {
      configPromise = fetch('/firebase/config', { credentials: 'same-origin' })
        .then(async (response) => {
          if (!response.ok) {
            throw new Error('Failed to load Firebase configuration');
          }
          return (await response.json()) as FirebaseClientConfig;
        })
        .catch((error) => {
          configError = error instanceof Error ? error.message : String(error);
          throw error;
        });
    }
    return configPromise;
  }

  async function ensureFirebase(): Promise<void> {
    if (firebaseReady && firebaseApp && auth) {
      return;
    }

    const config = await fetchFirebaseConfig();
    const existing = getApps();
    firebaseApp = existing.length > 0 ? existing[0] : initializeApp(config, 'hs-starter');
    auth = getAuth(firebaseApp);
    firebaseReady = true;
  }

  async function fetchProfile(): Promise<void> {
    refreshingProfile = true;
    try {
      const response = await fetch('/me', { credentials: 'same-origin' });
      if (response.status === 401) {
        profile = null;
        return;
      }
      if (!response.ok) {
        const body = await response.text();
        throw new Error(body || 'Failed to load profile');
      }
      profile = (await response.json()) as UserProfileResponse;
    } catch (error) {
      firebaseError = error instanceof Error ? error.message : String(error);
    } finally {
      refreshingProfile = false;
    }
  }

  async function login(): Promise<void> {
    firebaseError = null;
    try {
      await ensureFirebase();
      if (!auth) {
        throw new Error('Firebase auth not initialised');
      }
      const result = await signInWithPopup(auth, provider);
      const idToken = await result.user.getIdToken(true);
      const params = new URLSearchParams(window.location.search);
      const returnTo = params.get('return_to') ?? '/';

      const response = await fetch('/session/exchange', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        credentials: 'same-origin',
        body: JSON.stringify({ idToken, return_to: returnTo })
      });

      if (!response.ok) {
        const detail = await response.json().catch(() => ({}));
        const message = detail?.error?.message ?? 'Session exchange failed';
        throw new Error(message);
      }

      await fetchProfile();
    } catch (error) {
      firebaseError = error instanceof Error ? error.message : String(error);
    }
  }

  async function logout(): Promise<void> {
    firebaseError = null;
    try {
      await ensureFirebase();
      if (auth) {
        await signOut(auth);
      }
      const response = await fetch('/session/logout', {
        method: 'POST',
        credentials: 'same-origin'
      });
      if (!response.ok) {
        const detail = await response.json().catch(() => ({}));
        const message = detail?.error?.message ?? 'Failed to clear session';
        throw new Error(message);
      }
      profile = null;
    } catch (error) {
      firebaseError = error instanceof Error ? error.message : String(error);
    }
  }

  async function refresh(): Promise<void> {
    firebaseError = null;
    await fetchProfile();
  }

  onMount(async () => {
    try {
      await fetchProfile();
      await ensureFirebase();
    } finally {
      loading = false;
    }
  });
</script>

<svelte:head>
  <title>hs-starter</title>
</svelte:head>

<main class="container">
  <section class="panel">
    <h1>hs-starter</h1>
    <p class="muted">Firebase-authenticated profile served by the Haskell backend.</p>

    {#if loading}
      <p>Loading…</p>
    {:else}
      {#if profile}
        <section class="card">
          <header>
            {#if profile.firebase.picture}
              <img class="avatar" src={profile.firebase.picture} alt="user avatar" />
            {/if}
            <div>
              <h2>{profile.firebase.name ?? profile.firebase.email ?? profile.firebase.uid}</h2>
              {#if profile.firebase.email}
                <p class="muted">{profile.firebase.email}</p>
              {/if}
            </div>
          </header>
          <dl>
            <div>
              <dt>UID</dt>
              <dd>{profile.firebase.uid}</dd>
            </div>
            <div>
              <dt>Issuer</dt>
              <dd>{profile.firebase.issuer}</dd>
            </div>
            <div>
              <dt>Allowed</dt>
              <dd class:allowed={profile.allowed} class:denied={!profile.allowed}>
                {profile.allowed ? 'Yes' : 'No'}
              </dd>
            </div>
            {#if profile.firebase.audience?.length}
              <div>
                <dt>Audience</dt>
                <dd>{profile.firebase.audience.join(', ')}</dd>
              </div>
            {/if}
          </dl>
          <footer>
            <button class="secondary" on:click={refresh} disabled={refreshingProfile}>
              {#if refreshingProfile}Refreshing…{:else}Refresh profile{/if}
            </button>
            <button class="secondary" on:click={logout}>Log out</button>
          </footer>
        </section>
      {:else}
        <section class="card">
          <p>Sign in with Google to create a session.</p>
          <button class="primary" on:click={login}>Sign in with Google</button>
        </section>
      {/if}
    {/if}

    {#if firebaseError}
      <p class="error">{firebaseError}</p>
    {/if}
    {#if configError}
      <p class="error">{configError}</p>
    {/if}
  </section>
</main>

<style>
  :global(body) {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    margin: 0;
    background: #111827;
    color: #f1f5f9;
  }

  main.container {
    min-height: 100vh;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 2rem;
  }

  section.panel {
    width: min(700px, 100%);
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
    background: rgba(15, 23, 42, 0.6);
    border: 1px solid rgba(148, 163, 184, 0.1);
    border-radius: 16px;
    padding: 2.5rem;
    box-shadow: 0 20px 60px rgba(15, 23, 42, 0.4);
  }

  section.panel > h1 {
    margin: 0;
  }

  .muted {
    color: #94a3b8;
    margin: 0;
  }

  section.card {
    background: rgba(30, 41, 59, 0.9);
    border-radius: 12px;
    padding: 1.75rem;
    display: flex;
    flex-direction: column;
    gap: 1rem;
    border: 1px solid rgba(148, 163, 184, 0.15);
  }

  section.card header {
    display: flex;
    align-items: center;
    gap: 1rem;
  }

  section.card header h2 {
    margin: 0;
    font-size: 1.25rem;
  }

  .avatar {
    width: 56px;
    height: 56px;
    border-radius: 50%;
    object-fit: cover;
    border: 2px solid rgba(148, 163, 184, 0.4);
  }

  dl {
    display: grid;
    grid-template-columns: auto 1fr;
    gap: 0.5rem 1rem;
    margin: 0;
  }

  dt {
    color: #94a3b8;
    font-weight: 500;
  }

  dd {
    margin: 0;
    font-family: 'Fira Mono', ui-monospace, SFMono-Regular, SFMono-Regular, Consolas, 'Liberation Mono', Menlo, monospace;
  }

  .allowed {
    color: #34d399;
  }

  .denied {
    color: #f87171;
  }

  footer {
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
  }

  button {
    border: none;
    padding: 0.65rem 1.5rem;
    border-radius: 999px;
    font-size: 0.95rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.1s ease, box-shadow 0.1s ease;
  }

  button.primary {
    background: linear-gradient(120deg, #2563eb, #8b5cf6);
    color: white;
  }

  button.secondary {
    background: rgba(148, 163, 184, 0.2);
    color: #e2e8f0;
  }

  button:disabled {
    opacity: 0.6;
    cursor: wait;
  }

  button:not(:disabled):hover {
    transform: translateY(-1px);
    box-shadow: 0 8px 20px rgba(59, 130, 246, 0.15);
  }

  .error {
    background: rgba(239, 68, 68, 0.15);
    border: 1px solid rgba(248, 113, 113, 0.4);
    color: #fecaca;
    border-radius: 12px;
    padding: 1rem;
  }
</style>
