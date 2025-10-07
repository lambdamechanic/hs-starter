<script lang="ts">
  import { onMount } from 'svelte';
  import Topbar from '$lib/components/Topbar.svelte';
  import { session, initialiseSession, loginWithGoogle, refreshProfile } from '$lib/session';

  $: state = $session;
  $: loading = state.loading;
  $: profile = state.profile;
  $: firebaseError = state.firebaseError;
  $: configError = state.configError;
  $: refreshingProfile = state.refreshingProfile;
  $: displayName =
    profile?.firebase.name ?? profile?.firebase.email ?? profile?.firebase.uid ?? null;
  $: avatarUrl = profile?.firebase.picture ?? null;
  $: avatarInitial = displayName ? displayName.trim().charAt(0).toUpperCase() : 'U';

  onMount(() => {
    initialiseSession();
  });
</script>

<svelte:head>
  <title>hs-starter</title>
</svelte:head>

<main class="page">
  <Topbar />

  <section class="hero">
    <div class="hero-copy">
      <p class="eyebrow">Full-stack starter</p>
      <h1>Build production-ready Haskell services faster</h1>
      <p>
        hs-starter pairs a Servant backend, Squeal migrations, and a SvelteKit frontend with
        Firebase authentication, observability hooks, and deploy-ready Docker tooling.
      </p>
      <div class="cta">
        {#if profile}
          <a class="secondary link" href="/profile">View profile</a>
          <button class="secondary" on:click={refreshProfile} disabled={refreshingProfile}>
            {#if refreshingProfile}Refreshing…{:else}Refresh profile{/if}
          </button>
        {:else}
          <button class="primary" on:click={loginWithGoogle} disabled={loading}>
            {#if loading}Checking session…{:else}Sign in with Google{/if}
          </button>
        {/if}
      </div>
    </div>
    <div class="hero-card">
      {#if loading}
        <section class="card status-card">
          <p>Checking your session…</p>
        </section>
      {:else if profile}
        <section class="card profile-card">
          <header>
            {#if avatarUrl}
              <img class="avatar" src={avatarUrl} alt={`Avatar for ${displayName ?? 'user'}`} />
            {:else}
              <div class="avatar fallback">{avatarInitial}</div>
            {/if}
            <div>
              <h2>{displayName}</h2>
              {#if profile.firebase.email}
                <p class="muted small">{profile.firebase.email}</p>
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
        </section>
      {:else}
        <section class="card login-card">
          <h2>Authenticate with Firebase</h2>
          <p class="muted small">
            Use your Google identity to create a signed backend session and explore the starter
            stack.
          </p>
          <button class="primary" on:click={loginWithGoogle}>Sign in with Google</button>
        </section>
      {/if}
    </div>
  </section>

  <section class="features">
    <article>
      <h3>Typed API surface</h3>
      <p>
        Servant routes, generated OpenAPI clients, and Roboservant smoke tests keep the backend and
        SPA in sync.
      </p>
    </article>
    <article>
      <h3>Database confidence</h3>
      <p>
        pgroll migrations plus Squeal queries give you reversible schema changes with compile-time
        safety.
      </p>
    </article>
    <article>
      <h3>Deployment ready</h3>
      <p>
        Docker, Dokku, tracing hooks, and health endpoints ship in the box so you can promote to
        production quickly.
      </p>
    </article>
  </section>

  {#if firebaseError}
    <p class="error">{firebaseError}</p>
  {/if}
  {#if configError}
    <p class="error">{configError}</p>
  {/if}
</main>

<style>
  :global(body) {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    margin: 0;
    background: radial-gradient(circle at top left, #1f2937, #0f172a 45%, #050816 100%);
    color: #f8fafc;
  }

  main.page {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
    gap: 3rem;
    padding: 3rem clamp(1.5rem, 5vw, 4rem);
  }

  .hero {
    display: flex;
    flex-wrap: wrap;
    gap: 2.5rem;
    align-items: stretch;
  }

  .hero-copy {
    flex: 1 1 320px;
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
    max-width: 520px;
  }

  .hero-copy h1 {
    font-size: clamp(2.2rem, 4vw, 3rem);
    margin: 0;
  }

  .hero-copy p {
    margin: 0;
    line-height: 1.6;
    color: #cbd5f5;
  }

  .eyebrow {
    text-transform: uppercase;
    letter-spacing: 0.28em;
    font-size: 0.75rem;
    color: rgba(148, 163, 184, 0.7);
  }

  .cta {
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
  }

  .hero-card {
    flex: 1 1 320px;
    display: flex;
    justify-content: center;
  }

  .card {
    width: min(420px, 100%);
    background: rgba(15, 23, 42, 0.8);
    border: 1px solid rgba(148, 163, 184, 0.18);
    border-radius: 20px;
    padding: 1.75rem;
    display: flex;
    flex-direction: column;
    gap: 1.15rem;
    box-shadow: 0 28px 60px rgba(2, 6, 23, 0.55);
  }

  .profile-card header {
    display: flex;
    align-items: center;
    gap: 1rem;
  }

  .profile-card header h2 {
    margin: 0;
    font-size: 1.25rem;
  }

  .avatar {
    width: 56px;
    height: 56px;
    border-radius: 50%;
    object-fit: cover;
    border: 2px solid rgba(148, 163, 184, 0.4);
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 600;
    text-transform: uppercase;
  }

  .avatar.fallback {
    background: rgba(59, 130, 246, 0.25);
    color: #dbeafe;
  }

  dl {
    display: grid;
    grid-template-columns: auto 1fr;
    gap: 0.45rem 1rem;
    margin: 0;
  }

  dt {
    color: rgba(148, 163, 184, 0.75);
    font-weight: 600;
  }

  dd {
    margin: 0;
    font-family: "Fira Mono", ui-monospace, SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
    font-size: 0.9rem;
  }

  .allowed {
    color: #34d399;
  }

  .denied {
    color: #f87171;
  }

  button,
  .link {
    border: none;
    padding: 0.65rem 1.6rem;
    border-radius: 999px;
    font-size: 0.95rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.1s ease, box-shadow 0.1s ease;
  }

  .link {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    text-decoration: none;
  }

  button.primary {
    background: linear-gradient(120deg, #2563eb, #8b5cf6);
    color: white;
  }

  button.secondary,
  .secondary.link {
    background: rgba(148, 163, 184, 0.18);
    color: #e2e8f0;
    border: 1px solid rgba(148, 163, 184, 0.25);
  }

  button:disabled {
    opacity: 0.6;
    cursor: wait;
  }

  button:not(:disabled):hover,
  .secondary.link:hover {
    transform: translateY(-1px);
    box-shadow: 0 10px 22px rgba(59, 130, 246, 0.2);
  }

  .features {
    display: grid;
    gap: 1.5rem;
    grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  }

  .features article {
    background: rgba(15, 23, 42, 0.68);
    border: 1px solid rgba(148, 163, 184, 0.12);
    border-radius: 16px;
    padding: 1.5rem;
    box-shadow: 0 18px 34px rgba(2, 6, 23, 0.4);
  }

  .features h3 {
    margin: 0 0 0.75rem;
    font-size: 1.1rem;
  }

  .features p {
    margin: 0;
    color: #cbd5f5;
    line-height: 1.5;
  }

  .muted {
    color: #94a3b8;
  }

  .small {
    font-size: 0.9rem;
  }

  .error {
    background: rgba(239, 68, 68, 0.18);
    border: 1px solid rgba(248, 113, 113, 0.35);
    color: #fecaca;
    border-radius: 12px;
    padding: 1rem;
    margin: 0;
  }

  @media (max-width: 640px) {
    main.page {
      padding: 2rem 1.25rem 3rem;
    }
  }
</style>
