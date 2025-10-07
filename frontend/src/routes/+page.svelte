<script lang="ts">
  import { onMount } from 'svelte';
  import Topbar from '$lib/components/Topbar.svelte';
  import { session, initialiseSession } from '$lib/session';

  $: state = $session;
  $: loading = state.loading;
  $: profile = state.profile;
  $: firebaseError = state.firebaseError;
  $: configError = state.configError;
  $: displayName =
    profile?.firebase.name ?? profile?.firebase.email ?? profile?.firebase.uid ?? null;

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
          <a class="secondary link" href="/profile">Go to your profile</a>
        {:else}
          <p class="muted small cta-note">Use the sign-in button in the top right to get started.</p>
        {/if}
      </div>
    </div>
    <div class="hero-card">
      {#if loading}
        <section class="card status-card">
          <p>Checking your session…</p>
        </section>
      {:else}
        <section class="card info-card">
          <h2>What’s included?</h2>
          <ul>
            <li>Typed Servant API with generated clients</li>
            <li>Squeal migrations and pgroll for reversible schema changes</li>
            <li>SvelteKit SPA with Firebase auth integration</li>
            <li>Dokku-ready Dockerfile, observability hooks, and health checks</li>
          </ul>
          {#if !profile}
            <p class="muted small">Sign in to explore the authenticated experience.</p>
          {:else}
            <a class="secondary link" href="/profile">Manage your profile</a>
          {/if}
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

  .cta-note {
    margin: 0;
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

  .info-card ul {
    margin: 0;
    padding-left: 1.25rem;
    display: flex;
    flex-direction: column;
    gap: 0.65rem;
    color: #cbd5f5;
  }

  .info-card li {
    line-height: 1.5;
  }

  .info-card h2 {
    margin: 0;
  }

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

  .secondary.link {
    background: rgba(148, 163, 184, 0.18);
    color: #e2e8f0;
    border: 1px solid rgba(148, 163, 184, 0.25);
  }

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
