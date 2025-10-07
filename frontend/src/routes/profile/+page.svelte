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
  <title>Profile • hs-starter</title>
</svelte:head>

<main class="page">
  <Topbar />

  <section class="content">
    {#if loading}
      <section class="card status">
        <h1>Loading your profile…</h1>
        <p class="muted small">Fetching session details from the backend.</p>
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
            <h1>{displayName}</h1>
            {#if profile.firebase.email}
              <p class="muted">{profile.firebase.email}</p>
            {/if}
          </div>
        </header>
        <dl>
          <div>
            <dt>Firebase UID</dt>
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
          <button class="secondary" on:click={refreshProfile} disabled={refreshingProfile}>
            {#if refreshingProfile}Refreshing…{:else}Refresh profile{/if}
          </button>
        </footer>
      </section>
    {:else}
      <section class="card empty">
        <h1>Sign in to view your profile</h1>
        <p class="muted small">
          You need an active Firebase-backed session to access profile details.
        </p>
        <button class="primary" on:click={loginWithGoogle}>Sign in with Google</button>
      </section>
    {/if}
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

  .content {
    display: flex;
    justify-content: center;
  }

  .card {
    width: min(560px, 100%);
    background: rgba(15, 23, 42, 0.82);
    border: 1px solid rgba(148, 163, 184, 0.18);
    border-radius: 24px;
    padding: 2rem;
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
    box-shadow: 0 32px 70px rgba(2, 6, 23, 0.55);
  }

  .status h1,
  .empty h1 {
    margin: 0;
    font-size: 1.8rem;
  }

  .profile-card header {
    display: flex;
    align-items: center;
    gap: 1.5rem;
  }

  .profile-card header h1 {
    margin: 0;
    font-size: clamp(1.6rem, 3vw, 2.1rem);
  }

  .avatar {
    width: 72px;
    height: 72px;
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
    gap: 0.6rem 1.25rem;
    margin: 0;
  }

  dt {
    color: rgba(148, 163, 184, 0.75);
    font-weight: 600;
  }

  dd {
    margin: 0;
    font-family: "Fira Mono", ui-monospace, SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
    font-size: 0.95rem;
  }

  footer {
    display: flex;
    justify-content: flex-end;
  }

  button {
    border: none;
    padding: 0.75rem 1.8rem;
    border-radius: 999px;
    font-size: 1rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.1s ease, box-shadow 0.1s ease;
  }

  button.primary {
    background: linear-gradient(120deg, #2563eb, #8b5cf6);
    color: white;
  }

  button.secondary {
    background: rgba(148, 163, 184, 0.18);
    color: #e2e8f0;
    border: 1px solid rgba(148, 163, 184, 0.25);
  }

  button:disabled {
    opacity: 0.6;
    cursor: wait;
  }

  button:not(:disabled):hover {
    transform: translateY(-1px);
    box-shadow: 0 10px 22px rgba(59, 130, 246, 0.2);
  }

  .allowed {
    color: #34d399;
  }

  .denied {
    color: #f87171;
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

    .card {
      padding: 1.75rem;
    }
  }
</style>
