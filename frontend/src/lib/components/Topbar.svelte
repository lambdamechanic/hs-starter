<script lang="ts">
  import { onMount } from 'svelte';
  import { session, loginWithGoogle, logout, initialiseSession } from '$lib/session';

  let menuOpen = false;
  let menuElement: HTMLDivElement | null = null;
  let menuToggle: HTMLButtonElement | null = null;

  $: state = $session;
  $: profile = state.profile;
  $: loading = state.loading;
  $: displayName =
    profile?.firebase.name ?? profile?.firebase.email ?? profile?.firebase.uid ?? null;
  $: avatarUrl = profile?.firebase.picture ?? null;
  $: avatarInitial = displayName ? displayName.trim().charAt(0).toUpperCase() : 'U';
  $: if (!profile && menuOpen) {
    menuOpen = false;
  }

  onMount(() => {
    initialiseSession();
  });

  function toggleMenu(): void {
    if (!profile) {
      return;
    }
    menuOpen = !menuOpen;
  }

  function closeMenu(): void {
    menuOpen = false;
  }

  function handleWindowClick(event: MouseEvent): void {
    if (!menuOpen) {
      return;
    }
    const target = event.target as Node;
    if (menuElement?.contains(target) || menuToggle?.contains(target)) {
      return;
    }
    closeMenu();
  }

  function handleWindowKeydown(event: KeyboardEvent): void {
    if (event.key === 'Escape' && menuOpen) {
      closeMenu();
    }
  }

  async function handleLogout(): Promise<void> {
    menuOpen = false;
    await logout();
  }
</script>

<svelte:window on:click={handleWindowClick} on:keydown={handleWindowKeydown} />

<nav class="topbar">
  <a class="brand" href="/">
    <span class="brand-mark">hs</span>
    <span class="brand-name">hs-starter</span>
  </a>
  <div class="nav-actions">
    {#if loading}
      <span class="muted small">Checking session…</span>
    {:else if profile}
      <div class="nav-user">
        <button
          class="avatar-button"
          type="button"
          on:click={toggleMenu}
          aria-haspopup="true"
          aria-expanded={menuOpen}
          aria-label="Account menu"
          bind:this={menuToggle}
        >
          {#if avatarUrl}
            <img src={avatarUrl} alt={`Avatar for ${displayName ?? 'user'}`} />
          {:else}
            <span class="avatar-fallback">{avatarInitial}</span>
          {/if}
        </button>
        {#if menuOpen}
          <div class="dropdown" role="menu" bind:this={menuElement}>
            <a href="/profile" role="menuitem">Profile</a>
            <div class="dropdown-divider" />
            <button type="button" on:click={handleLogout} role="menuitem">Sign out</button>
          </div>
        {/if}
      </div>
    {:else}
      <button class="primary" on:click={loginWithGoogle}>Sign in with Google</button>
    {/if}
  </div>
</nav>

<style>
  .topbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    background: rgba(15, 23, 42, 0.75);
    border: 1px solid rgba(148, 163, 184, 0.12);
    border-radius: 16px;
    padding: 1rem 1.5rem;
    box-shadow: 0 18px 40px rgba(15, 23, 42, 0.45);
    backdrop-filter: blur(18px);
  }

  .brand {
    display: inline-flex;
    align-items: center;
    gap: 0.75rem;
    text-decoration: none;
    color: inherit;
    font-weight: 700;
    font-size: 1.05rem;
    letter-spacing: 0.04em;
  }

  .brand-mark {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
    border-radius: 12px;
    background: linear-gradient(135deg, #2563eb, #7c3aed);
    font-weight: 700;
    text-transform: uppercase;
  }

  .brand-name {
    font-size: 1.1rem;
    font-weight: 700;
  }

  .nav-actions {
    display: flex;
    align-items: center;
    gap: 1rem;
  }

  .nav-user {
    position: relative;
    display: flex;
    align-items: center;
  }

  .avatar-button {
    background: rgba(148, 163, 184, 0.12);
    border: 1px solid transparent;
    border-radius: 999px;
    padding: 0.25rem;
    cursor: pointer;
    transition: border-color 0.15s ease, transform 0.1s ease;
    display: inline-flex;
    align-items: center;
    justify-content: center;
  }

  .avatar-button:hover {
    border-color: rgba(148, 163, 184, 0.4);
    transform: translateY(-1px);
  }

  .avatar-button img,
  .avatar-button .avatar-fallback {
    width: 40px;
    height: 40px;
    border-radius: 50%;
    object-fit: cover;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    font-weight: 600;
    background: rgba(59, 130, 246, 0.2);
    color: #cbd5f5;
    text-transform: uppercase;
  }

  .dropdown {
    position: absolute;
    top: calc(100% + 0.5rem);
    right: 0;
    background: rgba(15, 23, 42, 0.96);
    border: 1px solid rgba(148, 163, 184, 0.14);
    border-radius: 12px;
    padding: 0.5rem;
    min-width: 180px;
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
    box-shadow: 0 20px 45px rgba(15, 23, 42, 0.55);
    backdrop-filter: blur(16px);
    z-index: 10;
  }

  .dropdown a,
  .dropdown button {
    background: none;
    border: none;
    color: #e2e8f0;
    font-size: 0.95rem;
    font-weight: 500;
    text-align: left;
    padding: 0.6rem 0.75rem;
    border-radius: 8px;
    cursor: pointer;
    text-decoration: none;
  }

  .dropdown a:hover,
  .dropdown button:hover {
    background: rgba(59, 130, 246, 0.2);
  }

  .dropdown-divider {
    height: 1px;
    background: rgba(148, 163, 184, 0.2);
    margin: 0.15rem 0;
  }

  .primary {
    border: none;
    padding: 0.65rem 1.6rem;
    border-radius: 999px;
    font-size: 0.95rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.1s ease, box-shadow 0.1s ease;
    background: linear-gradient(120deg, #2563eb, #8b5cf6);
    color: white;
  }

  .primary:hover {
    transform: translateY(-1px);
    box-shadow: 0 10px 22px rgba(59, 130, 246, 0.2);
  }

  .muted {
    color: #94a3b8;
  }

  .small {
    font-size: 0.9rem;
  }

  @media (max-width: 640px) {
    .topbar {
      padding: 0.85rem 1rem;
    }

    .dropdown {
      min-width: 160px;
    }
  }
</style>
