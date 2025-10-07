import { writable, get } from 'svelte/store';
import { getApps, initializeApp, type FirebaseApp } from 'firebase/app';
import {
  getAuth,
  GoogleAuthProvider,
  signInWithPopup,
  signOut,
  type Auth
} from 'firebase/auth';
import { DefaultService, type SessionExchangeRequest } from '$lib/apiClient';
import { ApiError } from '$lib/api/core/ApiError';
import type { FirebaseClientConfig, UserProfileResponse } from '$lib/api';

type SessionState = {
  loading: boolean;
  profile: UserProfileResponse | null;
  firebaseError: string | null;
  configError: string | null;
  refreshingProfile: boolean;
};

const defaultState: SessionState = {
  loading: true,
  profile: null,
  firebaseError: null,
  configError: null,
  refreshingProfile: false
};

const session = writable<SessionState>({ ...defaultState });

let initialised = false;
let firebaseApp: FirebaseApp | null = null;
let auth: Auth | null = null;
let configPromise: Promise<FirebaseClientConfig> | null = null;

const provider = new GoogleAuthProvider();
provider.setCustomParameters({ prompt: 'select_account' });

function setState(partial: Partial<SessionState>): void {
  session.update((current) => ({ ...current, ...partial }));
}

async function fetchFirebaseConfig(): Promise<FirebaseClientConfig> {
  if (!configPromise) {
    configPromise = DefaultService.getFirebaseConfig()
      .then((config) => {
        setState({ configError: null });
        return config;
      })
      .catch((error) => {
        const message =
          error instanceof ApiError
            ? error.body?.error?.message ?? error.message
            : error instanceof Error
              ? error.message
              : String(error);
        setState({ configError: message });
        configPromise = null;
        throw error;
      });
  }
  return configPromise;
}

async function ensureFirebase(): Promise<void> {
  if (firebaseApp && auth) {
    return;
  }

  const config = await fetchFirebaseConfig();
  const existing = getApps();
  firebaseApp = existing.length > 0 ? existing[0] : initializeApp(config, 'hs-starter');
  auth = getAuth(firebaseApp);
}

async function fetchProfile(): Promise<void> {
  setState({ refreshingProfile: true, firebaseError: null });
  try {
    const data = await DefaultService.getMe();
    setState({ profile: data });
  } catch (error) {
    if (error instanceof ApiError) {
      if (error.status === 401) {
        setState({ profile: null });
        return;
      }
      const message = error.body?.error?.message ?? error.message;
      setState({ firebaseError: message });
    } else {
      const message = error instanceof Error ? error.message : String(error);
      setState({ firebaseError: message });
    }
  } finally {
    setState({ refreshingProfile: false });
  }
}

export async function initialiseSession(): Promise<void> {
  if (initialised && !get(session).loading) {
    return;
  }
  initialised = true;
  setState({ loading: true });
  try {
    await fetchProfile();
    await ensureFirebase();
  } finally {
    setState({ loading: false });
  }
}

export async function loginWithGoogle(): Promise<void> {
  setState({ firebaseError: null });
  try {
    await ensureFirebase();
    if (!auth) {
      throw new Error('Firebase auth not initialised');
    }
    const result = await signInWithPopup(auth, provider);
    const idToken = await result.user.getIdToken(true);
    const params = new URLSearchParams(window.location.search);
    const returnTo = params.get('return_to') ?? undefined;
    const payload: SessionExchangeRequest = { idToken, returnTo };
    await DefaultService.postSessionExchange({ requestBody: payload });
    await fetchProfile();
  } catch (error) {
    const message =
      error instanceof ApiError
        ? error.body?.error?.message ?? error.message
        : error instanceof Error
          ? error.message
          : String(error);
    setState({ firebaseError: message });
  }
}

export async function logout(): Promise<void> {
  setState({ firebaseError: null });
  try {
    await ensureFirebase();
    if (auth) {
      await signOut(auth);
    }
    await DefaultService.postSessionLogout();
    setState({ profile: null });
  } catch (error) {
    const message =
      error instanceof ApiError
        ? error.body?.error?.message ?? error.message
        : error instanceof Error
          ? error.message
          : String(error);
    setState({ firebaseError: message });
  }
}

export async function refreshProfile(): Promise<void> {
  await fetchProfile();
}

export { session };
export type { SessionState };
