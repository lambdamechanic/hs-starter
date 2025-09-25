export interface FirebaseClientConfig {
  apiKey: string;
  authDomain: string;
  projectId: string;
  appId?: string;
  messagingSenderId?: string;
  storageBucket?: string;
  measurementId?: string;
}

export interface FirebaseUserClaims {
  [key: string]: unknown;
}

export interface FirebaseProfile {
  uid: string;
  issuer: string;
  audience: string[];
  email?: string;
  emailVerified?: boolean;
  name?: string;
  picture?: string;
  claims: FirebaseUserClaims;
}

export interface UserProfileResponse {
  firebase: FirebaseProfile;
  allowed: boolean;
}
