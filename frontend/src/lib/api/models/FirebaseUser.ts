/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { JsonValue } from './JsonValue';
export type FirebaseUser = {
    audience: Array<string>;
    claims: Record<string, JsonValue>;
    email?: string;
    emailVerified?: boolean;
    issuer: string;
    name?: string;
    picture?: string;
    uid: string;
};

