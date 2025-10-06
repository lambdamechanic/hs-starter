/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { FirebaseClientConfig } from '../models/FirebaseClientConfig';
import type { HealthStatus } from '../models/HealthStatus';
import type { SessionExchangeRequest } from '../models/SessionExchangeRequest';
import type { SessionExchangeResponse } from '../models/SessionExchangeResponse';
import type { SessionLogoutResponse } from '../models/SessionLogoutResponse';
import type { UserProfileResponse } from '../models/UserProfileResponse';
import type { CancelablePromise } from '../core/CancelablePromise';
import { OpenAPI } from '../core/OpenAPI';
import { request as __request } from '../core/request';
export class DefaultService {
    /**
     * @returns HealthStatus
     * @throws ApiError
     */
    public static getHealth(): CancelablePromise<HealthStatus> {
        return __request(OpenAPI, {
            method: 'GET',
            url: '/health',
        });
    }
    /**
     * @returns FirebaseClientConfig
     * @throws ApiError
     */
    public static getFirebaseConfig(): CancelablePromise<FirebaseClientConfig> {
        return __request(OpenAPI, {
            method: 'GET',
            url: '/firebase/config',
        });
    }
    /**
     * @returns SessionExchangeResponse
     * @throws ApiError
     */
    public static postSessionExchange({
        requestBody,
    }: {
        requestBody?: SessionExchangeRequest,
    }): CancelablePromise<SessionExchangeResponse> {
        return __request(OpenAPI, {
            method: 'POST',
            url: '/session/exchange',
            body: requestBody,
            mediaType: 'application/json;charset=utf-8',
            errors: {
                400: `Invalid \`body\``,
            },
        });
    }
    /**
     * @returns SessionLogoutResponse
     * @throws ApiError
     */
    public static postSessionLogout(): CancelablePromise<SessionLogoutResponse> {
        return __request(OpenAPI, {
            method: 'POST',
            url: '/session/logout',
        });
    }
    /**
     * @returns UserProfileResponse
     * @throws ApiError
     */
    public static getMe(): CancelablePromise<UserProfileResponse> {
        return __request(OpenAPI, {
            method: 'GET',
            url: '/me',
        });
    }
}
