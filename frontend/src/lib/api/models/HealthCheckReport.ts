/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { JsonValue } from './JsonValue';
import type { UTCTime } from './UTCTime';
export type HealthCheckReport = {
    details: JsonValue;
    durationMs: number;
    observedAt: UTCTime;
    status: string;
};

