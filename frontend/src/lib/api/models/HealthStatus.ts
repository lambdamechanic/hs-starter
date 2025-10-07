/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { HealthCheckReport } from './HealthCheckReport';
import type { UTCTime } from './UTCTime';
export type HealthStatus = {
    checks: Record<string, HealthCheckReport>;
    service: string;
    status: string;
    timestamp: UTCTime;
    version: string;
};

