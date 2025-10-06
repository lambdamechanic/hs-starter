import { OpenAPI } from '$lib/api/core/OpenAPI';
import { DefaultService } from '$lib/api/services/DefaultService';
import type { SessionExchangeRequest } from '$lib/api/models/SessionExchangeRequest';

const baseUrl = import.meta.env.VITE_API_BASE_URL ?? '';
OpenAPI.BASE = baseUrl;
OpenAPI.WITH_CREDENTIALS = true;
OpenAPI.CREDENTIALS = 'include';
OpenAPI.HEADERS = async () => ({ Accept: 'application/json' });

export { DefaultService };
export type { SessionExchangeRequest };
