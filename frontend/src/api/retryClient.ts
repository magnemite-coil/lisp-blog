/**
 * リトライ機能付きAPIクライアント
 */
import type { AxiosRequestConfig, AxiosResponse } from 'axios';
import { apiClient } from './client';
import { withRetry, DEFAULT_RETRY_CONFIG, type RetryConfig } from '../utils/retry';

/**
 * リトライ機能付きGETリクエスト
 */
export async function getWithRetry<T = any>(
  url: string,
  config?: AxiosRequestConfig,
  retryConfig?: Partial<RetryConfig>
): Promise<AxiosResponse<T>> {
  return withRetry(
    () => apiClient.get<T>(url, config),
    { ...DEFAULT_RETRY_CONFIG, ...retryConfig },
    (retryCount) => {
      console.log(`[API Retry] GET ${url} - Attempt ${retryCount}`);
    }
  );
}

/**
 * リトライ機能付きPOSTリクエスト
 * 注意: POSTはべき等でない場合があるため、リトライは慎重に使用
 */
export async function postWithRetry<T = any>(
  url: string,
  data?: any,
  config?: AxiosRequestConfig,
  retryConfig?: Partial<RetryConfig>
): Promise<AxiosResponse<T>> {
  return withRetry(
    () => apiClient.post<T>(url, data, config),
    { ...DEFAULT_RETRY_CONFIG, ...retryConfig },
    (retryCount) => {
      console.log(`[API Retry] POST ${url} - Attempt ${retryCount}`);
    }
  );
}

/**
 * リトライ機能付きPUTリクエスト
 */
export async function putWithRetry<T = any>(
  url: string,
  data?: any,
  config?: AxiosRequestConfig,
  retryConfig?: Partial<RetryConfig>
): Promise<AxiosResponse<T>> {
  return withRetry(
    () => apiClient.put<T>(url, data, config),
    { ...DEFAULT_RETRY_CONFIG, ...retryConfig },
    (retryCount) => {
      console.log(`[API Retry] PUT ${url} - Attempt ${retryCount}`);
    }
  );
}

/**
 * リトライ機能付きDELETEリクエスト
 */
export async function deleteWithRetry<T = any>(
  url: string,
  config?: AxiosRequestConfig,
  retryConfig?: Partial<RetryConfig>
): Promise<AxiosResponse<T>> {
  return withRetry(
    () => apiClient.delete<T>(url, config),
    { ...DEFAULT_RETRY_CONFIG, ...retryConfig },
    (retryCount) => {
      console.log(`[API Retry] DELETE ${url} - Attempt ${retryCount}`);
    }
  );
}
