/**
 * リトライユーティリティ
 */

/**
 * リトライ設定
 */
export interface RetryConfig {
  maxRetries: number; // 最大リトライ回数
  initialDelay: number; // 初回リトライまでの遅延（ミリ秒）
  maxDelay: number; // 最大遅延時間（ミリ秒）
  backoffFactor: number; // バックオフ係数（2なら指数バックオフ）
}

/**
 * デフォルトのリトライ設定
 */
export const DEFAULT_RETRY_CONFIG: RetryConfig = {
  maxRetries: 3,
  initialDelay: 1000, // 1秒
  maxDelay: 10000, // 10秒
  backoffFactor: 2, // 1秒 → 2秒 → 4秒 → 8秒
};

/**
 * 指数バックオフで遅延時間を計算
 */
export function calculateDelay(retryCount: number, config: RetryConfig): number {
  const delay = config.initialDelay * Math.pow(config.backoffFactor, retryCount);
  return Math.min(delay, config.maxDelay);
}

/**
 * 指定時間待機する
 */
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * リトライ可能なエラーかどうかを判定
 */
export function isRetryableError(error: any): boolean {
  // ネットワークエラー
  if (!error.response) {
    return true;
  }

  // リトライ可能なHTTPステータスコード
  const retryableStatusCodes = [
    408, // Request Timeout
    429, // Too Many Requests
    500, // Internal Server Error
    502, // Bad Gateway
    503, // Service Unavailable
    504, // Gateway Timeout
  ];

  return retryableStatusCodes.includes(error.response?.status);
}

/**
 * 関数を指定回数リトライする
 *
 * @param fn リトライする関数
 * @param config リトライ設定
 * @param onRetry リトライ時のコールバック
 * @returns 関数の実行結果
 */
export async function withRetry<T>(
  fn: () => Promise<T>,
  config: RetryConfig = DEFAULT_RETRY_CONFIG,
  onRetry?: (retryCount: number, error: any) => void
): Promise<T> {
  let lastError: any;

  for (let retryCount = 0; retryCount <= config.maxRetries; retryCount++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;

      // 最後のリトライで失敗した場合はエラーをスロー
      if (retryCount === config.maxRetries) {
        throw error;
      }

      // リトライ不可能なエラーの場合は即座にスロー
      if (!isRetryableError(error)) {
        throw error;
      }

      // リトライコールバックを呼び出し
      if (onRetry) {
        onRetry(retryCount + 1, error);
      }

      // 指数バックオフで待機
      const delay = calculateDelay(retryCount, config);
      console.log(`[Retry] Attempt ${retryCount + 1}/${config.maxRetries}, waiting ${delay}ms...`);
      await sleep(delay);
    }
  }

  throw lastError;
}
