import axios, { type AxiosError } from 'axios';
import { AppError, type ApiResponse } from '../types/Error';
import { errorMonitor } from '../utils/errorMonitor';

/**
 * Axiosインスタンスの作成
 *
 * このインスタンスを使うことで、すべてのAPIリクエストに
 * 共通の設定（ベースURL、認証情報など）が適用されます。
 */
export const apiClient = axios.create({
  // バックエンドサーバーのURL（環境変数から取得）
  // 開発環境: http://localhost:8080
  // 本番環境: 空文字列（相対URLとして扱われる）
  baseURL: import.meta.env.VITE_API_URL || '',

  // Cookieを含めてリクエストを送る設定
  // これにより、セッション認証が機能します
  withCredentials: true,

  // タイムアウト設定（10秒）
  timeout: 10000,

  // デフォルトのヘッダー
  headers: {
    'Content-Type': 'application/json',
  },
});

/**
 * AxiosErrorをAppErrorに変換
 */
function handleApiError(error: AxiosError): AppError {
  // ネットワークエラー（サーバーに到達できない）
  if (!error.response) {
    if (error.code === 'ECONNABORTED' || error.message.includes('timeout')) {
      return new AppError(
        'NETWORK_TIMEOUT',
        'リクエストがタイムアウトしました'
      );
    }
    return new AppError(
      'NETWORK_ERROR',
      'サーバーに接続できません。ネットワーク環境を確認してください。'
    );
  }

  const response = error.response;
  const data = response.data as ApiResponse<any>;

  // バックエンドから構造化されたエラーが返された場合
  if (data?.error) {
    return new AppError(
      data.error.code,
      data.error.message,
      data.error.field,
      data.error.details,
      response.status
    );
  }

  // 構造化されていないエラー（レガシー形式）
  if (typeof data === 'object' && 'error' in data && typeof data.error === 'string') {
    return new AppError(
      `HTTP_${response.status}`,
      data.error,
      undefined,
      undefined,
      response.status
    );
  }

  // ステータスコードのみからエラーを生成
  switch (response.status) {
    case 401:
      return new AppError('AUTH_REQUIRED', '認証が必要です', undefined, undefined, 401);
    case 403:
      return new AppError('AUTH_PERMISSION_DENIED', '権限がありません', undefined, undefined, 403);
    case 404:
      return new AppError('RESOURCE_NOT_FOUND', 'リソースが見つかりません', undefined, undefined, 404);
    case 500:
      return new AppError('SYSTEM_INTERNAL_ERROR', 'サーバーエラーが発生しました', undefined, undefined, 500);
    case 503:
      return new AppError('SYSTEM_SERVICE_UNAVAILABLE', 'サービスが一時的に利用できません', undefined, undefined, 503);
    default:
      return new AppError(
        `HTTP_${response.status}`,
        'エラーが発生しました',
        undefined,
        undefined,
        response.status
      );
  }
}

/**
 * レスポンスインターセプター
 *
 * すべてのレスポンスを受け取る前に、共通の処理を行います。
 * エラーハンドリングやログ出力などに使用します。
 */
apiClient.interceptors.response.use(
  // 成功時の処理
  (response) => response,

  // エラー時の処理
  (error: AxiosError) => {
    const appError = handleApiError(error);

    // エラーを監視システムに記録
    const errorId = errorMonitor.logError(appError);

    // ログ出力（開発環境のみ）
    if (import.meta.env.DEV) {
      console.error('[API Error]', {
        errorId,
        code: appError.code,
        message: appError.message,
        field: appError.field,
        status: appError.statusCode,
        details: appError.details,
      });
    }

    // 認証エラーの場合、自動的にログインページへリダイレクト
    if (appError.code === 'AUTH_REQUIRED' || appError.code === 'AUTH_SESSION_EXPIRED') {
      // パスが /login でない場合のみリダイレクト
      if (!window.location.pathname.includes('/login')) {
        window.location.href = '/login';
      }
    }

    return Promise.reject(appError);
  }
);
