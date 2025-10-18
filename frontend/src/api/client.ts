import axios from 'axios';

/**
 * Axiosインスタンスの作成
 *
 * このインスタンスを使うことで、すべてのAPIリクエストに
 * 共通の設定（ベースURL、認証情報など）が適用されます。
 */
export const apiClient = axios.create({
  // バックエンドサーバーのURL
  baseURL: 'http://localhost:5000',

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
 * レスポンスインターセプター
 *
 * すべてのレスポンスを受け取る前に、共通の処理を行います。
 * エラーハンドリングやログ出力などに使用します。
 */
apiClient.interceptors.response.use(
  // 成功時の処理
  (response) => {
    return response;
  },

  // エラー時の処理
  (error) => {
    // ネットワークエラー
    if (!error.response) {
      console.error('ネットワークエラー: バックエンドに接続できません');
      return Promise.reject({
        message: 'サーバーに接続できません。ネットワーク環境を確認してください。',
      });
    }

    // 401 Unauthorized（認証エラー）
    if (error.response.status === 401) {
      console.error('認証エラー: ログインが必要です');
      // ログインページへリダイレクト（後で実装）
    }

    // 500 Internal Server Error（サーバーエラー）
    if (error.response.status === 500) {
      console.error('サーバーエラー:', error.response.data);
    }

    return Promise.reject(error);
  }
);
