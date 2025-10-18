/**
 * ユーザー情報の型定義
 * バックエンドの users テーブルに対応
 */
export interface User {
  id: number;
  username: string;
  created_at: string; // ISO 8601 形式の日時文字列
}

/**
 * ログインリクエストの型
 */
export interface LoginRequest {
  username: string;
  password: string;
}

/**
 * ユーザー登録リクエストの型
 */
export interface RegisterRequest {
  username: string;
  password: string;
}

/**
 * ログインレスポンスの型
 * バックエンドは {success: true, data: {...}} 形式で返す
 */
export interface LoginResponse {
  success: boolean;
  data: User;
  message?: string;
  error?: string;
}
