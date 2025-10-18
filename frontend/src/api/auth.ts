import { apiClient } from './client';
import type { User, LoginRequest, RegisterRequest, LoginResponse } from '../types/User';

/**
 * 認証API関数
 *
 * バックエンドの /api/auth/* エンドポイントと通信します。
 */

/**
 * ユーザー登録
 *
 * @param data - ユーザー名とパスワード
 * @returns 作成されたユーザー情報
 */
export async function register(data: RegisterRequest): Promise<User> {
  const response = await apiClient.post<LoginResponse>('/api/auth/register', data);
  return response.data.data;
}

/**
 * ログイン
 *
 * @param data - ユーザー名とパスワード
 * @returns ログインしたユーザー情報
 */
export async function login(data: LoginRequest): Promise<User> {
  const response = await apiClient.post<LoginResponse>('/api/auth/login', data);
  return response.data.data;
}

/**
 * ログアウト
 */
export async function logout(): Promise<void> {
  await apiClient.post('/api/auth/logout');
}

/**
 * 現在のユーザー情報を取得
 *
 * @returns ログイン中のユーザー情報（未ログインの場合はnull）
 */
export async function getCurrentUser(): Promise<User | null> {
  try {
    const response = await apiClient.get<LoginResponse>('/api/auth/me');
    return response.data.data;
  } catch (error) {
    // 401エラー（未ログイン）の場合はnullを返す
    console.log('getCurrentUser failed:', error);
    return null;
  }
}
