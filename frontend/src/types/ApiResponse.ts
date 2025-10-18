/**
 * API共通レスポンスの型定義
 */

/**
 * 成功レスポンス
 * @template T データの型（User, Post, など）
 */
export interface SuccessResponse<T = unknown> {
  success: true;
  data?: T;
  message?: string;
}

/**
 * エラーレスポンス
 */
export interface ErrorResponse {
  success: false;
  message: string;
  field?: string; // バリデーションエラー時のフィールド名
}

/**
 * APIレスポンス（成功またはエラー）
 */
export type ApiResponse<T = unknown> = SuccessResponse<T> | ErrorResponse;

/**
 * レスポンスがエラーかどうかを判定する型ガード
 */
export function isErrorResponse(
  response: ApiResponse
): response is ErrorResponse {
  return response.success === false;
}
