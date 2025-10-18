/**
 * 投稿情報の型定義
 * バックエンドの posts テーブルに対応
 */
export interface Post {
  id: number;
  user_id: number;
  title: string;
  content: string;
  status: 'draft' | 'published'; // バックエンドは status フィールドを使用
  created_at: string; // ISO 8601 形式の日時文字列
  updated_at: string; // ISO 8601 形式の日時文字列
  username?: string; // バックエンドが付与するユーザー名（オプショナル）
}

/**
 * 投稿作成リクエストの型
 */
export interface CreatePostRequest {
  title: string;
  content: string;
  status?: 'draft' | 'published'; // オプショナル: 省略時は draft
}

/**
 * 投稿更新リクエストの型
 */
export interface UpdatePostRequest {
  title?: string;
  content?: string;
  status?: 'draft' | 'published';
}

/**
 * 投稿一覧取得時のクエリパラメータ
 */
export interface ListPostsQuery {
  user_id?: number;
  published?: boolean;
}

/**
 * API レスポンスの型（単一投稿）
 */
export interface PostResponse {
  success: boolean;
  data: Post;
  message?: string;
  error?: string;
}

/**
 * API レスポンスの型（投稿配列）
 */
export interface PostsResponse {
  success: boolean;
  data: Post[];
  message?: string;
  error?: string;
}
