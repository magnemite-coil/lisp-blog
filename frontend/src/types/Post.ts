/**
 * 投稿情報の型定義
 * バックエンドの posts テーブルに対応
 */
export interface Post {
  id: number;
  user_id: number;
  title: string;
  content: string;
  published: boolean;
  created_at: string; // ISO 8601 形式の日時文字列
  updated_at: string; // ISO 8601 形式の日時文字列
}

/**
 * 投稿作成リクエストの型
 */
export interface CreatePostRequest {
  title: string;
  content: string;
  published?: boolean; // オプショナル: 省略時は下書き
}

/**
 * 投稿更新リクエストの型
 */
export interface UpdatePostRequest {
  title?: string;
  content?: string;
  published?: boolean;
}

/**
 * 投稿一覧取得時のクエリパラメータ
 */
export interface ListPostsQuery {
  user_id?: number;
  published?: boolean;
}
