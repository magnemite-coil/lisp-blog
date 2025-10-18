import { apiClient } from './client';
import type {
  Post,
  CreatePostRequest,
  UpdatePostRequest,
  ListPostsQuery,
  PostResponse,
  PostsResponse,
} from '../types/Post';

/**
 * 投稿API関数
 *
 * バックエンドの /api/posts/* エンドポイントと通信します。
 */

/**
 * 投稿一覧を取得
 *
 * @param query - フィルタ条件（user_id, published）
 * @returns 投稿の配列
 */
export async function listPosts(query?: ListPostsQuery): Promise<Post[]> {
  const response = await apiClient.get<PostsResponse>('/api/posts', {
    params: query,
  });
  return response.data.data;
}

/**
 * 投稿詳細を取得
 *
 * @param id - 投稿ID
 * @returns 投稿情報
 */
export async function getPost(id: number): Promise<Post> {
  const response = await apiClient.get<PostResponse>(`/api/posts/${id}`);
  return response.data.data;
}

/**
 * 投稿を作成
 *
 * @param data - タイトル、本文、公開状態
 * @returns 作成された投稿情報
 */
export async function createPost(data: CreatePostRequest): Promise<Post> {
  const response = await apiClient.post<PostResponse>('/api/posts', data);
  return response.data.data;
}

/**
 * 投稿を更新
 *
 * @param id - 投稿ID
 * @param data - 更新内容
 * @returns 更新された投稿情報
 */
export async function updatePost(
  id: number,
  data: UpdatePostRequest
): Promise<Post> {
  const response = await apiClient.put<PostResponse>(`/api/posts/${id}`, data);
  return response.data.data;
}

/**
 * 投稿を削除
 *
 * @param id - 投稿ID
 */
export async function deletePost(id: number): Promise<void> {
  await apiClient.delete(`/api/posts/${id}`);
}

/**
 * 下書きを公開
 *
 * @param id - 投稿ID
 * @returns 更新された投稿情報
 */
export async function publishPost(id: number): Promise<Post> {
  const response = await apiClient.put<PostResponse>(`/api/posts/${id}/publish`);
  return response.data.data;
}

/**
 * 公開記事を下書きに戻す
 *
 * @param id - 投稿ID
 * @returns 更新された投稿情報
 */
export async function unpublishPost(id: number): Promise<Post> {
  const response = await apiClient.put<PostResponse>(`/api/posts/${id}/unpublish`);
  return response.data.data;
}
