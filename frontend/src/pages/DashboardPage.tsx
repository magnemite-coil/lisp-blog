import { useState, useEffect } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { useAuth } from '../hooks/useAuth';
import { Header } from '../components/Header';
import * as postsApi from '../api/posts';
import type { Post } from '../types/Post';

/**
 * ダッシュボードページ
 *
 * ログイン中のユーザーの投稿一覧を表示します。
 */
export function DashboardPage() {
  const { user, isAuthenticated, isLoading: authLoading } = useAuth();
  const navigate = useNavigate();
  const [posts, setPosts] = useState<Post[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string>('');

  /**
   * 未ログインの場合はログインページへリダイレクト
   * 認証状態の確認中（authLoading）は何もしない
   */
  useEffect(() => {
    if (!authLoading && !isAuthenticated) {
      navigate('/login');
    }
  }, [authLoading, isAuthenticated, navigate]);

  /**
   * 投稿一覧を取得
   */
  useEffect(() => {
    if (user) {
      fetchPosts();
    }
  }, [user]);

  /**
   * 投稿一覧を取得する関数
   */
  const fetchPosts = async () => {
    try {
      setError('');
      setIsLoading(true);
      // 自分の投稿のみ取得
      const data = await postsApi.listPosts({ user_id: user!.id });
      setPosts(data);
    } catch (err: any) {
      console.error('投稿の取得に失敗:', err);
      setError('投稿の取得に失敗しました。');
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * 投稿削除処理
   */
  const handleDelete = async (id: number) => {
    if (!window.confirm('この投稿を削除してもよろしいですか？')) {
      return;
    }

    try {
      await postsApi.deletePost(id);
      // 投稿一覧を再取得
      await fetchPosts();
    } catch (err: any) {
      console.error('削除に失敗:', err);
      alert('削除に失敗しました。');
    }
  };

  /**
   * 公開/非公開切り替え
   */
  const handleTogglePublish = async (post: Post) => {
    try {
      if (post.status === 'published') {
        await postsApi.unpublishPost(post.id);
      } else {
        await postsApi.publishPost(post.id);
      }
      // 投稿一覧を再取得
      await fetchPosts();
    } catch (err: any) {
      console.error('公開状態の変更に失敗:', err);
      alert('公開状態の変更に失敗しました。');
    }
  };

  /**
   * 投稿を公開済みと下書きに分類
   */
  const publishedPosts = posts.filter((post) => post.status === 'published');
  const draftPosts = posts.filter((post) => post.status === 'draft');

  return (
    <div className="min-h-screen bg-gray-50">
      <Header />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* ページヘッダー */}
        <div className="flex justify-between items-center mb-8">
          <h1 className="text-3xl font-bold text-gray-900">ダッシュボード</h1>
          <Link
            to="/posts/new"
            className="px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
          >
            新規投稿
          </Link>
        </div>

        {/* エラーメッセージ */}
        {error && (
          <div className="mb-4 rounded-md bg-red-50 p-4">
            <p className="text-sm text-red-800">{error}</p>
          </div>
        )}

        {/* ローディング */}
        {isLoading ? (
          <div className="text-center py-12">
            <p className="text-gray-500">読み込み中...</p>
          </div>
        ) : (
          <>
            {/* 公開済み投稿 */}
            <section className="mb-12">
              <h2 className="text-2xl font-bold text-gray-900 mb-4">
                公開済み投稿 ({publishedPosts.length})
              </h2>
              {publishedPosts.length === 0 ? (
                <p className="text-gray-500">公開済みの投稿がありません。</p>
              ) : (
                <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
                  {publishedPosts.map((post) => (
                    <PostCard
                      key={post.id}
                      post={post}
                      onDelete={handleDelete}
                      onTogglePublish={handleTogglePublish}
                    />
                  ))}
                </div>
              )}
            </section>

            {/* 下書き投稿 */}
            <section>
              <h2 className="text-2xl font-bold text-gray-900 mb-4">
                下書き ({draftPosts.length})
              </h2>
              {draftPosts.length === 0 ? (
                <p className="text-gray-500">下書きがありません。</p>
              ) : (
                <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
                  {draftPosts.map((post) => (
                    <PostCard
                      key={post.id}
                      post={post}
                      onDelete={handleDelete}
                      onTogglePublish={handleTogglePublish}
                    />
                  ))}
                </div>
              )}
            </section>
          </>
        )}
      </main>
    </div>
  );
}

/**
 * 投稿カードコンポーネント
 */
interface PostCardProps {
  post: Post;
  onDelete: (id: number) => void;
  onTogglePublish: (post: Post) => void;
}

function PostCard({ post, onDelete, onTogglePublish }: PostCardProps) {
  return (
    <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
      {/* タイトル */}
      <h3 className="text-xl font-semibold text-gray-900 mb-2">
        {post.title}
      </h3>

      {/* 本文プレビュー */}
      <p className="text-gray-600 mb-4 line-clamp-3">
        {post.content.substring(0, 100)}
        {post.content.length > 100 && '...'}
      </p>

      {/* メタ情報 */}
      <div className="text-sm text-gray-500 mb-4">
        <p>作成日: {new Date(post.created_at).toLocaleDateString('ja-JP')}</p>
        <p>更新日: {new Date(post.updated_at).toLocaleDateString('ja-JP')}</p>
      </div>

      {/* アクションボタン */}
      <div className="flex space-x-2">
        {/* 編集ボタン */}
        <Link
          to={`/posts/${post.id}/edit`}
          className="flex-1 px-3 py-2 text-sm font-medium text-blue-700 bg-blue-100 rounded-md hover:bg-blue-200 text-center"
        >
          編集
        </Link>

        {/* 公開/非公開ボタン */}
        <button
          onClick={() => onTogglePublish(post)}
          className={`flex-1 px-3 py-2 text-sm font-medium rounded-md ${
            post.status === 'published'
              ? 'text-yellow-700 bg-yellow-100 hover:bg-yellow-200'
              : 'text-green-700 bg-green-100 hover:bg-green-200'
          }`}
        >
          {post.status === 'published' ? '非公開' : '公開'}
        </button>

        {/* 削除ボタン */}
        <button
          onClick={() => onDelete(post.id)}
          className="flex-1 px-3 py-2 text-sm font-medium text-red-700 bg-red-100 rounded-md hover:bg-red-200"
        >
          削除
        </button>
      </div>
    </div>
  );
}
