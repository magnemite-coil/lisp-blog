import { useState, useEffect } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { useForm } from 'react-hook-form';
import ReactMarkdown from 'react-markdown';
import { useAuth } from '../hooks/useAuth';
import { Header } from '../components/Header';
import * as postsApi from '../api/posts';
import type { UpdatePostRequest } from '../types/Post';

/**
 * 投稿編集ページ
 */
export function EditPostPage() {
  const { id } = useParams<{ id: string }>();
  const { isAuthenticated, isLoading: authLoading } = useAuth();
  const navigate = useNavigate();
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string>('');
  const [showPreview, setShowPreview] = useState(false);

  // React Hook Formのセットアップ
  const {
    register,
    handleSubmit,
    formState: { errors },
    watch,
    setValue,
  } = useForm<UpdatePostRequest>();

  // リアルタイムプレビュー用に本文を監視
  const content = watch('content', '');

  /**
   * 未ログインの場合はログインページへリダイレクト
   * 認証確認中は何もしない
   */
  useEffect(() => {
    if (!authLoading && !isAuthenticated) {
      navigate('/login');
    }
  }, [authLoading, isAuthenticated, navigate]);

  /**
   * 投稿データを取得
   */
  useEffect(() => {
    if (id) {
      fetchPost(parseInt(id, 10));
    }
  }, [id]);

  /**
   * 投稿データ取得関数
   */
  const fetchPost = async (postId: number) => {
    try {
      setError('');
      setIsLoading(true);
      const post = await postsApi.getPost(postId);

      // フォームに値をセット
      setValue('title', post.title);
      setValue('content', post.content);
      setValue('status', post.status);
    } catch (err: any) {
      console.error('投稿の取得に失敗:', err);
      setError('投稿の取得に失敗しました。');
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * 投稿更新処理
   */
  const onSubmit = async (data: UpdatePostRequest) => {
    if (!id) return;

    try {
      setError('');
      setIsSubmitting(true);

      // 投稿更新
      await postsApi.updatePost(parseInt(id, 10), data);

      // 成功したらダッシュボードへリダイレクト
      navigate('/dashboard');
    } catch (err: any) {
      console.error('投稿の更新に失敗:', err);
      const message =
        err.response?.data?.message || '投稿の更新に失敗しました。';
      setError(message);
    } finally {
      setIsSubmitting(false);
    }
  };

  /**
   * 下書きとして保存
   */
  const handleSaveDraft = () => {
    handleSubmit((data) => onSubmit({ ...data, status: 'draft' }))();
  };

  /**
   * 公開する
   */
  const handlePublish = () => {
    handleSubmit((data) => onSubmit({ ...data, status: 'published' }))();
  };

  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50">
        <Header />
        <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="text-center py-12">
            <p className="text-gray-500">読み込み中...</p>
          </div>
        </main>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <Header />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* ページヘッダー */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">投稿編集</h1>
        </div>

        {/* エラーメッセージ */}
        {error && (
          <div className="mb-4 rounded-md bg-red-50 p-4">
            <p className="text-sm text-red-800">{error}</p>
          </div>
        )}

        {/* フォーム */}
        <div className="bg-white shadow rounded-lg p-6">
          <form onSubmit={(e) => e.preventDefault()}>
            {/* タイトル入力 */}
            <div className="mb-6">
              <label
                htmlFor="title"
                className="block text-sm font-medium text-gray-700 mb-2"
              >
                タイトル
              </label>
              <input
                id="title"
                type="text"
                className={`w-full px-3 py-2 border ${
                  errors.title ? 'border-red-300' : 'border-gray-300'
                } rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent`}
                placeholder="投稿のタイトルを入力"
                {...register('title', {
                  maxLength: {
                    value: 255,
                    message: 'タイトルは255文字以内で入力してください',
                  },
                })}
              />
              {errors.title && (
                <p className="mt-1 text-sm text-red-600">
                  {errors.title.message}
                </p>
              )}
            </div>

            {/* プレビュー切り替えボタン */}
            <div className="mb-2 flex space-x-2">
              <button
                type="button"
                onClick={() => setShowPreview(false)}
                className={`px-4 py-2 text-sm font-medium rounded-md ${
                  !showPreview
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-200 text-gray-700 hover:bg-gray-300'
                }`}
              >
                編集
              </button>
              <button
                type="button"
                onClick={() => setShowPreview(true)}
                className={`px-4 py-2 text-sm font-medium rounded-md ${
                  showPreview
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-200 text-gray-700 hover:bg-gray-300'
                }`}
              >
                プレビュー
              </button>
            </div>

            {/* 本文入力 / プレビュー */}
            <div className="mb-6">
              <label
                htmlFor="content"
                className="block text-sm font-medium text-gray-700 mb-2"
              >
                本文（Markdown形式）
              </label>

              {!showPreview ? (
                // 編集モード
                <>
                  <textarea
                    id="content"
                    rows={20}
                    className={`w-full px-3 py-2 border ${
                      errors.content ? 'border-red-300' : 'border-gray-300'
                    } rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent font-mono text-sm`}
                    placeholder="Markdown形式で本文を入力"
                    {...register('content', {
                      maxLength: {
                        value: 100000,
                        message: '本文は100,000文字以内で入力してください',
                      },
                    })}
                  />
                  {errors.content && (
                    <p className="mt-1 text-sm text-red-600">
                      {errors.content.message}
                    </p>
                  )}
                </>
              ) : (
                // プレビューモード
                <div className="w-full min-h-[500px] px-3 py-2 border border-gray-300 rounded-md bg-white">
                  <div className="prose max-w-none">
                    <ReactMarkdown>{content || '*プレビューがありません*'}</ReactMarkdown>
                  </div>
                </div>
              )}
            </div>

            {/* 送信ボタン */}
            <div className="flex justify-end space-x-4">
              <button
                type="button"
                onClick={() => navigate('/dashboard')}
                className="px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
              >
                キャンセル
              </button>
              <button
                type="button"
                onClick={handleSaveDraft}
                disabled={isSubmitting}
                className={`px-4 py-2 text-sm font-medium text-gray-700 bg-gray-200 rounded-md hover:bg-gray-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-500 ${
                  isSubmitting ? 'opacity-50 cursor-not-allowed' : ''
                }`}
              >
                下書き保存
              </button>
              <button
                type="button"
                onClick={handlePublish}
                disabled={isSubmitting}
                className={`px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 ${
                  isSubmitting ? 'opacity-50 cursor-not-allowed' : ''
                }`}
              >
                {isSubmitting ? '更新中...' : '更新する'}
              </button>
            </div>
          </form>
        </div>
      </main>
    </div>
  );
}
