import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useForm } from 'react-hook-form';
import ReactMarkdown from 'react-markdown';
import { useAuth } from '../hooks/useAuth';
import { Header } from '../components/Header';
import * as postsApi from '../api/posts';
import type { CreatePostRequest } from '../types/Post';

/**
 * 投稿作成ページ
 */
export function CreatePostPage() {
  const { isAuthenticated, isLoading } = useAuth();
  const navigate = useNavigate();
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string>('');
  const [showPreview, setShowPreview] = useState(false);

  // React Hook Formのセットアップ
  const {
    register,
    handleSubmit,
    formState: { errors },
    watch,
  } = useForm<CreatePostRequest>();

  // リアルタイムプレビュー用に本文を監視
  const content = watch('content', '');

  /**
   * 未ログインの場合はログインページへリダイレクト
   * 認証確認中は待機
   */
  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <p className="text-gray-500">読み込み中...</p>
      </div>
    );
  }

  if (!isAuthenticated) {
    navigate('/login');
    return null;
  }

  /**
   * 投稿作成処理
   */
  const onSubmit = async (data: CreatePostRequest) => {
    try {
      setError('');
      setIsSubmitting(true);

      // 投稿作成
      await postsApi.createPost(data);

      // 成功したらダッシュボードへリダイレクト
      navigate('/dashboard');
    } catch (err: any) {
      console.error('投稿の作成に失敗:', err);
      const message =
        err.response?.data?.message || '投稿の作成に失敗しました。';
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

  return (
    <div className="min-h-screen bg-gray-50">
      <Header />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* ページヘッダー */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">新規投稿</h1>
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
                  required: 'タイトルを入力してください',
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
                    placeholder="Markdown形式で本文を入力&#10;&#10;例:&#10;# 見出し1&#10;## 見出し2&#10;**太字** *斜体*&#10;- リスト1&#10;- リスト2"
                    {...register('content', {
                      required: '本文を入力してください',
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
                {isSubmitting ? '投稿中...' : '公開する'}
              </button>
            </div>
          </form>
        </div>

        {/* Markdownヘルプ */}
        <div className="mt-8 bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold text-gray-900 mb-4">
            Markdown記法ガイド
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
            <div>
              <h3 className="font-medium text-gray-900 mb-2">見出し</h3>
              <pre className="bg-gray-100 p-2 rounded">
{`# 見出し1
## 見出し2
### 見出し3`}
              </pre>
            </div>
            <div>
              <h3 className="font-medium text-gray-900 mb-2">強調</h3>
              <pre className="bg-gray-100 p-2 rounded">
{`**太字**
*斜体*
~~取り消し線~~`}
              </pre>
            </div>
            <div>
              <h3 className="font-medium text-gray-900 mb-2">リスト</h3>
              <pre className="bg-gray-100 p-2 rounded">
{`- 項目1
- 項目2
  - サブ項目`}
              </pre>
            </div>
            <div>
              <h3 className="font-medium text-gray-900 mb-2">リンク</h3>
              <pre className="bg-gray-100 p-2 rounded">
{`[リンクテキスト](URL)
![画像](画像URL)`}
              </pre>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
