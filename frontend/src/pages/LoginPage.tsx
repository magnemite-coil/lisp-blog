import { useState, useEffect, useRef } from 'react';
import { useNavigate, Link } from 'react-router-dom';
import { useForm } from 'react-hook-form';
import { useAuth } from '../hooks/useAuth';
import type { LoginRequest } from '../types/User';

/**
 * ログインページ
 */
export function LoginPage() {
  const navigate = useNavigate();
  const { login, isAuthenticated, isLoading } = useAuth();
  const [error, setError] = useState<string>('');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const timeoutIdRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  /**
   * すでにログイン済みの場合はダッシュボードへリダイレクト
   */
  useEffect(() => {
    if (!isLoading && isAuthenticated) {
      navigate('/dashboard');
    }
  }, [isLoading, isAuthenticated, navigate]);

  /**
   * クリーンアップ: コンポーネントアンマウント時にタイマーをクリア
   */
  useEffect(() => {
    return () => {
      if (timeoutIdRef.current) {
        clearTimeout(timeoutIdRef.current);
      }
    };
  }, []);

  // React Hook Formのセットアップ
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<LoginRequest>({
    mode: 'onBlur', // バリデーションをblurイベントで実行
  });

  /**
   * ログインフォーム送信処理
   */
  const onSubmit = async (data: LoginRequest) => {
    try {
      setError('');
      setIsSubmitting(true);

      // ログイン実行
      await login(data);

      // トーストを表示してから少し待ってリダイレクト
      timeoutIdRef.current = setTimeout(() => {
        navigate('/dashboard');
      }, 500);
    } catch (err: any) {
      // エラーメッセージを表示
      const message =
        err.response?.data?.message || 'ログインに失敗しました。';
      setError(message);
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-tumblr-navy to-tumblr-navy-light flex items-center justify-center p-4">
      <div className="max-w-md w-full">
        {/* カード */}
        <div className="bg-white/95 backdrop-blur-sm rounded-2xl shadow-2xl p-8 sm:p-10">
          {/* ブランド名 */}
          <h1 className="text-5xl font-bold text-center mb-2 text-gray-900 tracking-tight">
            MyBlog
          </h1>
          <p className="text-center text-gray-600 mb-8">
            アカウントにログイン
          </p>

          {/* ログインフォーム */}
          <form className="space-y-5" onSubmit={handleSubmit(onSubmit)}>
            {/* エラーメッセージ */}
            {error && (
              <div className="rounded-lg bg-red-50 p-4 border border-red-200">
                <p className="text-sm text-red-800">{error}</p>
              </div>
            )}

            {/* ユーザー名入力 */}
            <div>
              <label htmlFor="username" className="sr-only">
                ユーザー名
              </label>
              <input
                id="username"
                type="text"
                autoComplete="username"
                className={`appearance-none relative block w-full px-4 py-3 border ${
                  errors.username ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-lg focus:outline-none focus:ring-2 focus:ring-tumblr-blue focus:border-transparent transition-all sm:text-sm`}
                placeholder="ユーザー名"
                {...register('username', {
                  required: 'ユーザー名を入力してください',
                  minLength: {
                    value: 3,
                    message: 'ユーザー名は3文字以上で入力してください',
                  },
                })}
              />
              {errors.username && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.username.message}
                </p>
              )}
            </div>

            {/* パスワード入力 */}
            <div>
              <label htmlFor="password" className="sr-only">
                パスワード
              </label>
              <input
                id="password"
                type="password"
                autoComplete="current-password"
                className={`appearance-none relative block w-full px-4 py-3 border ${
                  errors.password ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-lg focus:outline-none focus:ring-2 focus:ring-tumblr-blue focus:border-transparent transition-all sm:text-sm`}
                placeholder="パスワード"
                {...register('password', {
                  required: 'パスワードを入力してください',
                  minLength: {
                    value: 8,
                    message: 'パスワードは8文字以上で入力してください',
                  },
                })}
              />
              {errors.password && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.password.message}
                </p>
              )}
            </div>

            {/* ログインボタン */}
            <div className="pt-2">
              <button
                type="submit"
                disabled={isSubmitting}
                className={`group relative w-full flex justify-center py-3 px-4 border border-transparent text-base font-semibold rounded-lg text-white transition-all ${
                  isSubmitting
                    ? 'bg-tumblr-blue/50 cursor-not-allowed'
                    : 'bg-tumblr-blue hover:bg-tumblr-blue-dark focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-tumblr-blue shadow-lg hover:shadow-xl'
                }`}
              >
                {isSubmitting ? 'ログイン中...' : 'ログイン'}
              </button>
            </div>
          </form>

          {/* 区切り線 */}
          <div className="relative my-8">
            <div className="absolute inset-0 flex items-center">
              <div className="w-full border-t border-gray-300"></div>
            </div>
            <div className="relative flex justify-center text-sm">
              <span className="px-3 bg-white text-gray-500">または</span>
            </div>
          </div>

          {/* OAuth用プレースホルダー（Phase 3で実装） */}
          <div className="text-center">
            <p className="text-sm text-gray-500 mb-4">
              Google認証は Phase 3 で実装予定
            </p>
          </div>

          {/* リンク */}
          <div className="mt-6 text-center">
            <p className="text-sm text-gray-600">
              アカウントをお持ちでない方は{' '}
              <Link
                to="/register"
                className="font-semibold text-tumblr-blue hover:text-tumblr-blue-dark transition-colors"
              >
                新規登録
              </Link>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
