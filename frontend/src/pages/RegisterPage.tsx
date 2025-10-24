import { useState, useEffect, useRef } from 'react';
import { useNavigate, Link } from 'react-router-dom';
import { useForm } from 'react-hook-form';
import { useAuth } from '../hooks/useAuth';
import type { RegisterRequest } from '../types/User';

/**
 * ユーザー登録ページ
 */
export function RegisterPage() {
  const navigate = useNavigate();
  const { register: registerUser, isAuthenticated, isLoading } = useAuth();
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
    watch,
  } = useForm<RegisterRequest & { passwordConfirm: string }>({
    mode: 'onBlur', // バリデーションをblurイベントで実行
  });

  // パスワード確認用にパスワードを監視
  const password = watch('password');

  /**
   * 登録フォーム送信処理
   */
  const onSubmit = async (data: RegisterRequest & { passwordConfirm: string }) => {
    try {
      setError('');
      setIsSubmitting(true);

      // パスワード確認フィールドを除外
      const { passwordConfirm, ...registerData } = data;

      // ユーザー登録実行
      await registerUser(registerData);

      // 成功したらダッシュボードへリダイレクト
      navigate('/dashboard');
    } catch (err: any) {
      // エラーメッセージを表示
      const message =
        err.response?.data?.message || 'ユーザー登録に失敗しました。';
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
            新しいアカウントを作成
          </p>

          {/* 登録フォーム */}
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
                placeholder="ユーザー名（3-50文字）"
                {...register('username', {
                  required: 'ユーザー名を入力してください',
                  minLength: {
                    value: 3,
                    message: 'ユーザー名は3文字以上で入力してください',
                  },
                  maxLength: {
                    value: 50,
                    message: 'ユーザー名は50文字以内で入力してください',
                  },
                  pattern: {
                    value: /^[a-zA-Z0-9_-]+$/,
                    message: 'ユーザー名は英数字、アンダースコア、ハイフンのみ使用できます',
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
                autoComplete="new-password"
                className={`appearance-none relative block w-full px-4 py-3 border ${
                  errors.password ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-lg focus:outline-none focus:ring-2 focus:ring-tumblr-blue focus:border-transparent transition-all sm:text-sm`}
                placeholder="パスワード（8-100文字）"
                {...register('password', {
                  required: 'パスワードを入力してください',
                  minLength: {
                    value: 8,
                    message: 'パスワードは8文字以上で入力してください',
                  },
                  maxLength: {
                    value: 100,
                    message: 'パスワードは100文字以内で入力してください',
                  },
                })}
              />
              {errors.password && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.password.message}
                </p>
              )}
            </div>

            {/* パスワード確認入力 */}
            <div>
              <label htmlFor="passwordConfirm" className="sr-only">
                パスワード（確認）
              </label>
              <input
                id="passwordConfirm"
                type="password"
                autoComplete="new-password"
                className={`appearance-none relative block w-full px-4 py-3 border ${
                  errors.passwordConfirm ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-lg focus:outline-none focus:ring-2 focus:ring-tumblr-blue focus:border-transparent transition-all sm:text-sm`}
                placeholder="パスワード（確認）"
                {...register('passwordConfirm', {
                  required: 'パスワード（確認）を入力してください',
                  validate: (value) =>
                    value === password || 'パスワードが一致しません',
                })}
              />
              {errors.passwordConfirm && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.passwordConfirm.message}
                </p>
              )}
            </div>

            {/* 登録ボタン */}
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
                {isSubmitting ? '登録中...' : '登録'}
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
              すでにアカウントをお持ちの方は{' '}
              <Link
                to="/login"
                className="font-semibold text-tumblr-blue hover:text-tumblr-blue-dark transition-colors"
              >
                ログイン
              </Link>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
