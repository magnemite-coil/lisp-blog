import { useState, useEffect } from 'react';
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

  /**
   * すでにログイン済みの場合はダッシュボードへリダイレクト
   */
  useEffect(() => {
    if (!isLoading && isAuthenticated) {
      navigate('/dashboard');
    }
  }, [isLoading, isAuthenticated, navigate]);

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
    <div className="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
      <div className="max-w-md w-full space-y-8">
        {/* ヘッダー */}
        <div>
          <h2 className="mt-6 text-center text-3xl font-extrabold text-gray-900">
            新規登録
          </h2>
          <p className="mt-2 text-center text-sm text-gray-600">
            または{' '}
            <Link
              to="/login"
              className="font-medium text-blue-600 hover:text-blue-500"
            >
              ログイン
            </Link>
          </p>
        </div>

        {/* 登録フォーム */}
        <form className="mt-8 space-y-6" onSubmit={handleSubmit(onSubmit)}>
          {/* エラーメッセージ */}
          {error && (
            <div className="rounded-md bg-red-50 p-4">
              <p className="text-sm text-red-800">{error}</p>
            </div>
          )}

          <div className="rounded-md shadow-sm space-y-4">
            {/* ユーザー名入力 */}
            <div>
              <label htmlFor="username" className="sr-only">
                ユーザー名
              </label>
              <input
                id="username"
                type="text"
                autoComplete="username"
                className={`appearance-none relative block w-full px-3 py-2 border ${
                  errors.username ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-md focus:outline-none focus:ring-blue-500 focus:border-blue-500 focus:z-10 sm:text-sm`}
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
                <p className="mt-1 text-sm text-red-600">
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
                className={`appearance-none relative block w-full px-3 py-2 border ${
                  errors.password ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-md focus:outline-none focus:ring-blue-500 focus:border-blue-500 focus:z-10 sm:text-sm`}
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
                <p className="mt-1 text-sm text-red-600">
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
                className={`appearance-none relative block w-full px-3 py-2 border ${
                  errors.passwordConfirm ? 'border-red-300' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 rounded-md focus:outline-none focus:ring-blue-500 focus:border-blue-500 focus:z-10 sm:text-sm`}
                placeholder="パスワード（確認）"
                {...register('passwordConfirm', {
                  required: 'パスワード（確認）を入力してください',
                  validate: (value) =>
                    value === password || 'パスワードが一致しません',
                })}
              />
              {errors.passwordConfirm && (
                <p className="mt-1 text-sm text-red-600">
                  {errors.passwordConfirm.message}
                </p>
              )}
            </div>
          </div>

          {/* 登録ボタン */}
          <div>
            <button
              type="submit"
              disabled={isSubmitting}
              className={`group relative w-full flex justify-center py-2 px-4 border border-transparent text-sm font-medium rounded-md text-white ${
                isSubmitting
                  ? 'bg-blue-400 cursor-not-allowed'
                  : 'bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500'
              }`}
            >
              {isSubmitting ? '登録中...' : '登録'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
