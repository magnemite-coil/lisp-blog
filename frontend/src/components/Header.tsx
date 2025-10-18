import { Link, useNavigate } from 'react-router-dom';
import { useAuth } from '../hooks/useAuth';

/**
 * ヘッダーコンポーネント
 *
 * ユーザー名とログアウトボタンを表示します。
 */
export function Header() {
  const { user, logout } = useAuth();
  const navigate = useNavigate();

  /**
   * ログアウト処理
   */
  const handleLogout = async () => {
    try {
      await logout();
      navigate('/login');
    } catch (error) {
      console.error('ログアウトに失敗:', error);
    }
  };

  return (
    <header className="bg-white shadow">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center py-6">
          {/* ロゴ・タイトル */}
          <div className="flex items-center">
            <Link to="/dashboard" className="text-2xl font-bold text-gray-900">
              Lisp Blog
            </Link>
          </div>

          {/* ユーザー情報とログアウト */}
          {user && (
            <div className="flex items-center space-x-4">
              <span className="text-gray-700">
                ようこそ、<span className="font-semibold">{user.username}</span>さん
              </span>
              <button
                onClick={handleLogout}
                className="px-4 py-2 text-sm font-medium text-white bg-red-600 rounded-md hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500"
              >
                ログアウト
              </button>
            </div>
          )}
        </div>
      </div>
    </header>
  );
}
