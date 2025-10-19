import { createContext, useContext, useState, useEffect, type ReactNode } from 'react';
import * as authApi from '../api/auth';
import type { User, LoginRequest, RegisterRequest } from '../types/User';
import { useToast } from '../contexts/ToastContext';
import { AppError } from '../types/Error';

/**
 * 認証コンテキストの型定義
 */
interface AuthContextType {
  // 現在のユーザー（未ログインならnull）
  user: User | null;

  // ログイン中かどうか
  isAuthenticated: boolean;

  // ローディング中かどうか（初回ユーザー情報取得時）
  isLoading: boolean;

  // ログイン関数
  login: (data: LoginRequest) => Promise<void>;

  // ユーザー登録関数
  register: (data: RegisterRequest) => Promise<void>;

  // ログアウト関数
  logout: () => Promise<void>;
}

/**
 * 認証コンテキストの作成
 */
const AuthContext = createContext<AuthContextType | undefined>(undefined);

/**
 * 認証プロバイダーコンポーネント
 *
 * アプリ全体をこのコンポーネントで囲むことで、
 * どこからでもログイン状態にアクセスできるようになります。
 */
export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const { showSuccess, showError } = useToast();

  /**
   * 初回レンダリング時にログイン状態を確認
   */
  useEffect(() => {
    checkAuthStatus();
  }, []);

  /**
   * ログイン状態を確認する関数
   */
  const checkAuthStatus = async () => {
    try {
      console.log('🔍 ログイン状態を確認中...');
      const currentUser = await authApi.getCurrentUser();
      console.log('✅ ユーザー情報を取得:', currentUser);
      setUser(currentUser);
    } catch (error) {
      console.error('❌ ログイン状態の確認に失敗:', error);
      setUser(null);
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * ログイン処理
   */
  const login = async (data: LoginRequest) => {
    try {
      console.log('🔐 ログイン中...');
      const userData = await authApi.login(data);
      console.log('✅ ログイン成功、ユーザー情報:', userData);
      setUser(userData);
      showSuccess(`ようこそ、${userData.username}さん！`);
    } catch (error) {
      if (error instanceof AppError) {
        showError(error.getUserMessage());
      } else {
        showError('ログインに失敗しました');
      }
      throw error;
    }
  };

  /**
   * ユーザー登録処理
   */
  const register = async (data: RegisterRequest) => {
    try {
      console.log('📝 ユーザー登録中...');
      const userData = await authApi.register(data);
      console.log('✅ 登録成功、ユーザー情報:', userData);
      setUser(userData);
      showSuccess(`アカウントを作成しました。ようこそ、${userData.username}さん！`);
    } catch (error) {
      if (error instanceof AppError) {
        showError(error.getUserMessage());
      } else {
        showError('ユーザー登録に失敗しました');
      }
      throw error;
    }
  };

  /**
   * ログアウト処理
   */
  const logout = async () => {
    try {
      await authApi.logout();
      setUser(null);
      showSuccess('ログアウトしました');
    } catch (error) {
      if (error instanceof AppError) {
        showError(error.getUserMessage());
      } else {
        showError('ログアウトに失敗しました');
      }
      throw error;
    }
  };

  // コンテキストの値
  const value: AuthContextType = {
    user,
    isAuthenticated: user !== null,
    isLoading,
    login,
    register,
    logout,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

/**
 * 認証コンテキストを使用するカスタムフック
 *
 * 使用例:
 * ```typescript
 * const { user, login, logout } = useAuth();
 *
 * if (user) {
 *   console.log('ログイン中:', user.username);
 * }
 * ```
 */
export function useAuth() {
  const context = useContext(AuthContext);

  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }

  return context;
}
