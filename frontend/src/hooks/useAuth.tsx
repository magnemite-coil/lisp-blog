import { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import * as authApi from '../api/auth';
import type { User, LoginRequest, RegisterRequest } from '../types/User';

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
      const currentUser = await authApi.getCurrentUser();
      setUser(currentUser);
    } catch (error) {
      console.error('ログイン状態の確認に失敗:', error);
      setUser(null);
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * ログイン処理
   */
  const login = async (data: LoginRequest) => {
    const userData = await authApi.login(data);
    setUser(userData);
  };

  /**
   * ユーザー登録処理
   */
  const register = async (data: RegisterRequest) => {
    const userData = await authApi.register(data);
    setUser(userData);
  };

  /**
   * ログアウト処理
   */
  const logout = async () => {
    await authApi.logout();
    setUser(null);
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
