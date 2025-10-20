import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { AuthProvider } from './hooks/useAuth';
import { ToastProvider } from './contexts/ToastContext';
import { ToastContainer } from './components/Toast/ToastContainer';
import { LoginPage } from './pages/LoginPage';
import { RegisterPage } from './pages/RegisterPage';
import { DashboardPage } from './pages/DashboardPage';
import { CreatePostPage } from './pages/CreatePostPage';
import { EditPostPage } from './pages/EditPostPage';

/**
 * メインアプリケーションコンポーネント
 */
function App() {
  return (
    <BrowserRouter>
      <ToastProvider>
        {/* トースト通知コンテナ - ToastProviderの直下に配置 */}
        <ToastContainer position="top-right" />

        <AuthProvider>
          <Routes>
            {/* ルートパス - ログインページにリダイレクト */}
            <Route path="/" element={<Navigate to="/login" replace />} />

            {/* ログインページ */}
            <Route path="/login" element={<LoginPage />} />

            {/* ユーザー登録ページ */}
            <Route path="/register" element={<RegisterPage />} />

            {/* ダッシュボード */}
            <Route path="/dashboard" element={<DashboardPage />} />

            {/* 投稿作成 */}
            <Route path="/posts/new" element={<CreatePostPage />} />

            {/* 投稿編集 */}
            <Route path="/posts/:id/edit" element={<EditPostPage />} />

            {/* 404 Not Found */}
            <Route path="*" element={<Navigate to="/login" replace />} />
          </Routes>
        </AuthProvider>
      </ToastProvider>
    </BrowserRouter>
  );
}

export default App;
