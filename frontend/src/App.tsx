import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { AuthProvider } from './hooks/useAuth';
import { LoginPage } from './pages/LoginPage';
import { RegisterPage } from './pages/RegisterPage';

/**
 * メインアプリケーションコンポーネント
 */
function App() {
  return (
    <BrowserRouter>
      <AuthProvider>
        <Routes>
          {/* ルートパス - ログインページにリダイレクト */}
          <Route path="/" element={<Navigate to="/login" replace />} />

          {/* ログインページ */}
          <Route path="/login" element={<LoginPage />} />

          {/* ユーザー登録ページ */}
          <Route path="/register" element={<RegisterPage />} />

          {/* ダッシュボード（後で実装） */}
          {/* <Route path="/dashboard" element={<DashboardPage />} /> */}

          {/* 404 Not Found */}
          <Route path="*" element={<Navigate to="/login" replace />} />
        </Routes>
      </AuthProvider>
    </BrowserRouter>
  );
}

export default App;
