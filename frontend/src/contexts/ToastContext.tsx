import { createContext, useContext, useState, useCallback, type ReactNode } from 'react';
import type { Toast, ToastOptions } from '../types/Toast';

/**
 * トーストコンテキストの型定義
 */
interface ToastContextType {
  toasts: Toast[];
  showToast: (message: string, options?: ToastOptions) => void;
  showSuccess: (message: string, duration?: number) => void;
  showError: (message: string, duration?: number) => void;
  showWarning: (message: string, duration?: number) => void;
  showInfo: (message: string, duration?: number) => void;
  dismissToast: (id: string) => void;
  clearAllToasts: () => void;
}

/**
 * トーストコンテキスト
 */
const ToastContext = createContext<ToastContextType | undefined>(undefined);

/**
 * 一意のIDを生成
 */
const generateId = (): string => {
  return `toast_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
};

/**
 * トーストプロバイダーのProps
 */
interface ToastProviderProps {
  children: ReactNode;
  maxToasts?: number; // 同時に表示する最大数（デフォルト: 5）
}

/**
 * トーストプロバイダー
 * アプリケーション全体をこのプロバイダーで囲むことで、
 * どのコンポーネントからでもトーストを表示できるようになります
 */
export function ToastProvider({ children, maxToasts = 5 }: ToastProviderProps) {
  const [toasts, setToasts] = useState<Toast[]>([]);

  /**
   * トーストを表示
   */
  const showToast = useCallback((message: string, options?: ToastOptions) => {
    const id = generateId();
    const newToast: Toast = {
      id,
      message,
      type: options?.type || 'info',
      duration: options?.duration ?? 5000,
      dismissible: options?.dismissible ?? true,
    };

    setToasts((prev) => {
      // 最大数を超える場合は古いものから削除
      const updated = [...prev, newToast];
      if (updated.length > maxToasts) {
        return updated.slice(updated.length - maxToasts);
      }
      return updated;
    });

    // 自動的に消去
    if (newToast.duration && newToast.duration > 0) {
      setTimeout(() => {
        dismissToast(id);
      }, newToast.duration);
    }
  }, [maxToasts]);

  /**
   * 成功トーストを表示
   */
  const showSuccess = useCallback((message: string, duration?: number) => {
    showToast(message, { type: 'success', duration });
  }, [showToast]);

  /**
   * エラートーストを表示
   */
  const showError = useCallback((message: string, duration?: number) => {
    showToast(message, { type: 'error', duration });
  }, [showToast]);

  /**
   * 警告トーストを表示
   */
  const showWarning = useCallback((message: string, duration?: number) => {
    showToast(message, { type: 'warning', duration });
  }, [showToast]);

  /**
   * 情報トーストを表示
   */
  const showInfo = useCallback((message: string, duration?: number) => {
    showToast(message, { type: 'info', duration });
  }, [showToast]);

  /**
   * 特定のトーストを消去
   */
  const dismissToast = useCallback((id: string) => {
    setToasts((prev) => prev.filter((toast) => toast.id !== id));
  }, []);

  /**
   * すべてのトーストを消去
   */
  const clearAllToasts = useCallback(() => {
    setToasts([]);
  }, []);

  const value: ToastContextType = {
    toasts,
    showToast,
    showSuccess,
    showError,
    showWarning,
    showInfo,
    dismissToast,
    clearAllToasts,
  };

  return <ToastContext.Provider value={value}>{children}</ToastContext.Provider>;
}

/**
 * トーストコンテキストを使用するカスタムフック
 *
 * 使用例:
 * const { showSuccess, showError } = useToast();
 * showSuccess('保存しました');
 * showError('エラーが発生しました');
 */
export function useToast(): ToastContextType {
  const context = useContext(ToastContext);
  if (!context) {
    throw new Error('useToast must be used within a ToastProvider');
  }
  return context;
}
