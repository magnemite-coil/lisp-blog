import { useEffect, useState } from 'react';
import type { Toast, ToastType } from '../../types/Toast';

/**
 * トーストアイテムのProps
 */
interface ToastItemProps {
  toast: Toast;
  onDismiss: (id: string) => void;
}

/**
 * トーストの種類ごとのスタイル設定（Twitter風）
 * すべて同じ水色で統一
 */
const toastStyles: Record<ToastType, { icon: string }> = {
  success: {
    icon: '✓',
  },
  error: {
    icon: '✕',
  },
  warning: {
    icon: '⚠',
  },
  info: {
    icon: 'ℹ',
  },
};

/**
 * 個別のトーストアイテムコンポーネント
 */
export function ToastItem({ toast, onDismiss }: ToastItemProps) {
  const [isVisible, setIsVisible] = useState(false);
  const [isLeaving, setIsLeaving] = useState(false);

  const style = toastStyles[toast.type];

  // マウント時にフェードイン
  useEffect(() => {
    const timer = setTimeout(() => setIsVisible(true), 10);
    return () => clearTimeout(timer);
  }, []);

  /**
   * トーストを閉じる
   */
  const handleDismiss = () => {
    setIsLeaving(true);
    // アニメーション完了後に削除
    setTimeout(() => {
      onDismiss(toast.id);
    }, 300);
  };

  return (
    <div
      className={`
        transform transition-all duration-300 ease-in-out
        ${isVisible && !isLeaving ? 'translate-y-0 opacity-100' : '-translate-y-2 opacity-0'}
      `}
      role="alert"
    >
      <div
        className="flex items-center gap-3 px-4 py-3 rounded-full shadow-lg mb-2"
        style={{
          minWidth: '280px',
          maxWidth: '400px',
          backgroundColor: '#1DA1F2', // Twitter blue
          color: 'white',
        }}
      >
        {/* アイコン */}
        <div className="flex-shrink-0 text-base font-bold">
          {style.icon}
        </div>

        {/* メッセージ */}
        <div className="flex-1 text-sm font-medium">
          {toast.message}
        </div>

        {/* 閉じるボタン */}
        {toast.dismissible && (
          <button
            onClick={handleDismiss}
            className="flex-shrink-0 text-white hover:text-gray-200 focus:outline-none ml-2"
            aria-label="閉じる"
          >
            <svg
              className="h-4 w-4"
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 20 20"
              fill="currentColor"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              />
            </svg>
          </button>
        )}
      </div>
    </div>
  );
}
