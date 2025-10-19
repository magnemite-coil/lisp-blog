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
 * トーストの種類ごとのスタイル設定
 */
const toastStyles: Record<ToastType, { bg: string; border: string; icon: string; iconColor: string }> = {
  success: {
    bg: 'bg-green-50',
    border: 'border-green-200',
    icon: '✓',
    iconColor: 'text-green-500',
  },
  error: {
    bg: 'bg-red-50',
    border: 'border-red-200',
    icon: '✕',
    iconColor: 'text-red-500',
  },
  warning: {
    bg: 'bg-yellow-50',
    border: 'border-yellow-200',
    icon: '⚠',
    iconColor: 'text-yellow-500',
  },
  info: {
    bg: 'bg-blue-50',
    border: 'border-blue-200',
    icon: 'ℹ',
    iconColor: 'text-blue-500',
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
        transform transition-all duration-300 ease-in-out mb-2
        ${isVisible && !isLeaving ? 'translate-x-0 opacity-100' : 'translate-x-full opacity-0'}
      `}
      role="alert"
    >
      <div
        className={`
          flex items-center gap-3 p-4 rounded-lg shadow-lg border
          ${style.bg} ${style.border}
          min-w-[300px] max-w-[500px]
        `}
      >
        {/* アイコン */}
        <div className={`flex-shrink-0 text-xl ${style.iconColor}`}>
          {style.icon}
        </div>

        {/* メッセージ */}
        <div className="flex-1 text-sm font-medium text-gray-800">
          {toast.message}
        </div>

        {/* 閉じるボタン */}
        {toast.dismissible && (
          <button
            onClick={handleDismiss}
            className="flex-shrink-0 text-gray-400 hover:text-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-500 rounded"
            aria-label="閉じる"
          >
            <svg
              className="h-5 w-5"
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
