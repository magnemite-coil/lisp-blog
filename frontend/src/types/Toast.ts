/**
 * トースト通知の型定義
 */

/**
 * トーストの種類
 */
export type ToastType = 'success' | 'error' | 'warning' | 'info';

/**
 * トーストの位置
 */
export type ToastPosition = 'top-right' | 'top-left' | 'bottom-right' | 'bottom-left' | 'top-center' | 'bottom-center';

/**
 * トーストメッセージの構造
 */
export interface Toast {
  id: string;
  type: ToastType;
  message: string;
  duration?: number; // ミリ秒（デフォルト: 5000）
  dismissible?: boolean; // 手動で閉じられるか（デフォルト: true）
}

/**
 * トーストの設定オプション
 */
export interface ToastOptions {
  type?: ToastType;
  duration?: number;
  dismissible?: boolean;
}
