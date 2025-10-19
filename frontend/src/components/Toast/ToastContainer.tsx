import { useToast } from '../../contexts/ToastContext';
import { ToastItem } from './ToastItem';

/**
 * トーストコンテナのProps
 */
interface ToastContainerProps {
  position?: 'top-right' | 'top-left' | 'bottom-right' | 'bottom-left';
}

/**
 * 位置ごとのスタイル設定
 */
const positionStyles = {
  'top-right': 'top-4 right-4',
  'top-left': 'top-4 left-4',
  'bottom-right': 'bottom-4 right-4',
  'bottom-left': 'bottom-4 left-4',
};

/**
 * トーストコンテナコンポーネント
 * すべてのトーストを画面上に表示します
 *
 * 使用例:
 * <ToastContainer position="top-right" />
 */
export function ToastContainer({ position = 'top-right' }: ToastContainerProps) {
  const { toasts, dismissToast } = useToast();

  if (toasts.length === 0) {
    return null;
  }

  return (
    <div
      className={`fixed ${positionStyles[position]} z-50 pointer-events-none`}
      aria-live="polite"
      aria-atomic="true"
    >
      <div className="flex flex-col pointer-events-auto">
        {toasts.map((toast) => (
          <ToastItem key={toast.id} toast={toast} onDismiss={dismissToast} />
        ))}
      </div>
    </div>
  );
}
