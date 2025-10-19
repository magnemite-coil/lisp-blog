/**
 * エラー監視ユーティリティ
 */
import { AppError } from '../types/Error';

/**
 * エラーログのエントリ
 */
interface ErrorLogEntry {
  timestamp: string;
  errorId: string;
  code: string;
  message: string;
  statusCode?: number;
  field?: string;
  details?: Record<string, any>;
  userAgent: string;
  url: string;
  stackTrace?: string;
}

/**
 * エラー統計
 */
interface ErrorStats {
  totalErrors: number;
  errorsByCode: Record<string, number>;
  errorsByStatus: Record<number, number>;
  lastError?: ErrorLogEntry;
}

/**
 * エラーモニタークラス
 */
class ErrorMonitor {
  private errorLogs: ErrorLogEntry[] = [];
  private maxLogs = 100; // 保存する最大ログ数
  private stats: ErrorStats = {
    totalErrors: 0,
    errorsByCode: {},
    errorsByStatus: {},
  };

  /**
   * 一意のエラーIDを生成
   */
  private generateErrorId(): string {
    return `err_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * エラーをログに記録
   */
  logError(error: AppError | Error): string {
    const errorId = this.generateErrorId();
    const timestamp = new Date().toISOString();

    let logEntry: ErrorLogEntry;

    if (error instanceof AppError) {
      logEntry = {
        timestamp,
        errorId,
        code: error.code,
        message: error.message,
        statusCode: error.statusCode,
        field: error.field,
        details: error.details,
        userAgent: navigator.userAgent,
        url: window.location.href,
        stackTrace: error.stack,
      };

      // 統計を更新
      this.stats.errorsByCode[error.code] = (this.stats.errorsByCode[error.code] || 0) + 1;
      if (error.statusCode) {
        this.stats.errorsByStatus[error.statusCode] = (this.stats.errorsByStatus[error.statusCode] || 0) + 1;
      }
    } else {
      logEntry = {
        timestamp,
        errorId,
        code: 'UNKNOWN_ERROR',
        message: error.message,
        userAgent: navigator.userAgent,
        url: window.location.href,
        stackTrace: error.stack,
      };

      this.stats.errorsByCode['UNKNOWN_ERROR'] = (this.stats.errorsByCode['UNKNOWN_ERROR'] || 0) + 1;
    }

    // ログを追加
    this.errorLogs.push(logEntry);

    // 最大数を超えた場合は古いログを削除
    if (this.errorLogs.length > this.maxLogs) {
      this.errorLogs.shift();
    }

    // 統計を更新
    this.stats.totalErrors++;
    this.stats.lastError = logEntry;

    // 開発環境ではコンソールに出力
    if (import.meta.env.DEV) {
      console.error(`[Error Monitor] ${errorId}`, logEntry);
    }

    // 本番環境では外部サービスに送信（将来の拡張ポイント）
    if (import.meta.env.PROD) {
      this.sendToExternalService(logEntry);
    }

    return errorId;
  }

  /**
   * すべてのエラーログを取得
   */
  getErrorLogs(): ErrorLogEntry[] {
    return [...this.errorLogs];
  }

  /**
   * エラー統計を取得
   */
  getStats(): ErrorStats {
    return { ...this.stats };
  }

  /**
   * 特定のエラーコードのログを取得
   */
  getErrorsByCode(code: string): ErrorLogEntry[] {
    return this.errorLogs.filter((log) => log.code === code);
  }

  /**
   * 最新のエラーを取得
   */
  getLatestErrors(count: number = 10): ErrorLogEntry[] {
    return this.errorLogs.slice(-count).reverse();
  }

  /**
   * ログをクリア
   */
  clearLogs(): void {
    this.errorLogs = [];
    this.stats = {
      totalErrors: 0,
      errorsByCode: {},
      errorsByStatus: {},
    };
  }

  /**
   * エラーログをJSON形式でエクスポート
   */
  exportLogs(): string {
    return JSON.stringify(
      {
        exportedAt: new Date().toISOString(),
        stats: this.stats,
        logs: this.errorLogs,
      },
      null,
      2
    );
  }

  /**
   * 外部サービスにエラーを送信（プレースホルダー）
   * 将来的にSentry等と統合する場合はここに実装
   */
  private sendToExternalService(logEntry: ErrorLogEntry): void {
    // TODO: Sentry、Rollbar、Bugsnag等との統合
    // 例: Sentry.captureException(error);
    console.log('[Error Monitor] Would send to external service:', logEntry.errorId);
  }

  /**
   * エラーレポートをダウンロード
   */
  downloadErrorReport(): void {
    const report = this.exportLogs();
    const blob = new Blob([report], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `error-report-${Date.now()}.json`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  }
}

/**
 * シングルトンインスタンス
 */
export const errorMonitor = new ErrorMonitor();

/**
 * グローバルエラーハンドラーを設定
 */
export function setupGlobalErrorHandler(): void {
  // 未処理のPromise rejectionをキャッチ
  window.addEventListener('unhandledrejection', (event) => {
    console.error('Unhandled Promise Rejection:', event.reason);
    errorMonitor.logError(event.reason);
  });

  // グローバルエラーをキャッチ
  window.addEventListener('error', (event) => {
    console.error('Global Error:', event.error);
    if (event.error) {
      errorMonitor.logError(event.error);
    }
  });

  if (import.meta.env.DEV) {
    console.log('[Error Monitor] Global error handlers installed');
  }
}
