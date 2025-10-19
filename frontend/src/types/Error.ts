/**
 * バックエンドから返されるエラーレスポンスの型
 */
export interface ApiError {
  code: string;
  message: string;
  field?: string;
  details?: Record<string, any>;
}

/**
 * APIレスポンスの共通型
 */
export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  message?: string;
  error?: ApiError;
}

/**
 * フロントエンド用のエラークラス
 */
export class AppError extends Error {
  public code: string;
  public field?: string;
  public details?: Record<string, any>;
  public statusCode?: number;

  constructor(
    code: string,
    message: string,
    field?: string,
    details?: Record<string, any>,
    statusCode?: number
  ) {
    super(message);
    this.name = 'AppError';
    this.code = code;
    this.field = field;
    this.details = details;
    this.statusCode = statusCode;
  }

  /**
   * ユーザー向けのメッセージを取得
   */
  getUserMessage(): string {
    return ERROR_MESSAGES[this.code] || this.message || 'エラーが発生しました';
  }

  /**
   * フィールドエラーかどうか
   */
  isFieldError(): boolean {
    return !!this.field;
  }
}

/**
 * エラーコード別のユーザー向けメッセージ
 */
export const ERROR_MESSAGES: Record<string, string> = {
  // 認証関連
  AUTH_REQUIRED: 'ログインが必要です',
  AUTH_INVALID_CREDENTIALS: 'ユーザー名またはパスワードが正しくありません',
  AUTH_SESSION_EXPIRED: 'セッションの有効期限が切れました。再度ログインしてください',
  AUTH_PERMISSION_DENIED: 'この操作を行う権限がありません',

  // バリデーション関連
  VALIDATION_ERROR: '入力内容に誤りがあります',
  VALIDATION_REQUIRED_FIELD: 'この項目は必須です',
  VALIDATION_INVALID_FORMAT: '入力形式が正しくありません',
  VALIDATION_TOO_SHORT: '入力内容が短すぎます',
  VALIDATION_TOO_LONG: '入力内容が長すぎます',

  // リソース関連
  RESOURCE_NOT_FOUND: '指定されたデータが見つかりません',
  RESOURCE_ALREADY_EXISTS: 'すでに登録されています',
  RESOURCE_CONFLICT: 'データの競合が発生しました',

  // ビジネスロジック関連
  BUSINESS_ALREADY_PUBLISHED: 'この投稿はすでに公開されています',
  BUSINESS_ALREADY_DRAFT: 'この投稿はすでに下書き状態です',

  // システム関連
  SYSTEM_DATABASE_ERROR: 'データベースエラーが発生しました',
  SYSTEM_INTERNAL_ERROR: 'サーバー内部でエラーが発生しました',
  SYSTEM_TIMEOUT: 'リクエストがタイムアウトしました',
  SYSTEM_SERVICE_UNAVAILABLE: 'サービスが一時的に利用できません',

  // ネットワーク関連
  NETWORK_ERROR: 'ネットワークエラーが発生しました。接続を確認してください',
  NETWORK_TIMEOUT: '通信がタイムアウトしました',
};
