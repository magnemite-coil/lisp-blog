(in-package :cl-user)
(defpackage lisp-blog.util.error
  (:use :cl)
  (:import-from :lisp-blog.util.conditions
                :lisp-blog-error
                :validation-error
                :authentication-error
                :authorization-error
                :resource-not-found-error
                :resource-conflict-error
                :business-logic-error
                :system-error
                :error-code
                :error-message
                :error-details
                :error-field
                :resource-type
                :resource-id)
  (:import-from :jonathan
                :to-json)
  (:export :make-error-response
           :validation-error-response
           :auth-error-response
           :resource-error-response
           :business-error-response
           :system-error-response
           :condition-to-response
           :generate-error-id))
(in-package :lisp-blog.util.error)

;;; ユーティリティ関数

(defun generate-error-id ()
  "一意のエラーIDを生成（デバッグ・サポート用）

   戻り値: エラーID文字列（例: 'err_a7k3m9sd'）"
  (format nil "err_~a"
          (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
            (coerce (loop repeat 8
                         collect (aref chars (random (length chars))))
                    'string))))

(defun get-timestamp ()
  "現在時刻のISO 8601形式タイムスタンプを取得

   戻り値: タイムスタンプ文字列"
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour min sec)))

;;; エラーレスポンス生成関数

(defun make-error-response (code message &key status field details error-id)
  "構造化されたエラーレスポンスを生成

   code: エラーコード（キーワードまたは文字列）
   message: エラーメッセージ（文字列）
   status: HTTPステータスコード（数値、デフォルト400）
   field: エラーが発生したフィールド名（省略可）
   details: 追加の詳細情報（plist、省略可）
   error-id: エラーID（省略時は自動生成）

   戻り値: (values json-string status headers)"
  (let* ((status-code (or status 400))
         (err-id (or error-id (generate-error-id)))
         (code-str (if (keywordp code)
                       (string-upcase (symbol-name code))
                       (string-upcase code)))
         (error-data (list :|code| code-str
                          :|message| message
                          :|error_id| err-id)))

    ;; フィールド名がある場合は追加
    (when field
      (setf error-data (append error-data (list :|field| field))))

    ;; 詳細情報がある場合は追加
    (when details
      (setf error-data (append error-data (list :|details| details))))

    ;; JSONレスポンスを生成
    (let ((response-json (jonathan:to-json
                          (list :|success| :false
                               :|error| error-data))))
      (values response-json
              status-code
              '(:content-type "application/json")))))

;;; 専用エラー生成関数

(defun validation-error-response (message &key field details)
  "バリデーションエラーレスポンスを生成

   message: エラーメッセージ
   field: エラーが発生したフィールド名（省略可）
   details: 追加の詳細情報（省略可）

   戻り値: (values json-string status headers)"
  (make-error-response :validation-error message
                      :status 400
                      :field field
                      :details details))

(defun auth-error-response (type &optional (message nil))
  "認証/認可エラーレスポンスを生成

   type: エラータイプ（キーワード）
         :required - 認証が必要
         :invalid-credentials - 認証情報が不正
         :session-expired - セッション期限切れ
         :permission-denied - 権限不足
   message: カスタムメッセージ（省略可）

   戻り値: (values json-string status headers)"
  (case type
    (:required
     (make-error-response :auth-required
                         (or message "Authentication required")
                         :status 401))
    (:invalid-credentials
     (make-error-response :auth-invalid-credentials
                         (or message "Invalid credentials")
                         :status 401))
    (:session-expired
     (make-error-response :auth-session-expired
                         (or message "Session expired")
                         :status 401))
    (:permission-denied
     (make-error-response :auth-permission-denied
                         (or message "Permission denied")
                         :status 403))
    (t
     (make-error-response :auth-error
                         (or message "Authentication error")
                         :status 401))))

(defun resource-error-response (type &key (message nil) (resource-type "resource") (resource-id nil))
  "リソース関連エラーレスポンスを生成

   type: エラータイプ（キーワード）
         :not-found - リソースが見つからない
         :already-exists - リソースが既に存在
         :conflict - リソース競合
   message: カスタムメッセージ（省略可）
   resource-type: リソースの種類（例: 'user', 'post'）
   resource-id: リソースのID（省略可）

   戻り値: (values json-string status headers)"
  (let ((details (when resource-id
                   (list :|resource_type| resource-type
                        :|resource_id| resource-id))))
    (case type
      (:not-found
       (make-error-response :resource-not-found
                           (or message (format nil "~A not found" resource-type))
                           :status 404
                           :details details))
      (:already-exists
       (make-error-response :resource-already-exists
                           (or message (format nil "~A already exists" resource-type))
                           :status 409
                           :details details))
      (:conflict
       (make-error-response :resource-conflict
                           (or message (format nil "~A conflict" resource-type))
                           :status 409
                           :details details))
      (t
       (make-error-response :resource-error
                           (or message "Resource error")
                           :status 400
                           :details details)))))

(defun business-error-response (code message &key details)
  "ビジネスロジックエラーレスポンスを生成

   code: エラーコード（キーワード）
   message: エラーメッセージ
   details: 追加の詳細情報（省略可）

   戻り値: (values json-string status headers)"
  (make-error-response code message
                      :status 400
                      :details details))

(defun system-error-response (type &optional (message nil))
  "システムエラーレスポンスを生成

   type: エラータイプ（キーワード）
         :database - データベースエラー
         :timeout - タイムアウト
         :unavailable - サービス利用不可
         :internal - 内部エラー
   message: カスタムメッセージ（省略可）

   戻り値: (values json-string status headers)"
  (case type
    (:database
     (make-error-response :system-database-error
                         (or message "Database error occurred")
                         :status 500))
    (:timeout
     (make-error-response :system-timeout
                         (or message "Request timeout")
                         :status 504))
    (:unavailable
     (make-error-response :system-service-unavailable
                         (or message "Service temporarily unavailable")
                         :status 503))
    (t
     (make-error-response :system-internal-error
                         (or message "Internal server error")
                         :status 500))))

;;; Conditionからレスポンスへの変換

(defun condition-to-response (condition)
  "Conditionオブジェクトから適切なエラーレスポンスを生成

   condition: lisp-blog-error またはそのサブクラスのインスタンス

   戻り値: (values json-string status headers)"
  (etypecase condition
    (validation-error
     (validation-error-response (error-message condition)
                               :field (error-field condition)
                               :details (error-details condition)))

    (authentication-error
     (let ((code (error-code condition)))
       (cond
         ((eq code :auth-required)
          (auth-error-response :required (error-message condition)))
         ((eq code :auth-invalid-credentials)
          (auth-error-response :invalid-credentials (error-message condition)))
         ((eq code :auth-session-expired)
          (auth-error-response :session-expired (error-message condition)))
         (t
          (auth-error-response :required (error-message condition))))))

    (authorization-error
     (auth-error-response :permission-denied (error-message condition)))

    (resource-not-found-error
     (resource-error-response :not-found
                             :message (error-message condition)
                             :resource-type (resource-type condition)
                             :resource-id (resource-id condition)))

    (resource-conflict-error
     (resource-error-response :conflict
                             :message (error-message condition)
                             :resource-type (resource-type condition)))

    (business-logic-error
     (business-error-response (error-code condition)
                             (error-message condition)
                             :details (error-details condition)))

    (system-error
     (system-error-response :internal (error-message condition)))

    (lisp-blog-error
     ;; 汎用的なエラー処理
     (make-error-response (error-code condition)
                         (error-message condition)
                         :status 500
                         :details (error-details condition)))))

;;; ログ出力関数

(defun log-error (condition &key (handler-name "unknown") (request-info nil))
  "エラーログを出力

   condition: エラー条件
   handler-name: ハンドラー名
   request-info: リクエスト情報（plist）

   戻り値: エラーID"
  (let ((error-id (generate-error-id))
        (timestamp (get-timestamp)))

    ;; コンソールに出力（本番環境ではログファイルに出力）
    (format t "~%[ERROR] ~A ~A~%" timestamp error-id)
    (format t "  Handler: ~A~%" handler-name)
    (format t "  Code: ~A~%" (if (typep condition 'lisp-blog-error)
                                 (error-code condition)
                                 "UNKNOWN"))
    (format t "  Message: ~A~%" (if (typep condition 'lisp-blog-error)
                                    (error-message condition)
                                    (princ-to-string condition)))
    (when request-info
      (format t "  Request: ~A~%" request-info))

    error-id))
