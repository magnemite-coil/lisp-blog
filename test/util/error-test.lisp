(in-package :lisp-blog-test)

;;; エラーレスポンス生成関数のテストスイート

(def-suite error-tests
  :in lisp-blog-test-suite
  :description "エラーレスポンス生成のテスト")

(in-suite error-tests)

;;; generate-error-id のテスト

(test generate-error-id-format
  "エラーIDの形式テスト"
  (let ((error-id (lisp-blog.util.error:generate-error-id)))
    ;; "err_" で始まること
    (is (search "err_" error-id))
    ;; 長さが12文字（"err_" + 8文字）
    (is (= 12 (length error-id)))
    ;; 小文字英数字のみ（"err_"以降の8文字）
    (let ((suffix (subseq error-id 4)))
      (is (every (lambda (c)
                   (or (digit-char-p c)
                       (char<= #\a c #\z)))
                 suffix)))))

(test generate-error-id-uniqueness
  "エラーIDの一意性（高確率で異なるIDが生成される）"
  (let ((ids (loop repeat 100
                   collect (lisp-blog.util.error:generate-error-id))))
    ;; 100個生成して全て異なることを確認（確率的にはほぼ確実）
    (is (= 100 (length (remove-duplicates ids :test #'string=))))))

;;; make-error-response のテスト

(test make-error-response-basic
  "基本的なエラーレスポンス生成"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :test-error "Test error message")
    ;; デフォルトステータスは400
    (is (= 400 status))
    ;; Content-Typeヘッダー
    (is (string= "application/json" (getf headers :content-type)))
    ;; JSONパース
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (not (getf parsed :|success|)))
      (let ((error-obj (getf parsed :|error|)))
        (is (string= "TEST-ERROR" (getf error-obj :|code|)))
        (is (string= "Test error message" (getf error-obj :|message|)))
        ;; error_idが含まれていること
        (is (getf error-obj :|error_id|))
        (is (search "err_" (getf error-obj :|error_id|)))))))

(test make-error-response-with-custom-status
  "カスタムステータスコード付きエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :not-found "Resource not found" :status 404)
    (is (= 404 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "NOT-FOUND" (getf error-obj :|code|)))
      (is (string= "Resource not found" (getf error-obj :|message|))))))

(test make-error-response-with-field
  "フィールド指定付きエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response
       :validation-error
       "Username is required"
       :field "username")
    (is (= 400 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "VALIDATION-ERROR" (getf error-obj :|code|)))
      (is (string= "Username is required" (getf error-obj :|message|)))
      (is (string= "username" (getf error-obj :|field|))))))

(test make-error-response-with-details
  "詳細情報付きエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response
       :validation-error
       "Password too short"
       :details (list :|min_length| 8 :|actual_length| 5))
    (is (= 400 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (= 8 (getf details :|min_length|)))
      (is (= 5 (getf details :|actual_length|))))))

(test make-error-response-with-custom-error-id
  "カスタムエラーID付きエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response
       :test-error
       "Test message"
       :error-id "err_custom123")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "err_custom123" (getf error-obj :|error_id|))))))

(test make-error-response-code-formats
  "エラーコードの各種形式テスト"
  ;; キーワード形式
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :test-error "Test")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "TEST-ERROR" (getf error-obj :|code|)))))

  ;; 文字列形式
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response "custom-error" "Test")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "CUSTOM-ERROR" (getf error-obj :|code|))))))

;;; validation-error-response のテスト

(test validation-error-response-basic
  "バリデーションエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response "Invalid input")
    (is (= 400 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "VALIDATION-ERROR" (getf error-obj :|code|)))
      (is (string= "Invalid input" (getf error-obj :|message|))))))

(test validation-error-response-with-field
  "フィールド指定付きバリデーションエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response
       "Username is required"
       :field "username")
    (is (= 400 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "username" (getf error-obj :|field|))))))

(test validation-error-response-with-details
  "詳細情報付きバリデーションエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response
       "Invalid format"
       :field "email"
       :details (list :|pattern| "^[a-z]+@[a-z]+\\.[a-z]+$"))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (string= "email" (getf error-obj :|field|)))
      (is (stringp (getf details :|pattern|))))))

;;; auth-error-response のテスト

(test auth-error-response-required
  "認証が必要エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :required)
    (is (= 401 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "AUTH-REQUIRED" (getf error-obj :|code|)))
      (is (string= "Authentication required" (getf error-obj :|message|))))))

(test auth-error-response-invalid-credentials
  "認証情報不正エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :invalid-credentials)
    (is (= 401 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "AUTH-INVALID-CREDENTIALS" (getf error-obj :|code|)))
      (is (string= "Invalid credentials" (getf error-obj :|message|))))))

(test auth-error-response-session-expired
  "セッション期限切れエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :session-expired)
    (is (= 401 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "AUTH-SESSION-EXPIRED" (getf error-obj :|code|))))))

(test auth-error-response-permission-denied
  "権限不足エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :permission-denied)
    (is (= 403 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "AUTH-PERMISSION-DENIED" (getf error-obj :|code|))))))

(test auth-error-response-with-custom-message
  "カスタムメッセージ付き認証エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :required "Please login to continue")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "Please login to continue" (getf error-obj :|message|))))))

(test auth-error-response-unknown-type
  "不明な認証エラータイプ"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :unknown-type)
    (is (= 401 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "AUTH-ERROR" (getf error-obj :|code|))))))

;;; resource-error-response のテスト

(test resource-error-response-not-found
  "リソース未検出エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :not-found
                                                   :resource-type "post")
    (is (= 404 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "RESOURCE-NOT-FOUND" (getf error-obj :|code|)))
      (is (string= "post not found" (getf error-obj :|message|))))))

(test resource-error-response-not-found-with-id
  "ID付きリソース未検出エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :not-found
                                                   :resource-type "user"
                                                   :resource-id 123)
    (is (= 404 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (string= "user" (getf details :|resource_type|)))
      (is (= 123 (getf details :|resource_id|))))))

(test resource-error-response-already-exists
  "リソース既存エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :already-exists
                                                   :resource-type "user")
    (is (= 409 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "RESOURCE-ALREADY-EXISTS" (getf error-obj :|code|)))
      (is (string= "user already exists" (getf error-obj :|message|))))))

(test resource-error-response-conflict
  "リソース競合エラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :conflict
                                                   :resource-type "post")
    (is (= 409 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "RESOURCE-CONFLICT" (getf error-obj :|code|))))))

(test resource-error-response-with-custom-message
  "カスタムメッセージ付きリソースエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :not-found
                                                   :message "The requested item was not found"
                                                   :resource-type "item")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "The requested item was not found" (getf error-obj :|message|))))))

;;; business-error-response のテスト

(test business-error-response-basic
  "ビジネスロジックエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:business-error-response :already-published
                                                   "Post is already published")
    (is (= 400 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "ALREADY-PUBLISHED" (getf error-obj :|code|)))
      (is (string= "Post is already published" (getf error-obj :|message|))))))

(test business-error-response-with-details
  "詳細情報付きビジネスロジックエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:business-error-response
       :invalid-state
       "Cannot transition from draft to archived"
       :details (list :|current_state| "draft"
                     :|requested_state| "archived"
                     :|allowed_states| (list "published" "deleted")))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (string= "draft" (getf details :|current_state|)))
      (is (string= "archived" (getf details :|requested_state|)))
      (is (listp (getf details :|allowed_states|))))))

;;; system-error-response のテスト

(test system-error-response-database
  "データベースエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :database)
    (is (= 500 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "SYSTEM-DATABASE-ERROR" (getf error-obj :|code|)))
      (is (string= "Database error occurred" (getf error-obj :|message|))))))

(test system-error-response-timeout
  "タイムアウトエラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :timeout)
    (is (= 504 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "SYSTEM-TIMEOUT" (getf error-obj :|code|))))))

(test system-error-response-unavailable
  "サービス利用不可エラーレスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :unavailable)
    (is (= 503 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "SYSTEM-SERVICE-UNAVAILABLE" (getf error-obj :|code|))))))

(test system-error-response-internal
  "内部エラーレスポンス（デフォルト）"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :unknown-type)
    (is (= 500 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "SYSTEM-INTERNAL-ERROR" (getf error-obj :|code|))))))

(test system-error-response-with-custom-message
  "カスタムメッセージ付きシステムエラー"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :database "Connection pool exhausted")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "Connection pool exhausted" (getf error-obj :|message|))))))

;;; condition-to-response のテスト

(test condition-to-response-validation-error
  "バリデーションエラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:validation-error
                                   :message "Username is required"
                                   :field "username")))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 400 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "VALIDATION-ERROR" (getf error-obj :|code|)))
        (is (string= "Username is required" (getf error-obj :|message|)))
        (is (string= "username" (getf error-obj :|field|)))))))

(test condition-to-response-authentication-error
  "認証エラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:authentication-error
                                   :code :auth-required
                                   :message "Authentication required")))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 401 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "AUTH-REQUIRED" (getf error-obj :|code|)))))))

(test condition-to-response-authorization-error
  "認可エラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:authorization-error
                                   :message "Permission denied")))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 403 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "AUTH-PERMISSION-DENIED" (getf error-obj :|code|)))))))

(test condition-to-response-resource-not-found-error
  "リソース未検出エラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:resource-not-found-error
                                   :message "Post not found"
                                   :resource-type "post"
                                   :resource-id 999)))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 404 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|))
             (details (getf error-obj :|details|)))
        (is (string= "RESOURCE-NOT-FOUND" (getf error-obj :|code|)))
        (is (string= "post" (getf details :|resource_type|)))
        (is (= 999 (getf details :|resource_id|)))))))

(test condition-to-response-resource-conflict-error
  "リソース競合エラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:resource-conflict-error
                                   :message "Username already exists"
                                   :resource-type "user")))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 409 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "RESOURCE-CONFLICT" (getf error-obj :|code|)))))))

(test condition-to-response-business-logic-error
  "ビジネスロジックエラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:business-logic-error
                                   :code :already-published
                                   :message "Post is already published"
                                   :details (list :|post_id| 42))))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 400 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|))
             (details (getf error-obj :|details|)))
        (is (string= "ALREADY-PUBLISHED" (getf error-obj :|code|)))
        (is (= 42 (getf details :|post_id|)))))))

(test condition-to-response-system-error
  "システムエラー条件からレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:system-error
                                   :message "Database connection failed")))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 500 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "SYSTEM-INTERNAL-ERROR" (getf error-obj :|code|)))))))

(test condition-to-response-generic-lisp-blog-error
  "汎用lisp-blog-errorからレスポンスへの変換"
  (let ((condition (make-condition 'lisp-blog.util.conditions:lisp-blog-error
                                   :code :custom-error
                                   :message "Custom error message"
                                   :details (list :|info| "test"))))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:condition-to-response condition)
      (is (= 500 status))
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= "CUSTOM-ERROR" (getf error-obj :|code|)))
        (is (string= "Custom error message" (getf error-obj :|message|)))))))

;;; エッジケーステスト

(test error-response-with-empty-message
  "空のエラーメッセージ"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :test-error "")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      (is (string= "" (getf error-obj :|message|))))))

(test error-response-with-special-characters
  "特殊文字を含むエラーメッセージ"
  (let ((message "Error: \"quoted\" and <tags> & ampersand"))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:make-error-response :test-error message)
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        ;; JSONエスケープが正しく行われていること
        (is (string= message (getf error-obj :|message|)))))))

(test error-response-with-unicode
  "Unicode文字を含むエラーメッセージ"
  (let ((message "エラー: 無効な入力です 😀"))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:make-error-response :test-error message)
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|)))
        (is (string= message (getf error-obj :|message|)))))))

(test error-response-with-nil-details
  "nilの詳細情報"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :test-error "Test" :details nil)
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; detailsフィールドが含まれていないこと
      (is (not (member :|details| error-obj))))))

(test error-response-with-empty-details
  "空の詳細情報plist"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:make-error-response :test-error "Test" :details (list))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      ;; 空のリストはdetailsに含まれる
      (is (listp details))
      ;; 空のリストであること
      (is (null details)))))
