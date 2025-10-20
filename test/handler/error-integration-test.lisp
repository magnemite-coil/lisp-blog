(in-package :lisp-blog-test)

;;; Handler層のエラーハンドリング統合テストスイート

(def-suite handler-error-integration-tests
  :in lisp-blog-test-suite
  :description "Handler層とService層のConditionベースエラーハンドリング統合テスト")

(in-suite handler-error-integration-tests)

;;; エラーレスポンス形式の検証ヘルパー

(defun verify-error-response (json expected-status expected-code expected-message-substring)
  "エラーレスポンスの形式を検証

   json: JSONレスポンス文字列
   expected-status: 期待されるHTTPステータスコード
   expected-code: 期待されるエラーコード
   expected-message-substring: メッセージに含まれるべき文字列"
  (let ((parsed (jonathan:parse json :as :plist)))
    ;; success: false
    (is (not (getf parsed :|success|)))

    ;; error オブジェクトが存在
    (let ((error-obj (getf parsed :|error|)))
      (is (not (null error-obj)))

      ;; エラーコード
      (is (string= (string-upcase expected-code)
                   (getf error-obj :|code|)))

      ;; エラーメッセージ
      (when expected-message-substring
        (is (search expected-message-substring
                    (getf error-obj :|message|)
                    :test #'char-equal)))

      ;; エラーIDが含まれている
      (is (getf error-obj :|error_id|))
      (is (search "err_" (getf error-obj :|error_id|))))))

;;; Condition to Response 変換のテスト

(test condition-to-response-validation-error-integration
  "Service層のvalidation-errorがHandler層で適切にレスポンスに変換されること"
  (handler-case
      (lisp-blog.service.post:create-post 1 "" "Content")
    (lisp-blog.util.conditions:validation-error (e)
      (multiple-value-bind (json status headers)
          (lisp-blog.util.error:condition-to-response e)
        (is (= 400 status))
        (verify-error-response json 400 "VALIDATION-ERROR" "Invalid title")

        ;; フィールド名が含まれている
        (let* ((parsed (jonathan:parse json :as :plist))
               (error-obj (getf parsed :|error|)))
          (is (string= "title" (getf error-obj :|field|))))))))

(test condition-to-response-business-logic-error-integration
  "Service層のbusiness-logic-errorがHandler層で適切にレスポンスに変換されること"
  (with-test-db
    (let* ((user (create-test-user "bizuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Content"
                                  "published")))
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        (lisp-blog.util.conditions:business-logic-error (e)
          (multiple-value-bind (json status headers)
              (lisp-blog.util.error:condition-to-response e)
            (is (= 400 status))
            (verify-error-response json 400 "POST-ALREADY-PUBLISHED" "already published")))))))

;;; エラーレスポンス関数の統合テスト

(test validation-error-response-integration
  "バリデーションエラーレスポンスの完全な形式テスト"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response
       "Username must be 3-50 characters"
       :field "username"
       :details (list :|min_length| 3 :|max_length| 50 :|actual_length| 2))

    (is (= 400 status))
    (is (string= "application/json" (getf headers :content-type)))

    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; エラーコード
      (is (string= "VALIDATION-ERROR" (getf error-obj :|code|)))

      ;; エラーメッセージ
      (is (string= "Username must be 3-50 characters" (getf error-obj :|message|)))

      ;; フィールド名
      (is (string= "username" (getf error-obj :|field|)))

      ;; 詳細情報
      (let ((details (getf error-obj :|details|)))
        (is (= 3 (getf details :|min_length|)))
        (is (= 50 (getf details :|max_length|)))
        (is (= 2 (getf details :|actual_length|))))

      ;; エラーID
      (is (stringp (getf error-obj :|error_id|)))
      (is (search "err_" (getf error-obj :|error_id|))))))

(test auth-error-response-integration
  "認証エラーレスポンスの完全な形式テスト"
  ;; 認証が必要
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :required)
    (is (= 401 status))
    (verify-error-response json 401 "AUTH-REQUIRED" "Authentication required"))

  ;; 認証情報が不正
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :invalid-credentials)
    (is (= 401 status))
    (verify-error-response json 401 "AUTH-INVALID-CREDENTIALS" "Invalid credentials"))

  ;; セッション期限切れ
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :session-expired)
    (is (= 401 status))
    (verify-error-response json 401 "AUTH-SESSION-EXPIRED" "Session expired"))

  ;; 権限不足
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :permission-denied)
    (is (= 403 status))
    (verify-error-response json 403 "AUTH-PERMISSION-DENIED" "Permission denied")))

(test resource-error-response-integration
  "リソースエラーレスポンスの完全な形式テスト"
  ;; リソースが見つからない
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response
       :not-found
       :resource-type "post"
       :resource-id 999)
    (is (= 404 status))
    (verify-error-response json 404 "RESOURCE-NOT-FOUND" "post not found")

    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (string= "post" (getf details :|resource_type|)))
      (is (= 999 (getf details :|resource_id|)))))

  ;; リソースが既に存在
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response
       :already-exists
       :resource-type "user")
    (is (= 409 status))
    (verify-error-response json 409 "RESOURCE-ALREADY-EXISTS" "user already exists"))

  ;; リソース競合
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response
       :conflict
       :resource-type "post")
    (is (= 409 status))
    (verify-error-response json 409 "RESOURCE-CONFLICT" "post conflict")))

(test business-error-response-integration
  "ビジネスロジックエラーレスポンスの完全な形式テスト"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:business-error-response
       :post-already-published
       "Post is already published"
       :details (list :|post_id| 42 :|current_status| "published"))

    (is (= 400 status))
    (verify-error-response json 400 "POST-ALREADY-PUBLISHED" "already published")

    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|))
           (details (getf error-obj :|details|)))
      (is (= 42 (getf details :|post_id|)))
      (is (string= "published" (getf details :|current_status|))))))

(test system-error-response-integration
  "システムエラーレスポンスの完全な形式テスト"
  ;; データベースエラー
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :database)
    (is (= 500 status))
    (verify-error-response json 500 "SYSTEM-DATABASE-ERROR" "Database error"))

  ;; タイムアウト
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :timeout)
    (is (= 504 status))
    (verify-error-response json 504 "SYSTEM-TIMEOUT" "timeout"))

  ;; サービス利用不可
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :unavailable)
    (is (= 503 status))
    (verify-error-response json 503 "SYSTEM-SERVICE-UNAVAILABLE" "unavailable"))

  ;; 内部エラー
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :unknown)
    (is (= 500 status))
    (verify-error-response json 500 "SYSTEM-INTERNAL-ERROR" "Internal server error")))

;;; エラーID一意性テスト

(test error-id-uniqueness-across-responses
  "複数のエラーレスポンスで異なるエラーIDが生成されること"
  (let ((error-ids '()))
    ;; 複数のエラーレスポンスを生成
    (dotimes (i 20)
      (multiple-value-bind (json status headers)
          (lisp-blog.util.error:validation-error-response "Test error")
        (let* ((parsed (jonathan:parse json :as :plist))
               (error-obj (getf parsed :|error|))
               (error-id (getf error-obj :|error_id|)))
          (push error-id error-ids))))

    ;; すべてのエラーIDが一意であること
    (is (= 20 (length (remove-duplicates error-ids :test #'string=))))))

;;; エラーレスポンスのHTTPステータスコード検証

(test http-status-codes-correctness
  "各エラータイプで正しいHTTPステータスコードが返されること"
  ;; 4xx クライアントエラー
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response "Invalid input")
    (is (= 400 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :required)
    (is (= 401 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:auth-error-response :permission-denied)
    (is (= 403 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :not-found :resource-type "post")
    (is (= 404 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:resource-error-response :already-exists :resource-type "user")
    (is (= 409 status)))

  ;; 5xx サーバーエラー
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :database)
    (is (= 500 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :unavailable)
    (is (= 503 status)))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:system-error-response :timeout)
    (is (= 504 status))))

;;; Content-Typeヘッダーのテスト

(test error-response-content-type-header
  "すべてのエラーレスポンスで正しいContent-Typeヘッダーが設定されること"
  (flet ((check-content-type (response-fn &rest args)
           (multiple-value-bind (json status headers)
               (apply response-fn args)
             (is (string= "application/json" (getf headers :content-type))))))

    (check-content-type #'lisp-blog.util.error:validation-error-response "Test error")
    (check-content-type #'lisp-blog.util.error:auth-error-response :required)
    (check-content-type #'lisp-blog.util.error:resource-error-response :not-found :resource-type "post")
    (check-content-type #'lisp-blog.util.error:business-error-response :test-error "Test message")
    (check-content-type #'lisp-blog.util.error:system-error-response :database)))

;;; エラーレスポンスの階層的ハンドリングテスト

(test hierarchical-error-handling
  "親条件でサブ条件をキャッチし、適切なレスポンスが生成されること"
  (with-test-db
    (let* ((user (create-test-user "hieruser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Content"
                                  "published")))

      ;; lisp-blog-error（親条件）でbusiness-logic-error（子条件）をキャッチ
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        (lisp-blog.util.conditions:lisp-blog-error (e)
          (multiple-value-bind (json status headers)
              (lisp-blog.util.error:condition-to-response e)
            ;; 適切なレスポンスが生成される
            (is (= 400 status))
            (verify-error-response json 400 "POST-ALREADY-PUBLISHED" "already published")))))))

;;; 複数の異なるエラータイプの連続テスト

(test multiple-error-types-sequence
  "異なるエラータイプを連続して発生させても正しく処理されること"
  (with-test-db
    (let* ((user (create-test-user "sequser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Content"
                                  "draft")))

      ;; 1. バリデーションエラー
      (handler-case
          (lisp-blog.service.post:create-post 1 "" "Content")
        (lisp-blog.util.conditions:validation-error (e)
          (multiple-value-bind (json status headers)
              (lisp-blog.util.error:condition-to-response e)
            (is (= 400 status))
            (verify-error-response json 400 "VALIDATION-ERROR" "Invalid title"))))

      ;; 2. ビジネスロジックエラー（公開）
      (let ((published-post (lisp-blog.service.post:publish-draft post)))
        (handler-case
            (lisp-blog.service.post:publish-draft published-post)
          (lisp-blog.util.conditions:business-logic-error (e)
            (multiple-value-bind (json status headers)
                (lisp-blog.util.error:condition-to-response e)
              (is (= 400 status))
              (verify-error-response json 400 "POST-ALREADY-PUBLISHED" "already published")))))

      ;; 3. ビジネスロジックエラー（下書き化）
      (let ((draft-post (lisp-blog.service.post:unpublish-post post)))
        (handler-case
            (lisp-blog.service.post:unpublish-post draft-post)
          (lisp-blog.util.conditions:business-logic-error (e)
            (multiple-value-bind (json status headers)
                (lisp-blog.util.error:condition-to-response e)
              (is (= 400 status))
              (verify-error-response json 400 "POST-ALREADY-DRAFT" "already a draft"))))))))

;;; JSONパース可能性テスト

(test error-response-json-parseable
  "すべてのエラーレスポンスが有効なJSONとしてパース可能であること"
  (flet ((check-json-valid (response-fn &rest args)
           (multiple-value-bind (json status headers)
               (apply response-fn args)
             ;; パースでエラーが発生しないこと
             (finishes (jonathan:parse json :as :plist))
             ;; パース結果がplistであること
             (is (listp (jonathan:parse json :as :plist))))))

    (check-json-valid #'lisp-blog.util.error:validation-error-response "Test error" :field "test")
    (check-json-valid #'lisp-blog.util.error:auth-error-response :required)
    (check-json-valid #'lisp-blog.util.error:resource-error-response :not-found :resource-type "post" :resource-id 123)
    (check-json-valid #'lisp-blog.util.error:business-error-response :test-error "Test" :details (list :|key| "value"))
    (check-json-valid #'lisp-blog.util.error:system-error-response :database)))

;;; 特殊文字を含むエラーメッセージのテスト

(test error-response-with-special-characters
  "特殊文字を含むエラーメッセージが正しくエスケープされること"
  (let ((message "Error: \"quoted\" and <tags> & ampersand 日本語"))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.error:validation-error-response message)

      ;; JSONとしてパース可能
      (let* ((parsed (jonathan:parse json :as :plist))
             (error-obj (getf parsed :|error|))
             (parsed-message (getf error-obj :|message|)))
        ;; メッセージが正しく復元される
        (is (string= message parsed-message))))))

;;; エラーレスポンスのフィールド存在確認

(test error-response-required-fields
  "エラーレスポンスに必須フィールドが含まれていること"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response "Test error")

    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))

      ;; 必須フィールド
      (is (not (null (getf parsed :|success|))))  ; success フィールド
      (is (not (null error-obj)))  ; error オブジェクト
      (is (stringp (getf error-obj :|code|)))  ; code
      (is (stringp (getf error-obj :|message|)))  ; message
      (is (stringp (getf error-obj :|error_id|))))))  ; error_id

(test error-response-optional-fields
  "エラーレスポンスにオプションフィールドが適切に含まれること"
  ;; フィールド名なし
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response "Test error")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; fieldは含まれない
      (is (not (member :|field| error-obj)))))

  ;; フィールド名あり
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:validation-error-response "Test error" :field "username")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; fieldが含まれる
      (is (string= "username" (getf error-obj :|field|)))))

  ;; 詳細情報なし
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:business-error-response :test-error "Test")
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; detailsは含まれない
      (is (not (member :|details| error-obj)))))

  ;; 詳細情報あり
  (multiple-value-bind (json status headers)
      (lisp-blog.util.error:business-error-response :test-error "Test" :details (list :|key| "value"))
    (let* ((parsed (jonathan:parse json :as :plist))
           (error-obj (getf parsed :|error|)))
      ;; detailsが含まれる
      (is (not (null (getf error-obj :|details|)))))))
