(in-package :lisp-blog-test)

;;; Conditionsのテストスイート

(def-suite conditions-tests
  :in lisp-blog-test-suite
  :description "Common Lisp Conditionシステムのテスト")

(in-suite conditions-tests)

;;; 基底条件（lisp-blog-error）のテスト

(test lisp-blog-error-basic
  "基底エラー条件の基本的な生成と読み取り"
  (let ((err (make-condition 'lisp-blog.util.conditions:lisp-blog-error
                             :code :test-error
                             :message "Test error message")))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :test-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Test error message" (lisp-blog.util.conditions:error-message err)))
    (is (null (lisp-blog.util.conditions:error-details err)))))

(test lisp-blog-error-with-details
  "詳細情報付きエラー条件"
  (let ((err (make-condition 'lisp-blog.util.conditions:lisp-blog-error
                             :code :test-error
                             :message "Error with details"
                             :details (list :|info| "additional info"
                                           :|count| 42))))
    (is (eq :test-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Error with details" (lisp-blog.util.conditions:error-message err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (string= "additional info" (getf details :|info|)))
      (is (= 42 (getf details :|count|))))))

(test lisp-blog-error-print-object
  "エラー条件の文字列表現"
  (let ((err (make-condition 'lisp-blog.util.conditions:lisp-blog-error
                             :code :test-error
                             :message "Test message")))
    (let ((printed (princ-to-string err)))
      ;; プリント表現にエラーコードとメッセージが含まれていること
      (is (search "TEST-ERROR" (string-upcase printed)))
      (is (search "Test message" printed)))))

;;; バリデーションエラーのテスト

(test validation-error-basic
  "バリデーションエラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:validation-error
                             :message "Username is required"
                             :field "username")))
    (is (typep err 'lisp-blog.util.conditions:validation-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :validation-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Username is required" (lisp-blog.util.conditions:error-message err)))
    (is (string= "username" (lisp-blog.util.conditions:error-field err)))))

(test validation-error-without-field
  "フィールド指定なしのバリデーションエラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:validation-error
                             :message "Invalid input")))
    (is (eq :validation-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Invalid input" (lisp-blog.util.conditions:error-message err)))
    (is (null (lisp-blog.util.conditions:error-field err)))))

(test validation-error-with-details
  "詳細情報付きバリデーションエラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:validation-error
                             :message "Password too short"
                             :field "password"
                             :details (list :|min_length| 8
                                           :|actual_length| 5))))
    (is (string= "password" (lisp-blog.util.conditions:error-field err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (= 8 (getf details :|min_length|)))
      (is (= 5 (getf details :|actual_length|))))))

;;; 認証エラーのテスト

(test authentication-error-basic
  "認証エラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:authentication-error
                             :code :auth-required
                             :message "Authentication required")))
    (is (typep err 'lisp-blog.util.conditions:authentication-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :auth-required (lisp-blog.util.conditions:error-code err)))
    (is (string= "Authentication required" (lisp-blog.util.conditions:error-message err)))))

(test authentication-error-invalid-credentials
  "認証情報不正エラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:authentication-error
                             :code :auth-invalid-credentials
                             :message "Invalid username or password")))
    (is (eq :auth-invalid-credentials (lisp-blog.util.conditions:error-code err)))
    (is (string= "Invalid username or password" (lisp-blog.util.conditions:error-message err)))))

(test authentication-error-session-expired
  "セッション期限切れエラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:authentication-error
                             :code :auth-session-expired
                             :message "Session expired")))
    (is (eq :auth-session-expired (lisp-blog.util.conditions:error-code err)))))

;;; 認可エラーのテスト

(test authorization-error-basic
  "認可エラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:authorization-error
                             :message "Permission denied")))
    (is (typep err 'lisp-blog.util.conditions:authorization-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :authorization-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Permission denied" (lisp-blog.util.conditions:error-message err)))))

(test authorization-error-with-details
  "詳細情報付き認可エラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:authorization-error
                             :message "Cannot edit other user's post"
                             :details (list :|post_id| 123
                                           :|owner_id| 1
                                           :|current_user_id| 2))))
    (is (eq :authorization-error (lisp-blog.util.conditions:error-code err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (= 123 (getf details :|post_id|)))
      (is (= 1 (getf details :|owner_id|)))
      (is (= 2 (getf details :|current_user_id|))))))

;;; リソース未検出エラーのテスト

(test resource-not-found-error-basic
  "リソース未検出エラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:resource-not-found-error
                             :message "Post not found"
                             :resource-type "post"
                             :resource-id 999)))
    (is (typep err 'lisp-blog.util.conditions:resource-not-found-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :resource-not-found (lisp-blog.util.conditions:error-code err)))
    (is (string= "Post not found" (lisp-blog.util.conditions:error-message err)))
    (is (string= "post" (lisp-blog.util.conditions:resource-type err)))
    (is (= 999 (lisp-blog.util.conditions:resource-id err)))))

(test resource-not-found-error-without-id
  "ID指定なしのリソース未検出エラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:resource-not-found-error
                             :message "User not found"
                             :resource-type "user")))
    (is (eq :resource-not-found (lisp-blog.util.conditions:error-code err)))
    (is (string= "user" (lisp-blog.util.conditions:resource-type err)))
    (is (null (lisp-blog.util.conditions:resource-id err)))))

(test resource-not-found-error-default-resource-type
  "デフォルトのリソースタイプ"
  (let ((err (make-condition 'lisp-blog.util.conditions:resource-not-found-error
                             :message "Resource not found")))
    (is (string= "resource" (lisp-blog.util.conditions:resource-type err)))))

;;; リソース競合エラーのテスト

(test resource-conflict-error-basic
  "リソース競合エラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:resource-conflict-error
                             :message "Username already exists"
                             :resource-type "user")))
    (is (typep err 'lisp-blog.util.conditions:resource-conflict-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :resource-conflict (lisp-blog.util.conditions:error-code err)))
    (is (string= "Username already exists" (lisp-blog.util.conditions:error-message err)))
    (is (string= "user" (lisp-blog.util.conditions:resource-type err)))))

(test resource-conflict-error-with-details
  "詳細情報付きリソース競合エラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:resource-conflict-error
                             :message "Username 'admin' already exists"
                             :resource-type "user"
                             :details (list :|username| "admin"
                                           :|existing_user_id| 1))))
    (is (eq :resource-conflict (lisp-blog.util.conditions:error-code err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (string= "admin" (getf details :|username|)))
      (is (= 1 (getf details :|existing_user_id|))))))

;;; ビジネスロジックエラーのテスト

(test business-logic-error-basic
  "ビジネスロジックエラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:business-logic-error
                             :code :already-published
                             :message "Post is already published")))
    (is (typep err 'lisp-blog.util.conditions:business-logic-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :already-published (lisp-blog.util.conditions:error-code err)))
    (is (string= "Post is already published" (lisp-blog.util.conditions:error-message err)))))

(test business-logic-error-custom-codes
  "各種ビジネスロジックエラーコード"
  (let ((err1 (make-condition 'lisp-blog.util.conditions:business-logic-error
                              :code :already-draft
                              :message "Post is already in draft state"))
        (err2 (make-condition 'lisp-blog.util.conditions:business-logic-error
                              :code :invalid-state
                              :message "Invalid post state transition")))
    (is (eq :already-draft (lisp-blog.util.conditions:error-code err1)))
    (is (eq :invalid-state (lisp-blog.util.conditions:error-code err2)))))

(test business-logic-error-with-details
  "詳細情報付きビジネスロジックエラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:business-logic-error
                             :code :already-published
                             :message "Cannot publish already published post"
                             :details (list :|post_id| 42
                                           :|current_status| "published"
                                           :|requested_status| "published"))))
    (is (eq :already-published (lisp-blog.util.conditions:error-code err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (= 42 (getf details :|post_id|)))
      (is (string= "published" (getf details :|current_status|)))
      (is (string= "published" (getf details :|requested_status|))))))

;;; システムエラーのテスト

(test system-error-basic
  "システムエラーの基本的な生成"
  (let ((err (make-condition 'lisp-blog.util.conditions:system-error
                             :message "Database connection failed")))
    (is (typep err 'lisp-blog.util.conditions:system-error))
    (is (typep err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (eq :system-error (lisp-blog.util.conditions:error-code err)))
    (is (string= "Database connection failed" (lisp-blog.util.conditions:error-message err)))))

(test system-error-with-details
  "詳細情報付きシステムエラー"
  (let ((err (make-condition 'lisp-blog.util.conditions:system-error
                             :message "Database timeout"
                             :details (list :|timeout_seconds| 30
                                           :|query| "SELECT * FROM posts"))))
    (is (eq :system-error (lisp-blog.util.conditions:error-code err)))
    (let ((details (lisp-blog.util.conditions:error-details err)))
      (is (= 30 (getf details :|timeout_seconds|)))
      (is (string= "SELECT * FROM posts" (getf details :|query|))))))

;;; エラー条件の継承関係テスト

(test condition-inheritance
  "エラー条件の継承関係が正しいこと"
  (let ((base-err (make-condition 'lisp-blog.util.conditions:lisp-blog-error
                                  :code :test
                                  :message "test"))
        (val-err (make-condition 'lisp-blog.util.conditions:validation-error
                                 :message "test"))
        (auth-err (make-condition 'lisp-blog.util.conditions:authentication-error
                                  :code :auth-required
                                  :message "test"))
        (authz-err (make-condition 'lisp-blog.util.conditions:authorization-error
                                   :message "test"))
        (res-not-found (make-condition 'lisp-blog.util.conditions:resource-not-found-error
                                       :message "test"))
        (res-conflict (make-condition 'lisp-blog.util.conditions:resource-conflict-error
                                      :message "test"))
        (biz-err (make-condition 'lisp-blog.util.conditions:business-logic-error
                                 :code :test
                                 :message "test"))
        (sys-err (make-condition 'lisp-blog.util.conditions:system-error
                                 :message "test")))

    ;; すべてのエラーはlisp-blog-errorを継承している
    (is (typep base-err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep val-err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep auth-err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep authz-err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep res-not-found 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep res-conflict 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep biz-err 'lisp-blog.util.conditions:lisp-blog-error))
    (is (typep sys-err 'lisp-blog.util.conditions:lisp-blog-error))

    ;; すべてのエラーはerrorを継承している
    (is (typep base-err 'error))
    (is (typep val-err 'error))
    (is (typep auth-err 'error))))

;;; エラー条件のシグナル（発生）テスト

(test signal-validation-error
  "バリデーションエラーのシグナル"
  (signals lisp-blog.util.conditions:validation-error
    (error 'lisp-blog.util.conditions:validation-error
           :message "Test validation error"
           :field "test-field")))

(test signal-authentication-error
  "認証エラーのシグナル"
  (signals lisp-blog.util.conditions:authentication-error
    (error 'lisp-blog.util.conditions:authentication-error
           :code :auth-required
           :message "Test auth error")))

(test signal-authorization-error
  "認可エラーのシグナル"
  (signals lisp-blog.util.conditions:authorization-error
    (error 'lisp-blog.util.conditions:authorization-error
           :message "Test authorization error")))

(test signal-resource-not-found-error
  "リソース未検出エラーのシグナル"
  (signals lisp-blog.util.conditions:resource-not-found-error
    (error 'lisp-blog.util.conditions:resource-not-found-error
           :message "Test not found"
           :resource-type "test"
           :resource-id 999)))

(test signal-resource-conflict-error
  "リソース競合エラーのシグナル"
  (signals lisp-blog.util.conditions:resource-conflict-error
    (error 'lisp-blog.util.conditions:resource-conflict-error
           :message "Test conflict"
           :resource-type "test")))

(test signal-business-logic-error
  "ビジネスロジックエラーのシグナル"
  (signals lisp-blog.util.conditions:business-logic-error
    (error 'lisp-blog.util.conditions:business-logic-error
           :code :test-business-error
           :message "Test business error")))

(test signal-system-error
  "システムエラーのシグナル"
  (signals lisp-blog.util.conditions:system-error
    (error 'lisp-blog.util.conditions:system-error
           :message "Test system error")))

;;; handler-caseを使ったエラーハンドリングテスト

(test handler-case-validation-error
  "handler-caseでのバリデーションエラーキャッチ"
  (let ((caught nil)
        (message nil))
    (handler-case
        (error 'lisp-blog.util.conditions:validation-error
               :message "Test error"
               :field "test-field")
      (lisp-blog.util.conditions:validation-error (e)
        (setf caught t)
        (setf message (lisp-blog.util.conditions:error-message e))))
    (is (eq t caught))
    (is (string= "Test error" message))))

(test handler-case-catch-parent-condition
  "親条件でサブ条件をキャッチできること"
  (let ((caught nil))
    (handler-case
        (error 'lisp-blog.util.conditions:validation-error
               :message "Test error")
      ;; lisp-blog-errorで全てのサブエラーをキャッチできる
      (lisp-blog.util.conditions:lisp-blog-error (e)
        (setf caught t)))
    (is (eq t caught))))

(test handler-case-multiple-conditions
  "複数の条件を個別にハンドリング"
  (let ((auth-caught nil)
        (val-caught nil))

    ;; 認証エラーのキャッチ
    (handler-case
        (error 'lisp-blog.util.conditions:authentication-error
               :code :auth-required
               :message "Auth required")
      (lisp-blog.util.conditions:authentication-error (e)
        (setf auth-caught t))
      (lisp-blog.util.conditions:validation-error (e)
        (setf val-caught t)))
    (is (eq t auth-caught))
    (is (eq nil val-caught))

    ;; バリデーションエラーのキャッチ
    (setf auth-caught nil
          val-caught nil)
    (handler-case
        (error 'lisp-blog.util.conditions:validation-error
               :message "Validation failed")
      (lisp-blog.util.conditions:authentication-error (e)
        (setf auth-caught t))
      (lisp-blog.util.conditions:validation-error (e)
        (setf val-caught t)))
    (is (eq nil auth-caught))
    (is (eq t val-caught))))
