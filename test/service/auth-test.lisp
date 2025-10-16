(in-package :lisp-blog-test)

;;; 認証サービスのテストスイート

(def-suite auth-service-tests
  :in lisp-blog-test-suite
  :description "認証サービスのテスト")

(in-suite auth-service-tests)

;;; バリデーションのテスト

(test validate-username-valid
  "有効なユーザー名"
  (is (lisp-blog.service.auth:validate-username "user123"))
  (is (lisp-blog.service.auth:validate-username "test_user"))
  (is (lisp-blog.service.auth:validate-username "ABC"))
  (is (lisp-blog.service.auth:validate-username "user_123_test")))

(test validate-username-invalid-length
  "無効な長さのユーザー名"
  ;; 2文字（短すぎる）
  (is (not (lisp-blog.service.auth:validate-username "ab")))
  ;; 51文字（長すぎる）
  (is (not (lisp-blog.service.auth:validate-username
            (make-string 51 :initial-element #\a)))))

(test validate-username-invalid-characters
  "無効な文字を含むユーザー名"
  (is (not (lisp-blog.service.auth:validate-username "user-name")))  ; ハイフン
  (is (not (lisp-blog.service.auth:validate-username "user.name")))  ; ドット
  (is (not (lisp-blog.service.auth:validate-username "user name")))  ; スペース
  (is (not (lisp-blog.service.auth:validate-username "user@name")))  ; @記号
  (is (not (lisp-blog.service.auth:validate-username "ユーザー"))))  ; Unicode

(test validate-password-valid
  "有効なパスワード"
  (is (lisp-blog.service.auth:validate-password "password"))
  (is (lisp-blog.service.auth:validate-password "12345678"))
  (is (lisp-blog.service.auth:validate-password "P@ssw0rd!"))
  (is (lisp-blog.service.auth:validate-password (make-string 255 :initial-element #\x))))

(test validate-password-invalid-length
  "無効な長さのパスワード"
  ;; 7文字（短すぎる）
  (is (not (lisp-blog.service.auth:validate-password "1234567")))
  ;; 256文字（長すぎる）
  (is (not (lisp-blog.service.auth:validate-password
            (make-string 256 :initial-element #\a)))))

;;; ユーザー登録のテスト

(test register-user-success
  "正常なユーザー登録"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:register-user "testuser" "password123")))
      (is (getf result :success))
      (is (not (null (getf result :user))))
      (is (not (null (getf result :session-id))))
      ;; ユーザー情報の確認
      (let ((user (getf result :user)))
        (is (string= "testuser" (lisp-blog.model.user:user-username user)))
        ;; パスワードはハッシュ化されている
        (is (not (string= "password123" (lisp-blog.model.user:user-password user))))))))

(test register-user-invalid-username
  "無効なユーザー名での登録"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:register-user "ab" "password123")))
      (is (not (getf result :success)))
      (is (string= "invalid-username" (getf result :error))))))

(test register-user-invalid-password
  "無効なパスワードでの登録"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:register-user "testuser" "short")))
      (is (not (getf result :success)))
      (is (string= "invalid-password" (getf result :error))))))

(test register-user-duplicate-username
  "重複ユーザー名での登録"
  (with-empty-db
    ;; 1人目のユーザー登録
    (lisp-blog.service.auth:register-user "duplicate" "password123")
    ;; 2人目の登録（同じユーザー名）
    (let ((result (lisp-blog.service.auth:register-user "duplicate" "password456")))
      (is (not (getf result :success)))
      (is (string= "username-exists" (getf result :error))))))

(test register-user-session-creation
  "登録時にセッションが作成される"
  (with-empty-db
    (let* ((result (lisp-blog.service.auth:register-user "sessiontest" "password123"))
           (session-id (getf result :session-id))
           (user-id (lisp-blog.model.user:user-id (getf result :user))))
      (is (not (null session-id)))
      ;; セッションからユーザーIDを取得できる
      (is (= user-id (lisp-blog.middleware.session:get-session-user-id session-id))))))

;;; ユーザー認証のテスト

(test authenticate-user-success
  "正常な認証（ログイン）"
  (with-empty-db
    ;; ユーザー登録
    (lisp-blog.service.auth:register-user "authtest" "password123")
    ;; 認証
    (let ((result (lisp-blog.service.auth:authenticate-user "authtest" "password123")))
      (is (getf result :success))
      (is (not (null (getf result :user))))
      (is (not (null (getf result :session-id))))
      (is (string= "authtest" (lisp-blog.model.user:user-username (getf result :user)))))))

(test authenticate-user-wrong-password
  "間違ったパスワードでの認証"
  (with-empty-db
    ;; ユーザー登録
    (lisp-blog.service.auth:register-user "authtest" "password123")
    ;; 間違ったパスワードで認証
    (let ((result (lisp-blog.service.auth:authenticate-user "authtest" "wrongpassword")))
      (is (not (getf result :success)))
      (is (string= "invalid-credentials" (getf result :error))))))

(test authenticate-user-nonexistent
  "存在しないユーザーでの認証"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:authenticate-user "nonexistent" "password123")))
      (is (not (getf result :success)))
      (is (string= "invalid-credentials" (getf result :error))))))

(test authenticate-user-session-creation
  "認証時にセッションが作成される"
  (with-empty-db
    ;; ユーザー登録
    (lisp-blog.service.auth:register-user "sessiontest" "password123")
    ;; 認証
    (let* ((result (lisp-blog.service.auth:authenticate-user "sessiontest" "password123"))
           (session-id (getf result :session-id))
           (user-id (lisp-blog.model.user:user-id (getf result :user))))
      (is (not (null session-id)))
      ;; セッションからユーザーIDを取得できる
      (is (= user-id (lisp-blog.middleware.session:get-session-user-id session-id))))))

(test authenticate-user-multiple-sessions
  "複数回ログインで複数のセッションが作成される"
  (with-empty-db
    ;; ユーザー登録
    (lisp-blog.service.auth:register-user "multitest" "password123")
    ;; 1回目のログイン
    (let* ((result1 (lisp-blog.service.auth:authenticate-user "multitest" "password123"))
           (session-id1 (getf result1 :session-id))
           ;; 2回目のログイン
           (result2 (lisp-blog.service.auth:authenticate-user "multitest" "password123"))
           (session-id2 (getf result2 :session-id)))
      ;; 異なるセッションIDが生成される
      (is (not (string= session-id1 session-id2)))
      ;; 両方のセッションが有効
      (is (not (null (lisp-blog.middleware.session:get-session-user-id session-id1))))
      (is (not (null (lisp-blog.middleware.session:get-session-user-id session-id2)))))))

;;; ユーザー取得のテスト

(test get-user-by-id-success
  "IDでユーザーを取得"
  (with-empty-db
    (let* ((register-result (lisp-blog.service.auth:register-user "gettest" "password123"))
           (user (getf register-result :user))
           (user-id (lisp-blog.model.user:user-id user))
           (retrieved-user (lisp-blog.service.auth:get-user-by-id user-id)))
      (is (not (null retrieved-user)))
      (is (= user-id (lisp-blog.model.user:user-id retrieved-user)))
      (is (string= "gettest" (lisp-blog.model.user:user-username retrieved-user))))))

(test get-user-by-id-nonexistent
  "存在しないIDでのユーザー取得"
  (with-empty-db
    (let ((user (lisp-blog.service.auth:get-user-by-id 99999)))
      (is (null user)))))

(test get-user-by-session-success
  "セッションIDでユーザーを取得"
  (with-empty-db
    (let* ((register-result (lisp-blog.service.auth:register-user "sessionget" "password123"))
           (session-id (getf register-result :session-id))
           (user (lisp-blog.service.auth:get-user-by-session session-id)))
      (is (not (null user)))
      (is (string= "sessionget" (lisp-blog.model.user:user-username user))))))

(test get-user-by-session-invalid
  "無効なセッションIDでのユーザー取得"
  (with-empty-db
    (let ((user (lisp-blog.service.auth:get-user-by-session "invalid-session-id")))
      (is (null user)))))

(test get-user-by-session-after-deletion
  "削除されたセッションでのユーザー取得"
  (with-empty-db
    (let* ((register-result (lisp-blog.service.auth:register-user "deletetest" "password123"))
           (session-id (getf register-result :session-id)))
      ;; セッション削除
      (lisp-blog.middleware.session:delete-session session-id)
      ;; セッションからユーザーを取得（失敗するはず）
      (let ((user (lisp-blog.service.auth:get-user-by-session session-id)))
        (is (null user))))))

;;; エッジケースのテスト

(test register-user-max-length-username
  "最大長（50文字）のユーザー名"
  (with-empty-db
    (let* ((long-username (make-string 50 :initial-element #\a))
           (result (lisp-blog.service.auth:register-user long-username "password123")))
      (is (getf result :success))
      (is (string= long-username
                   (lisp-blog.model.user:user-username (getf result :user)))))))

(test register-user-min-length-username
  "最小長（3文字）のユーザー名"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:register-user "abc" "password123")))
      (is (getf result :success))
      (is (string= "abc"
                   (lisp-blog.model.user:user-username (getf result :user)))))))

(test register-user-max-length-password
  "最大長（255文字）のパスワード"
  (with-empty-db
    (let* ((long-password (make-string 255 :initial-element #\x))
           (result (lisp-blog.service.auth:register-user "longpassuser" long-password)))
      (is (getf result :success)))))

(test register-user-min-length-password
  "最小長（8文字）のパスワード"
  (with-empty-db
    (let ((result (lisp-blog.service.auth:register-user "minpassuser" "12345678")))
      (is (getf result :success)))))
