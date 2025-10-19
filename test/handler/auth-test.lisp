(in-package :lisp-blog-test)

;;; 認証ハンドラーのテストスイート

(def-suite auth-handler-tests
  :in lisp-blog-test-suite
  :description "認証ハンドラーのテスト")

(in-suite auth-handler-tests)

;;; ヘルパー関数のテスト

(test get-json-param-basic
  "JSONパラメータから値を取得"
  (let ((params '(("username" . "testuser")
                  ("password" . "password123")
                  ("email" . "test@example.com"))))
    (is (string= "testuser" (lisp-blog.handler.auth::get-json-param params "username")))
    (is (string= "password123" (lisp-blog.handler.auth::get-json-param params "password")))
    (is (string= "test@example.com" (lisp-blog.handler.auth::get-json-param params "email")))))

(test get-json-param-nonexistent
  "存在しないキー"
  (let ((params '(("username" . "testuser"))))
    (is (null (lisp-blog.handler.auth::get-json-param params "nonexistent")))))

(test get-json-param-empty-params
  "空のパラメータ"
  (is (null (lisp-blog.handler.auth::get-json-param '() "username"))))

(test user-to-json-conversion
  "userオブジェクトをJSON用plistに変換"
  (with-empty-db
    (let* ((user (create-test-user :username "jsontest"))
           (json-data (lisp-blog.handler.auth::user-to-json user)))
      (is (= (lisp-blog.model.user:user-id user) (getf json-data :|id|)))
      (is (string= "jsontest" (getf json-data :|username|)))
      (is (not (null (getf json-data :|created_at|)))))))

(test set-session-cookie-format
  "セッションCookieの形式確認"
  (let ((cookie-str (lisp-blog.handler.auth::set-session-cookie "test-session-id-123")))
    ;; session_id が含まれる
    (is (search "session_id=test-session-id-123" cookie-str))
    ;; セキュリティ属性が含まれる
    (is (search "HttpOnly" cookie-str))
    (is (search "SameSite=Lax" cookie-str))
    (is (search "Path=/" cookie-str))
    ;; Max-Age が含まれる（7日間 = 604800秒）
    (is (search "Max-Age=604800" cookie-str))))

(test get-session-id-from-cookies-success
  "CookieからセッションIDを取得"
  (let ((cookies '(("session_id" . "abc123")
                   ("other_cookie" . "value"))))
    (is (string= "abc123" (lisp-blog.handler.auth::get-session-id-from-cookies cookies)))))

(test get-session-id-from-cookies-not-found
  "セッションIDがCookieに存在しない"
  (let ((cookies '(("other_cookie" . "value"))))
    (is (null (lisp-blog.handler.auth::get-session-id-from-cookies cookies)))))

(test get-session-id-from-cookies-empty
  "空のCookie"
  (is (null (lisp-blog.handler.auth::get-session-id-from-cookies '()))))

;;; 統合的な動作確認テスト
;;; 注: 実際のHTTPリクエストではなく、サービス層の動作を確認

(test register-flow-complete
  "ユーザー登録の完全なフロー確認"
  (with-empty-db
    ;; 登録サービスを呼び出し
    (let ((result (lisp-blog.service.auth:register-user "flowtest" "password123")))
      ;; 成功確認（戻り値に:successキーは無い）
      (let* ((user (getf result :user))
             (session-id (getf result :session-id))
             ;; JSON変換
             (json-data (lisp-blog.handler.auth::user-to-json user))
             ;; Cookie生成
             (cookie (lisp-blog.handler.auth::set-session-cookie session-id)))
        ;; JSON データ確認
        (is (not (null (getf json-data :|id|))))
        (is (string= "flowtest" (getf json-data :|username|)))
        ;; Cookie確認
        (is (search session-id cookie))
        ;; セッションからユーザー取得可能
        (let ((retrieved-user (lisp-blog.service.auth:get-user-by-session session-id)))
          (is (not (null retrieved-user)))
          (is (string= "flowtest" (lisp-blog.model.user:user-username retrieved-user))))))))

(test login-flow-complete
  "ログインの完全なフロー確認"
  (with-empty-db
    ;; まず登録
    (lisp-blog.service.auth:register-user "loginflow" "password123")
    ;; ログイン
    (let ((result (lisp-blog.service.auth:authenticate-user "loginflow" "password123")))
      ;; 成功確認（戻り値に:successキーは無い）
      (let* ((user (getf result :user))
             (session-id (getf result :session-id))
             (json-data (lisp-blog.handler.auth::user-to-json user))
             (cookie (lisp-blog.handler.auth::set-session-cookie session-id)))
        ;; JSON データ確認
        (is (string= "loginflow" (getf json-data :|username|)))
        ;; Cookie確認
        (is (search session-id cookie))))))

(test logout-flow-complete
  "ログアウトの完全なフロー確認"
  (with-empty-db
    ;; 登録
    (let* ((register-result (lisp-blog.service.auth:register-user "logoutflow" "password123"))
           (session-id (getf register-result :session-id)))
      ;; ログアウト前: セッションからユーザー取得可能
      (is (not (null (lisp-blog.service.auth:get-user-by-session session-id))))
      ;; ログアウト
      (lisp-blog.middleware.session:delete-session session-id)
      ;; ログアウト後: セッションからユーザー取得不可
      (is (null (lisp-blog.service.auth:get-user-by-session session-id))))))

(test me-flow-complete
  "現在のユーザー情報取得フロー確認"
  (with-empty-db
    (let* ((register-result (lisp-blog.service.auth:register-user "meflow" "password123"))
           (user (getf register-result :user))
           (session-id (getf register-result :session-id))
           ;; セッションからユーザー取得
           (retrieved-user (lisp-blog.service.auth:get-user-by-session session-id))
           ;; JSON変換
           (json-data (lisp-blog.handler.auth::user-to-json retrieved-user)))
      ;; ユーザー情報確認
      (is (not (null retrieved-user)))
      (is (string= "meflow" (getf json-data :|username|)))
      (is (= (lisp-blog.model.user:user-id user)
             (getf json-data :|id|))))))

;;; エラーハンドリングのテスト

(test register-flow-duplicate-username
  "重複ユーザー名でのエラー"
  (with-empty-db
    ;; 1人目登録
    (lisp-blog.service.auth:register-user "duplicate" "password123")
    ;; 2人目登録（同じユーザー名） - Conditionを投げる
    (signals lisp-blog.util.conditions:resource-conflict-error
      (lisp-blog.service.auth:register-user "duplicate" "password456"))))

(test login-flow-invalid-credentials
  "無効な認証情報でのエラー"
  (with-empty-db
    ;; ユーザー登録
    (lisp-blog.service.auth:register-user "credtest" "password123")
    ;; 間違ったパスワードでログイン - Conditionを投げる
    (signals lisp-blog.util.conditions:authentication-error
      (lisp-blog.service.auth:authenticate-user "credtest" "wrongpassword"))))

(test me-flow-invalid-session
  "無効なセッションでのエラー"
  (with-empty-db
    ;; 無効なセッションIDでユーザー取得
    (let ((user (lisp-blog.service.auth:get-user-by-session "invalid-session-id")))
      (is (null user)))))

;;; Cookie処理のエッジケース

(test cookie-multiple-cookies
  "複数のCookieが存在する場合"
  (let ((cookies '(("csrf_token" . "token123")
                   ("session_id" . "my-session")
                   ("preferences" . "dark-mode"))))
    (is (string= "my-session" (lisp-blog.handler.auth::get-session-id-from-cookies cookies)))))

(test cookie-special-characters
  "特殊文字を含むセッションID"
  (let ((session-id "session-123_abc-xyz"))
    (let ((cookie (lisp-blog.handler.auth::set-session-cookie session-id)))
      (is (search session-id cookie)))))

(test json-param-numeric-value
  "数値を含むJSONパラメータ"
  (let ((params '(("age" . 25)
                  ("count" . 100))))
    (is (= 25 (lisp-blog.handler.auth::get-json-param params "age")))
    (is (= 100 (lisp-blog.handler.auth::get-json-param params "count")))))

(test user-to-json-timestamp-format
  "タイムスタンプのフォーマット確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (json-data (lisp-blog.handler.auth::user-to-json user))
           (created-at-str (getf json-data :|created_at|)))
      ;; タイムスタンプが文字列として変換されている
      (is (stringp created-at-str))
      ;; 空でない
      (is (> (length created-at-str) 0)))))
