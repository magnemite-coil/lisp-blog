(in-package :lisp-blog-test)

;;; テスト用i18n初期化（翻訳なしのダミー）

(defun setup-test-i18n ()
  "テスト用のi18n設定（最小限）"
  ;; *translations*をクリア
  (clrhash lisp-blog::*translations*)
  ;; 必要最小限の翻訳を手動で設定
  (setf (gethash "ja.validation.username.format" lisp-blog::*translations*)
        "ユーザー名は3-50文字の英数字、アンダースコア、ハイフンである必要があります")
  (setf (gethash "ja.validation.email.format" lisp-blog::*translations*)
        "有効なメールアドレス形式ではありません")
  (setf (gethash "ja.validation.password.length" lisp-blog::*translations*)
        "パスワードは8-100文字である必要があります")
  (setf (gethash "ja.validation.username.taken" lisp-blog::*translations*)
        "このユーザー名は既に使用されています")
  (setf (gethash "ja.validation.email.taken" lisp-blog::*translations*)
        "このメールアドレスは既に使用されています"))

;; テスト環境の初期化
(setup-test-i18n)

;;; テスト用データベース接続設定

(defparameter *test-db-spec*
  '("lisp_blog_test" "bloguser" "" "localhost")
  "テスト用データベース接続仕様")

(defun setup-test-db ()
  "テスト用データベースのテーブルを初期化"
  (let ((lisp-blog::*db-spec* *test-db-spec*))
    (lisp-blog::init-db)
    (lisp-blog::migrate-database)))

(defun teardown-test-db ()
  "テスト用データベースの全テーブルを削除"
  (let ((lisp-blog::*db-spec* *test-db-spec*))
    (lisp-blog::drop-all-tables)))

(defun clean-test-db ()
  "テスト用データベースの全データを削除（テーブル構造は保持）"
  ;; run-testsで既に*db-spec*が設定されているので、ここでは変更しない
  (postmodern:with-connection *test-db-spec*
    (handler-case
        (postmodern:execute "TRUNCATE users, sessions, posts, comments RESTART IDENTITY CASCADE")
      (error (e)
        ;; テーブルが存在しない場合は何もしない（run-testsで初期化される）
        (format t "Warning: Could not truncate tables: ~A~%" e)
        nil))))

;;; FiveAM フィクスチャ定義

(def-fixture with-empty-db ()
  "空のデータベースでテストを実行するフィクスチャ"
  ;; run-testsで既に*db-spec*が設定されているので、ここでは変更しない
  ;; clean-test-dbは独自の接続を確立して閉じる
  (clean-test-db)
  ;; テスト本体を実行（各テストはwith-dbで独自の接続を確立する）
  (&body))

(def-fixture with-transaction ()
  "トランザクションをロールバックするフィクスチャ（データベースを変更しない）"
  ;; run-testsで既に*db-spec*が設定されているので、ここでは変更しない
  (postmodern:with-connection *test-db-spec*
    (postmodern:execute "BEGIN")
    (unwind-protect
         (&body)
      (postmodern:execute "ROLLBACK"))))

;;; テスト用ヘルパー関数

(defun create-test-user (&key
                          (username "testuser")
                          (email "test@example.com")
                          (password "password123")
                          (display-name "Test User")
                          (bio "Test bio"))
  "テスト用ユーザーを作成してIDを返す（フィクスチャ内で使用）"
  ;; *db-spec*はフィクスチャで既に設定されている
  (lisp-blog::create-user username email password display-name bio)
  ;; 作成したユーザーのIDを取得して返す
  (lisp-blog::with-db
    (postmodern:query "SELECT id FROM users WHERE username = $1" username :single)))

(defun create-test-post (user-id &key
                                  (title "Test Post")
                                  (content "Test content")
                                  (status "published"))
  "テスト用投稿を作成（フィクスチャ内で使用）"
  ;; *db-spec*はフィクスチャで既に設定されている
  (lisp-blog::create-post user-id title content status))

(defun get-test-user-id (username)
  "テスト用ユーザーのIDを取得（フィクスチャ内で使用）"
  ;; *db-spec*はフィクスチャで既に設定されている
  (lisp-blog::with-db
    (postmodern:query (:select 'id :from 'users :where (:= 'username username))
                      :single)))
