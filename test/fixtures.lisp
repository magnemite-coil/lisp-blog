(in-package :lisp-blog-test)

;;; テスト用データベース管理
;;;
;;; 注意: 現在はクリーンアップ方式を採用しています
;;; （トランザクションロールバック方式は将来実装予定）

(defmacro with-test-db (&body body)
  "テスト用データベースでテストを実行

   このマクロは、テスト実行前にデータベースをクリーンアップし、
   テスト実行後も状態をリセットします。

   使用例:
   (test my-test
     (with-test-db
       (let ((user (create-test-user)))
         (is (not (null user))))))"
  `(progn
     ;; テスト前のクリーンアップ
     (cleanup-test-db)
     (setup-test-tables)
     ;; テスト実行
     (unwind-protect
          (progn ,@body)
       ;; テスト後のクリーンアップ（エラー時も必ず実行）
       (cleanup-test-db)
       (setup-test-tables))))

(defmacro with-empty-db (&body body)
  "完全に空のDBでテストを実行

   使用例:
   (test my-test
     (with-empty-db
       (is (= 0 (length (mito:select-dao 'user))))))"
  `(progn
     (cleanup-test-db)
     (setup-test-tables)
     (with-test-db
       ,@body)))

;;; テストデータ作成ヘルパー

(defun create-test-user (&key (username "testuser") (password "password123"))
  "テスト用ユーザーを作成

   username: ユーザー名（デフォルト: testuser）
   password: パスワード（デフォルト: password123）

   戻り値: userオブジェクト"
  (let ((hashed-password (lisp-blog.util.crypto:hash-password password)))
    (mito:create-dao 'lisp-blog.model.user:user
                     :username username
                     :password hashed-password)))

(defun create-test-post (user-id &key
                                  (title "Test Post")
                                  (content "Test Content")
                                  (status "draft"))
  "テスト用投稿を作成

   user-id: ユーザーID
   title: 投稿タイトル（デフォルト: Test Post）
   content: 投稿本文（デフォルト: Test Content）
   status: ステータス（デフォルト: draft）

   戻り値: postオブジェクト"
  (mito:create-dao 'lisp-blog.model.post:post
                   :user-id user-id
                   :title title
                   :content content
                   :status status))

(defun create-test-session (user)
  "テスト用セッションを作成

   user: userオブジェクト
   戻り値: セッションID文字列"
  (lisp-blog.middleware.session:create-session
   (lisp-blog.model.user:user-id user)))

;;; アサーションヘルパー

(defun assert-user-equal (user username)
  "ユーザーのusernameが一致することをアサート"
  (is (string= username (lisp-blog.model.user:user-username user))))

(defun assert-post-equal (post title content status)
  "投稿のタイトル、本文、ステータスが一致することをアサート"
  (is (string= title (lisp-blog.model.post:post-title post)))
  (is (string= content (lisp-blog.model.post:post-content post)))
  (is (string= status (lisp-blog.model.post:post-status post))))
