(in-package :lisp-blog-test)

;;; トランザクション管理

(define-condition rollback-for-test (error)
  ()
  (:documentation "テスト用のロールバックを強制するコンディション"))

(defmacro with-test-db (&body body)
  "トランザクション内でテストを実行し、終了後に必ずロールバック

   使用例:
   (test my-test
     (with-test-db
       (let ((user (create-test-user)))
         (is (= 1 (user-id user))))))"
  `(handler-case
       (mito:with-transaction
         (unwind-protect
              (progn ,@body)
           ;; 常にロールバック（テストデータを残さない）
           (error 'rollback-for-test)))
     (rollback-for-test ())))

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
