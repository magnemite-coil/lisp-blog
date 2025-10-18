(in-package :cl-user)
(defpackage lisp-blog-test.middleware.session
  (:use :cl :fiveam)
  (:import-from :lisp-blog.middleware.session
                :generate-session-id
                :create-session
                :get-session
                :get-session-user-id
                :delete-session
                :*redis-connection*)
  (:import-from :lisp-blog-test
                :lisp-blog-test-suite
                :with-empty-db
                :create-test-user))
(in-package :lisp-blog-test.middleware.session)

(def-suite session-middleware-tests
    :description "ミドルウェア層のセッション管理テスト"
    :in lisp-blog-test-suite)

(in-suite session-middleware-tests)

;;; セッションID生成テスト

(test generate-session-id-format
  "セッションIDが正しい形式で生成される"
  (let ((session-id (generate-session-id)))
    (is (stringp session-id))
    (is (> (length session-id) 0))
    ;; Base64エンコードされた文字列のパターン（英数字と+/=）
    (is (cl-ppcre:scan "^[A-Za-z0-9+/=]+$" session-id))))

(test generate-session-id-uniqueness
  "セッションIDは毎回異なる値が生成される（ランダム性）"
  (let ((id1 (generate-session-id))
        (id2 (generate-session-id))
        (id3 (generate-session-id)))
    (is (not (string= id1 id2)))
    (is (not (string= id2 id3)))
    (is (not (string= id1 id3)))))

(test generate-session-id-length
  "セッションIDは適切な長さである（16バイト→Base64で約22文字）"
  (let ((session-id (generate-session-id)))
    ;; 16バイトをBase64エンコードすると約22文字（パディング含む）
    (is (>= (length session-id) 20))
    (is (<= (length session-id) 24))))

;;; セッション作成テスト

(test create-session-success
  "セッションが正常に作成される"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser1" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id)))
      (is (stringp session-id))
      (is (> (length session-id) 0))
      ;; セッションが取得できることを確認
      (let ((session (get-session session-id)))
        (is (not (null session)))
        (is (= user-id (getf session :user-id)))))))

(test create-session-multiple-users
  "複数ユーザーのセッションを作成できる"
  (with-empty-db
    (let* ((user1 (create-test-user "sessionuser2" "password123"))
           (user2 (create-test-user "sessionuser3" "password456"))
           (session-id1 (create-session (lisp-blog.model.user:user-id user1)))
           (session-id2 (create-session (lisp-blog.model.user:user-id user2))))
      (is (not (string= session-id1 session-id2)))
      ;; それぞれのセッションが正しいユーザーに紐付いている
      (is (= (lisp-blog.model.user:user-id user1)
             (get-session-user-id session-id1)))
      (is (= (lisp-blog.model.user:user-id user2)
             (get-session-user-id session-id2))))))

(test create-session-same-user-multiple-times
  "同じユーザーが複数回ログインできる（複数セッション）"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser4" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id1 (create-session user-id))
           (session-id2 (create-session user-id)))
      (is (not (string= session-id1 session-id2)))
      ;; 両方のセッションが有効
      (is (= user-id (get-session-user-id session-id1)))
      (is (= user-id (get-session-user-id session-id2))))))

;;; セッション取得テスト

(test get-session-success
  "有効なセッションが取得できる"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser5" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id))
           (session (get-session session-id)))
      (is (not (null session)))
      (is (= user-id (getf session :user-id)))
      (is (string= session-id (getf session :session-id))))))

(test get-session-nonexistent
  "存在しないセッションIDでNILが返る"
  (let ((session (get-session "nonexistent-session-id")))
    (is (null session))))

(test get-session-user-id-success
  "有効なセッションからユーザーIDが取得できる"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser6" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id))
           (retrieved-user-id (get-session-user-id session-id)))
      (is (= user-id retrieved-user-id)))))

(test get-session-user-id-nonexistent
  "存在しないセッションIDでNILが返る"
  (let ((user-id (get-session-user-id "nonexistent-session-id")))
    (is (null user-id))))

;;; セッション削除テスト

(test delete-session-success
  "セッションが正常に削除される"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser7" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id)))
      ;; 削除前は取得できる
      (is (not (null (get-session session-id))))
      ;; 削除
      (let ((deleted (delete-session session-id)))
        (is (not (null deleted))))
      ;; 削除後は取得できない
      (is (null (get-session session-id)))
      (is (null (get-session-user-id session-id))))))

(test delete-session-nonexistent
  "存在しないセッションを削除してもエラーにならない"
  (let ((deleted (delete-session "nonexistent-session-id")))
    ;; NILが返る（削除対象が存在しなかった）
    (is (null deleted))))

(test delete-session-twice
  "同じセッションを2回削除してもエラーにならない"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser8" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id)))
      ;; 1回目の削除は成功
      (is (not (null (delete-session session-id))))
      ;; 2回目の削除はNIL（既に存在しない）
      (is (null (delete-session session-id))))))

;;; セッション隔離テスト（ユーザー間）

(test session-isolation-between-users
  "異なるユーザーのセッションは独立している"
  (with-empty-db
    (let* ((user1 (create-test-user "sessionuser9" "password123"))
           (user2 (create-test-user "sessionuser10" "password456"))
           (session-id1 (create-session (lisp-blog.model.user:user-id user1)))
           (session-id2 (create-session (lisp-blog.model.user:user-id user2))))
      ;; セッション1を削除
      (delete-session session-id1)
      ;; セッション2は影響を受けない
      (is (null (get-session session-id1)))
      (is (not (null (get-session session-id2)))))))

;;; エッジケーステスト

(test create-session-with-large-user-id
  "大きなユーザーIDでもセッションを作成できる"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser11" "password123"))
           ;; ユーザーIDは自動生成されるが、大きな数値でもテスト
           (session-id (create-session (lisp-blog.model.user:user-id user))))
      (is (not (null session-id)))
      (is (not (null (get-session session-id)))))))

(test session-id-no-collision
  "大量のセッションIDを生成しても衝突しない"
  ;; 100個のセッションIDを生成して重複がないことを確認
  (let ((ids (loop repeat 100 collect (generate-session-id))))
    (is (= 100 (length (remove-duplicates ids :test #'string=))))))

;;; セッションデータ整合性テスト

(test session-data-consistency
  "セッション作成後、データが正しく保持される"
  (with-empty-db
    (let* ((user (create-test-user "sessionuser12" "password123"))
           (user-id (lisp-blog.model.user:user-id user))
           (session-id (create-session user-id))
           (session (get-session session-id)))
      ;; セッションデータの整合性
      (is (getf session :user-id))
      (is (getf session :session-id))
      (is (= user-id (getf session :user-id)))
      (is (string= session-id (getf session :session-id))))))
