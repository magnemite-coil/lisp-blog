(in-package :lisp-blog-test)

;;; Userモデルのテストスイート

(def-suite user-model-tests
  :in lisp-blog-test-suite
  :description "Userモデルのテスト")

(in-suite user-model-tests)

;;; ユーザー作成のテスト

(test create-user-basic
  "基本的なユーザー作成"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "testuser"
                                 :password "hashedpassword123")))
      (is (not (null user)))
      (is (not (null (lisp-blog.model.user:user-id user))))
      (is (string= "testuser" (lisp-blog.model.user:user-username user)))
      (is (string= "hashedpassword123" (lisp-blog.model.user:user-password user))))))

(test create-user-with-timestamps
  "タイムスタンプ付きユーザー作成"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "testuser"
                                 :password "hashedpassword123")))
      (is (not (null (lisp-blog.model.user:user-created-at user))))
      (is (not (null (lisp-blog.model.user:user-updated-at user)))))))

(test create-multiple-users
  "複数ユーザーの作成"
  (with-empty-db
    (let ((user1 (mito:create-dao 'lisp-blog.model.user:user
                                  :username "user1"
                                  :password "password1"))
          (user2 (mito:create-dao 'lisp-blog.model.user:user
                                  :username "user2"
                                  :password "password2")))
      (is (not (= (lisp-blog.model.user:user-id user1)
                  (lisp-blog.model.user:user-id user2))))
      (is (string= "user1" (lisp-blog.model.user:user-username user1)))
      (is (string= "user2" (lisp-blog.model.user:user-username user2))))))

(test create-user-duplicate-username
  "重複ユーザー名でのエラー"
  (with-empty-db
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "duplicate"
                     :password "password1")
    ;; 同じユーザー名で2回目の作成はエラーになるべき
    (signals error
      (mito:create-dao 'lisp-blog.model.user:user
                       :username "duplicate"
                       :password "password2"))))

;;; ユーザー検索のテスト

(test find-user-by-id
  "IDでユーザーを検索"
  (with-empty-db
    (let* ((created-user (mito:create-dao 'lisp-blog.model.user:user
                                          :username "findme"
                                          :password "password"))
           (user-id (lisp-blog.model.user:user-id created-user))
           (found-user (mito:find-dao 'lisp-blog.model.user:user :id user-id)))
      (is (not (null found-user)))
      (is (= user-id (lisp-blog.model.user:user-id found-user)))
      (is (string= "findme" (lisp-blog.model.user:user-username found-user))))))

(test find-user-nonexistent-id
  "存在しないIDでの検索"
  (with-empty-db
    (let ((user (mito:find-dao 'lisp-blog.model.user:user :id 99999)))
      (is (null user)))))

(test select-all-users
  "全ユーザーの取得"
  (with-empty-db
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "user1"
                     :password "password1")
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "user2"
                     :password "password2")
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "user3"
                     :password "password3")
    (let ((users (mito:select-dao 'lisp-blog.model.user:user)))
      (is (= 3 (length users))))))

(test select-user-by-username
  "ユーザー名で検索"
  (with-empty-db
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "alice"
                     :password "password1")
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "bob"
                     :password "password2")
    (let ((users (mito:select-dao 'lisp-blog.model.user:user
                   (sxql:where (:= :username "alice")))))
      (is (= 1 (length users)))
      (is (string= "alice" (lisp-blog.model.user:user-username (first users)))))))

;;; ユーザー更新のテスト

(test update-user-password
  "ユーザーのパスワード更新"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "updatetest"
                                 :password "oldpassword")))
      (setf (lisp-blog.model.user:user-password user) "newpassword")
      (mito:save-dao user)
      (let ((updated-user (mito:find-dao 'lisp-blog.model.user:user
                                         :id (lisp-blog.model.user:user-id user))))
        (is (string= "newpassword" (lisp-blog.model.user:user-password updated-user)))))))

(test update-user-username
  "ユーザー名の更新"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "oldname"
                                 :password "password")))
      (setf (lisp-blog.model.user:user-username user) "newname")
      (mito:save-dao user)
      (let ((updated-user (mito:find-dao 'lisp-blog.model.user:user
                                         :id (lisp-blog.model.user:user-id user))))
        (is (string= "newname" (lisp-blog.model.user:user-username updated-user)))))))

;;; ユーザー削除のテスト

(test delete-user
  "ユーザーの削除"
  (with-empty-db
    (let* ((user (mito:create-dao 'lisp-blog.model.user:user
                                  :username "deleteme"
                                  :password "password"))
           (user-id (lisp-blog.model.user:user-id user)))
      (mito:delete-dao user)
      (let ((found-user (mito:find-dao 'lisp-blog.model.user:user :id user-id)))
        (is (null found-user))))))

(test delete-user-count
  "ユーザー削除後のカウント確認"
  (with-empty-db
    (mito:create-dao 'lisp-blog.model.user:user
                     :username "user1"
                     :password "password1")
    (let ((user2 (mito:create-dao 'lisp-blog.model.user:user
                                  :username "user2"
                                  :password "password2")))
      (is (= 2 (length (mito:select-dao 'lisp-blog.model.user:user))))
      (mito:delete-dao user2)
      (is (= 1 (length (mito:select-dao 'lisp-blog.model.user:user)))))))

;;; エッジケースのテスト

(test create-user-long-username
  "長いユーザー名（50文字）"
  (with-empty-db
    (let* ((long-username (make-string 50 :initial-element #\a))
           (user (mito:create-dao 'lisp-blog.model.user:user
                                  :username long-username
                                  :password "password")))
      (is (string= long-username (lisp-blog.model.user:user-username user))))))

(test create-user-long-password
  "長いパスワード（255文字）"
  (with-empty-db
    (let* ((long-password (make-string 255 :initial-element #\x))
           (user (mito:create-dao 'lisp-blog.model.user:user
                                  :username "longpassuser"
                                  :password long-password)))
      (is (string= long-password (lisp-blog.model.user:user-password user))))))

(test create-user-unicode-username
  "Unicode文字を含むユーザー名"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "ユーザー123"
                                 :password "password")))
      (is (string= "ユーザー123" (lisp-blog.model.user:user-username user))))))

(test create-user-special-characters
  "特殊文字を含むユーザー名"
  (with-empty-db
    (let ((user (mito:create-dao 'lisp-blog.model.user:user
                                 :username "user_123-test"
                                 :password "password")))
      (is (string= "user_123-test" (lisp-blog.model.user:user-username user))))))
