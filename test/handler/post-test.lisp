(in-package :lisp-blog-test)

;;; 投稿ハンドラーのテストスイート

(def-suite post-handler-tests
  :in lisp-blog-test-suite
  :description "投稿ハンドラーのテスト")

(in-suite post-handler-tests)

;;; ヘルパー関数のテスト

(test get-json-param-post-handler
  "JSONパラメータから値を取得（Post Handler）"
  (let ((params '(("title" . "Test Post")
                  ("content" . "Content here")
                  ("status" . "published"))))
    (is (string= "Test Post" (lisp-blog.handler.post::get-json-param params "title")))
    (is (string= "Content here" (lisp-blog.handler.post::get-json-param params "content")))
    (is (string= "published" (lisp-blog.handler.post::get-json-param params "status")))))

(test get-query-param-basic
  "クエリパラメータから値を取得"
  (let ((params '(("status" . "draft")
                  ("page" . "1")
                  ("limit" . "20"))))
    (is (string= "draft" (lisp-blog.handler.post::get-query-param params "status")))
    (is (string= "1" (lisp-blog.handler.post::get-query-param params "page")))
    (is (string= "20" (lisp-blog.handler.post::get-query-param params "limit")))))

(test get-query-param-nonexistent
  "存在しないクエリパラメータ"
  (let ((params '(("status" . "draft"))))
    (is (null (lisp-blog.handler.post::get-query-param params "page")))))

(test get-username-by-user-id-success
  "ユーザーIDからユーザー名を取得"
  (with-empty-db
    (let* ((user (create-test-user :username "usernametest"))
           (user-id (lisp-blog.model.user:user-id user))
           (username (lisp-blog.handler.post::get-username-by-user-id user-id)))
      (is (string= "usernametest" username)))))

(test get-username-by-user-id-nonexistent
  "存在しないユーザーID"
  (with-empty-db
    (let ((username (lisp-blog.handler.post::get-username-by-user-id 99999)))
      (is (null username)))))

(test post-to-json-basic
  "postオブジェクトをJSON用plistに変換（ユーザー名なし）"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :title "JSON Test"
                                   :content "Content"
                                   :status "draft"))
           (json-data (lisp-blog.handler.post::post-to-json post)))
      (is (= (lisp-blog.model.post:post-id post) (getf json-data :|id|)))
      (is (= (lisp-blog.model.user:user-id user) (getf json-data :|user_id|)))
      (is (string= "JSON Test" (getf json-data :|title|)))
      (is (string= "Content" (getf json-data :|content|)))
      (is (string= "draft" (getf json-data :|status|)))
      (is (not (null (getf json-data :|created_at|))))
      (is (not (null (getf json-data :|updated_at|))))
      ;; ユーザー名は含まれない
      (is (null (getf json-data :|username|))))))

(test post-to-json-with-username
  "postオブジェクトをJSON用plistに変換（ユーザー名あり）"
  (with-empty-db
    (let* ((user (create-test-user :username "author"))
           (post (create-test-post (lisp-blog.model.user:user-id user)))
           (json-data (lisp-blog.handler.post::post-to-json post "author")))
      (is (string= "author" (getf json-data :|username|))))))

(test post-to-json-timestamp-format
  "タイムスタンプのフォーマット確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)))
           (json-data (lisp-blog.handler.post::post-to-json post)))
      (is (stringp (getf json-data :|created_at|)))
      (is (stringp (getf json-data :|updated_at|)))
      (is (> (length (getf json-data :|created_at|)) 0))
      (is (> (length (getf json-data :|updated_at|)) 0)))))

;;; 統合的な動作確認テスト

(test create-post-flow-complete
  "投稿作成の完全なフロー確認"
  (with-empty-db
    (let* ((user (create-test-user :username "author"))
           (user-id (lisp-blog.model.user:user-id user))
           ;; 投稿作成
           (post (lisp-blog.service.post:create-post
                  user-id
                  "Flow Test Post"
                  "Content here"
                  "draft"))
           ;; JSON変換
           (json-data (lisp-blog.handler.post::post-to-json post "author")))
      ;; JSON データ確認
      (is (not (null (getf json-data :|id|))))
      (is (= user-id (getf json-data :|user_id|)))
      (is (string= "Flow Test Post" (getf json-data :|title|)))
      (is (string= "Content here" (getf json-data :|content|)))
      (is (string= "draft" (getf json-data :|status|)))
      (is (string= "author" (getf json-data :|username|))))))

(test list-published-posts-flow
  "公開投稿一覧取得のフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      ;; 下書き2件、公開2件作成
      (lisp-blog.service.post:create-post user-id "Draft 1" "Content" "draft")
      (lisp-blog.service.post:create-post user-id "Published 1" "Content" "published")
      (lisp-blog.service.post:create-post user-id "Draft 2" "Content" "draft")
      (lisp-blog.service.post:create-post user-id "Published 2" "Content" "published")
      ;; 公開投稿のみ取得
      (let ((published-posts (lisp-blog.service.post:get-published-posts)))
        (is (= 2 (length published-posts)))
        ;; JSON変換
        (let ((json-list (mapcar (lambda (post)
                                   (lisp-blog.handler.post::post-to-json post))
                                 published-posts)))
          (is (every (lambda (json-data)
                       (string= "published" (getf json-data :|status|)))
                     json-list)))))))

(test get-user-drafts-flow
  "ユーザーの下書き一覧取得のフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      ;; 下書き2件、公開1件作成
      (lisp-blog.service.post:create-post user-id "Draft 1" "Content" "draft")
      (lisp-blog.service.post:create-post user-id "Published" "Content" "published")
      (lisp-blog.service.post:create-post user-id "Draft 2" "Content" "draft")
      ;; 下書きのみ取得
      (let ((drafts (lisp-blog.service.post:get-user-drafts user-id)))
        (is (= 2 (length drafts)))
        ;; JSON変換
        (let ((json-list (mapcar (lambda (post)
                                   (lisp-blog.handler.post::post-to-json post))
                                 drafts)))
          (is (every (lambda (json-data)
                       (string= "draft" (getf json-data :|status|)))
                     json-list)))))))

(test update-post-flow-complete
  "投稿更新の完全なフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Old Title"
                  "Old Content")))
      ;; 更新前
      (let ((json-before (lisp-blog.handler.post::post-to-json post)))
        (is (string= "Old Title" (getf json-before :|title|)))
        (is (string= "Old Content" (getf json-before :|content|))))
      ;; 更新
      (lisp-blog.service.post:update-post post "New Title" "New Content")
      ;; 更新後
      (let ((json-after (lisp-blog.handler.post::post-to-json post)))
        (is (string= "New Title" (getf json-after :|title|)))
        (is (string= "New Content" (getf json-after :|content|)))))))

(test delete-post-flow-complete
  "投稿削除の完全なフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Delete Me"
                  "Content"))
           (post-id (lisp-blog.model.post:post-id post)))
      ;; 削除前: 取得可能
      (is (not (null (lisp-blog.service.post:get-post-by-id post-id))))
      ;; 削除
      (lisp-blog.service.post:delete-post post)
      ;; 削除後: 取得不可
      (is (null (lisp-blog.service.post:get-post-by-id post-id))))))

(test publish-draft-flow-complete
  "下書き公開の完全なフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Draft Post"
                  "Content"
                  "draft")))
      ;; 公開前
      (let ((json-before (lisp-blog.handler.post::post-to-json post)))
        (is (string= "draft" (getf json-before :|status|))))
      ;; 公開
      (lisp-blog.service.post:publish-draft post)
      ;; 公開後
      (let ((json-after (lisp-blog.handler.post::post-to-json post)))
        (is (string= "published" (getf json-after :|status|)))))))

(test unpublish-post-flow-complete
  "公開記事を下書きに戻すフロー確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Published Post"
                  "Content"
                  "published")))
      ;; 下書きに戻す前
      (let ((json-before (lisp-blog.handler.post::post-to-json post)))
        (is (string= "published" (getf json-before :|status|))))
      ;; 下書きに戻す
      (lisp-blog.service.post:unpublish-post post)
      ;; 下書きに戻した後
      (let ((json-after (lisp-blog.handler.post::post-to-json post)))
        (is (string= "draft" (getf json-after :|status|)))))))

;;; 権限チェックのテスト

(test check-post-ownership-flow
  "投稿所有権チェックのフロー確認"
  (with-empty-db
    (let* ((user1 (create-test-user :username "user1"))
           (user2 (create-test-user :username "user2"))
           (user1-id (lisp-blog.model.user:user-id user1))
           (user2-id (lisp-blog.model.user:user-id user2))
           (post (lisp-blog.service.post:create-post user1-id "Post" "Content")))
      ;; user1は所有者
      (is (lisp-blog.service.post:check-post-ownership post user1-id))
      ;; user2は所有者ではない
      (is (not (lisp-blog.service.post:check-post-ownership post user2-id))))))

;;; エッジケースのテスト

(test post-to-json-long-title
  "長いタイトルのJSON変換"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-title (make-string 255 :initial-element #\あ))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  long-title
                  "Content"))
           (json-data (lisp-blog.handler.post::post-to-json post)))
      (is (string= long-title (getf json-data :|title|))))))

(test post-to-json-long-content
  "長い本文のJSON変換"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-content (make-string 10000 :initial-element #\x))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Title"
                  long-content))
           (json-data (lisp-blog.handler.post::post-to-json post)))
      (is (string= long-content (getf json-data :|content|))))))

(test post-to-json-unicode-content
  "Unicode文字を含む投稿のJSON変換"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "日本語タイトル 🎉"
                  "日本語の本文です"))
           (json-data (lisp-blog.handler.post::post-to-json post)))
      (is (string= "日本語タイトル 🎉" (getf json-data :|title|)))
      (is (string= "日本語の本文です" (getf json-data :|content|))))))

(test get-query-param-numeric-value
  "数値を含むクエリパラメータ"
  (let ((params '(("page" . 1)
                  ("limit" . 20))))
    (is (= 1 (lisp-blog.handler.post::get-query-param params "page")))
    (is (= 20 (lisp-blog.handler.post::get-query-param params "limit")))))

(test get-json-param-empty-string
  "空文字列を含むJSONパラメータ"
  (let ((params '(("title" . "")
                  ("content" . "Some content"))))
    (is (string= "" (lisp-blog.handler.post::get-json-param params "title")))
    (is (string= "Some content" (lisp-blog.handler.post::get-json-param params "content")))))

(test post-to-json-multiple-statuses
  "異なるステータスの投稿のJSON変換"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user))
           (draft-post (lisp-blog.service.post:create-post user-id "Draft" "Content" "draft"))
           (published-post (lisp-blog.service.post:create-post user-id "Published" "Content" "published"))
           (draft-json (lisp-blog.handler.post::post-to-json draft-post))
           (published-json (lisp-blog.handler.post::post-to-json published-post)))
      (is (string= "draft" (getf draft-json :|status|)))
      (is (string= "published" (getf published-json :|status|))))))
