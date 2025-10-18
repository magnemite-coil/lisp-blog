(in-package :lisp-blog-test)

;;; Postモデルのテストスイート

(def-suite post-model-tests
  :in lisp-blog-test-suite
  :description "Postモデルのテスト")

(in-suite post-model-tests)

;;; 投稿作成のテスト

(test create-post-basic
  "基本的な投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (mito:create-dao 'lisp-blog.model.post:post
                                  :user-id (lisp-blog.model.user:user-id user)
                                  :title "Test Post"
                                  :content "This is a test post."
                                  :status "draft")))
      (is (not (null post)))
      (is (not (null (lisp-blog.model.post:post-id post))))
      (is (= (lisp-blog.model.user:user-id user)
             (lisp-blog.model.post:post-user-id post)))
      (is (string= "Test Post" (lisp-blog.model.post:post-title post)))
      (is (string= "This is a test post." (lisp-blog.model.post:post-content post)))
      (is (string= "draft" (lisp-blog.model.post:post-status post))))))

(test create-post-with-timestamps
  "タイムスタンプ付き投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user))))
      (is (not (null (lisp-blog.model.post:post-created-at post))))
      (is (not (null (lisp-blog.model.post:post-updated-at post)))))))

(test create-post-default-status
  "デフォルトステータス（draft）の確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (mito:create-dao 'lisp-blog.model.post:post
                                  :user-id (lisp-blog.model.user:user-id user)
                                  :title "Draft Post"
                                  :content "Content")))
      ;; statusを指定しない場合、デフォルトは "draft"
      (is (string= "draft" (lisp-blog.model.post:post-status post))))))

(test create-post-published
  "公開ステータスの投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :status "published")))
      (is (string= "published" (lisp-blog.model.post:post-status post))))))

(test create-multiple-posts
  "複数投稿の作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user))
           (post1 (create-test-post user-id :title "Post 1"))
           (post2 (create-test-post user-id :title "Post 2"))
           (post3 (create-test-post user-id :title "Post 3")))
      (is (not (= (lisp-blog.model.post:post-id post1)
                  (lisp-blog.model.post:post-id post2))))
      (is (not (= (lisp-blog.model.post:post-id post2)
                  (lisp-blog.model.post:post-id post3)))))))

(test create-posts-by-different-users
  "異なるユーザーの投稿作成"
  (with-empty-db
    (let* ((user1 (create-test-user :username "user1"))
           (user2 (create-test-user :username "user2"))
           (post1 (create-test-post (lisp-blog.model.user:user-id user1)))
           (post2 (create-test-post (lisp-blog.model.user:user-id user2))))
      (is (= (lisp-blog.model.user:user-id user1)
             (lisp-blog.model.post:post-user-id post1)))
      (is (= (lisp-blog.model.user:user-id user2)
             (lisp-blog.model.post:post-user-id post2)))
      (is (not (= (lisp-blog.model.post:post-user-id post1)
                  (lisp-blog.model.post:post-user-id post2)))))))

;;; 投稿検索のテスト

(test find-post-by-id
  "IDで投稿を検索"
  (with-empty-db
    (let* ((user (create-test-user))
           (created-post (create-test-post (lisp-blog.model.user:user-id user)
                                           :title "Find Me"))
           (post-id (lisp-blog.model.post:post-id created-post))
           (found-post (mito:find-dao 'lisp-blog.model.post:post :id post-id)))
      (is (not (null found-post)))
      (is (= post-id (lisp-blog.model.post:post-id found-post)))
      (is (string= "Find Me" (lisp-blog.model.post:post-title found-post))))))

(test find-post-nonexistent-id
  "存在しないIDでの検索"
  (with-empty-db
    (let ((post (mito:find-dao 'lisp-blog.model.post:post :id 99999)))
      (is (null post)))))

(test select-all-posts
  "全投稿の取得"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      (create-test-post user-id :title "Post 1")
      (create-test-post user-id :title "Post 2")
      (create-test-post user-id :title "Post 3")
      (let ((posts (mito:select-dao 'lisp-blog.model.post:post)))
        (is (= 3 (length posts)))))))

(test select-posts-by-user
  "ユーザーIDで投稿を検索"
  (with-empty-db
    (let* ((user1 (create-test-user :username "user1"))
           (user2 (create-test-user :username "user2"))
           (user1-id (lisp-blog.model.user:user-id user1))
           (user2-id (lisp-blog.model.user:user-id user2)))
      (create-test-post user1-id :title "User1 Post 1")
      (create-test-post user1-id :title "User1 Post 2")
      (create-test-post user2-id :title "User2 Post 1")
      (let ((user1-posts (mito:select-dao 'lisp-blog.model.post:post
                           (sxql:where (:= :user-id user1-id)))))
        (is (= 2 (length user1-posts)))
        (is (every (lambda (post)
                     (= user1-id (lisp-blog.model.post:post-user-id post)))
                   user1-posts))))))

(test select-posts-by-status
  "ステータスで投稿を検索"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      (create-test-post user-id :status "draft")
      (create-test-post user-id :status "draft")
      (create-test-post user-id :status "published")
      (let ((draft-posts (mito:select-dao 'lisp-blog.model.post:post
                           (sxql:where (:= :status "draft")))))
        (is (= 2 (length draft-posts)))
        (is (every (lambda (post)
                     (string= "draft" (lisp-blog.model.post:post-status post)))
                   draft-posts))))))

;;; 投稿更新のテスト

(test update-post-title
  "投稿タイトルの更新"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :title "Old Title")))
      (setf (lisp-blog.model.post:post-title post) "New Title")
      (mito:save-dao post)
      (let ((updated-post (mito:find-dao 'lisp-blog.model.post:post
                                         :id (lisp-blog.model.post:post-id post))))
        (is (string= "New Title" (lisp-blog.model.post:post-title updated-post)))))))

(test update-post-content
  "投稿本文の更新"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :content "Old content")))
      (setf (lisp-blog.model.post:post-content post) "New content")
      (mito:save-dao post)
      (let ((updated-post (mito:find-dao 'lisp-blog.model.post:post
                                         :id (lisp-blog.model.post:post-id post))))
        (is (string= "New content" (lisp-blog.model.post:post-content updated-post)))))))

(test update-post-status
  "投稿ステータスの更新（下書き→公開）"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :status "draft")))
      (is (string= "draft" (lisp-blog.model.post:post-status post)))
      (setf (lisp-blog.model.post:post-status post) "published")
      (mito:save-dao post)
      (let ((updated-post (mito:find-dao 'lisp-blog.model.post:post
                                         :id (lisp-blog.model.post:post-id post))))
        (is (string= "published" (lisp-blog.model.post:post-status updated-post)))))))

;;; 投稿削除のテスト

(test delete-post
  "投稿の削除"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)))
           (post-id (lisp-blog.model.post:post-id post)))
      (mito:delete-dao post)
      (let ((found-post (mito:find-dao 'lisp-blog.model.post:post :id post-id)))
        (is (null found-post))))))

(test delete-post-count
  "投稿削除後のカウント確認"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      (create-test-post user-id :title "Post 1")
      (let ((post2 (create-test-post user-id :title "Post 2")))
        (is (= 2 (length (mito:select-dao 'lisp-blog.model.post:post))))
        (mito:delete-dao post2)
        (is (= 1 (length (mito:select-dao 'lisp-blog.model.post:post))))))))

;;; エッジケースのテスト

(test create-post-long-title
  "長いタイトル（255文字）"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-title (make-string 255 :initial-element #\あ))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :title long-title)))
      (is (string= long-title (lisp-blog.model.post:post-title post))))))

(test create-post-long-content
  "長い本文（10000文字）"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-content (make-string 10000 :initial-element #\x))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :content long-content)))
      (is (string= long-content (lisp-blog.model.post:post-content post))))))

(test create-post-unicode-title
  "Unicode文字を含むタイトル"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :title "日本語タイトル 🎉")))
      (is (string= "日本語タイトル 🎉" (lisp-blog.model.post:post-title post))))))

(test create-post-markdown-content
  "Markdown形式の本文"
  (with-empty-db
    (let* ((user (create-test-user))
           (markdown-content "# Heading\n\n**Bold** and *italic*\n\n- List item 1\n- List item 2")
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :content markdown-content)))
      (is (string= markdown-content (lisp-blog.model.post:post-content post))))))

(test create-post-empty-content
  "空本文の投稿"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                   :content "")))
      (is (string= "" (lisp-blog.model.post:post-content post))))))
