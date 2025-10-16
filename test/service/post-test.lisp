(in-package :lisp-blog-test)

;;; 投稿サービスのテストスイート

(def-suite post-service-tests
  :in lisp-blog-test-suite
  :description "投稿サービスのテスト")

(in-suite post-service-tests)

;;; バリデーションのテスト

(test validate-title-valid
  "有効なタイトル"
  (is (lisp-blog.service.post:validate-title "Test Post"))
  (is (lisp-blog.service.post:validate-title "A"))
  (is (lisp-blog.service.post:validate-title (make-string 255 :initial-element #\a))))

(test validate-title-invalid
  "無効なタイトル"
  ;; 空文字列
  (is (not (lisp-blog.service.post:validate-title "")))
  ;; 256文字
  (is (not (lisp-blog.service.post:validate-title
            (make-string 256 :initial-element #\a)))))

(test validate-content-valid
  "有効な本文"
  (is (lisp-blog.service.post:validate-content "Content"))
  (is (lisp-blog.service.post:validate-content "C"))
  (is (lisp-blog.service.post:validate-content (make-string 100000 :initial-element #\x))))

(test validate-content-invalid
  "無効な本文"
  ;; 空文字列
  (is (not (lisp-blog.service.post:validate-content "")))
  ;; 100,001文字
  (is (not (lisp-blog.service.post:validate-content
            (make-string 100001 :initial-element #\a)))))

(test validate-status-valid
  "有効なステータス"
  (is (lisp-blog.service.post:validate-status "draft"))
  (is (lisp-blog.service.post:validate-status "published")))

(test validate-status-invalid
  "無効なステータス"
  (is (not (lisp-blog.service.post:validate-status "pending")))
  (is (not (lisp-blog.service.post:validate-status "archived")))
  (is (not (lisp-blog.service.post:validate-status "")))
  (is (not (lisp-blog.service.post:validate-status "DRAFT"))))  ; 大文字小文字区別

;;; 投稿作成のテスト

(test create-post-success
  "正常な投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user))
           (post (lisp-blog.service.post:create-post user-id "Test Title" "Test Content")))
      (is (not (null post)))
      (is (= user-id (lisp-blog.model.post:post-user-id post)))
      (is (string= "Test Title" (lisp-blog.model.post:post-title post)))
      (is (string= "Test Content" (lisp-blog.model.post:post-content post)))
      (is (string= "draft" (lisp-blog.model.post:post-status post))))))

(test create-post-with-published-status
  "公開ステータスでの投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Published Post"
                  "Content"
                  "published")))
      (is (string= "published" (lisp-blog.model.post:post-status post))))))

(test create-post-invalid-title
  "無効なタイトルでの投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      ;; 空タイトル
      (signals error
        (lisp-blog.service.post:create-post user-id "" "Content")))))

(test create-post-invalid-content
  "無効な本文での投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      ;; 空本文
      (signals error
        (lisp-blog.service.post:create-post user-id "Title" "")))))

(test create-post-invalid-status
  "無効なステータスでの投稿作成"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      (signals error
        (lisp-blog.service.post:create-post user-id "Title" "Content" "invalid")))))

;;; 投稿取得のテスト

(test get-post-by-id-success
  "IDで投稿を取得"
  (with-empty-db
    (let* ((user (create-test-user))
           (created-post (lisp-blog.service.post:create-post
                          (lisp-blog.model.user:user-id user)
                          "Get Test"
                          "Content"))
           (post-id (lisp-blog.model.post:post-id created-post))
           (retrieved-post (lisp-blog.service.post:get-post-by-id post-id)))
      (is (not (null retrieved-post)))
      (is (= post-id (lisp-blog.model.post:post-id retrieved-post)))
      (is (string= "Get Test" (lisp-blog.model.post:post-title retrieved-post))))))

(test get-post-by-id-nonexistent
  "存在しないIDでの投稿取得"
  (with-empty-db
    (let ((post (lisp-blog.service.post:get-post-by-id 99999)))
      (is (null post)))))

(test get-published-posts-only
  "公開済み投稿のみ取得"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      ;; 下書き2件、公開2件作成
      (lisp-blog.service.post:create-post user-id "Draft 1" "Content" "draft")
      (lisp-blog.service.post:create-post user-id "Published 1" "Content" "published")
      (lisp-blog.service.post:create-post user-id "Draft 2" "Content" "draft")
      (lisp-blog.service.post:create-post user-id "Published 2" "Content" "published")
      ;; 公開済みのみ取得
      (let ((published-posts (lisp-blog.service.post:get-published-posts)))
        (is (= 2 (length published-posts)))
        (is (every (lambda (post)
                     (string= "published" (lisp-blog.model.post:post-status post)))
                   published-posts))))))

(test get-published-posts-order
  "公開済み投稿が新しい順に並んでいる"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user)))
      (lisp-blog.service.post:create-post user-id "Post 1" "Content" "published")
      (sleep 0.01)  ; タイムスタンプの差を確実にするため
      (lisp-blog.service.post:create-post user-id "Post 2" "Content" "published")
      (sleep 0.01)
      (lisp-blog.service.post:create-post user-id "Post 3" "Content" "published")
      (let ((posts (lisp-blog.service.post:get-published-posts)))
        ;; 最新の投稿が最初に来る
        (is (string= "Post 3" (lisp-blog.model.post:post-title (first posts))))
        (is (string= "Post 1" (lisp-blog.model.post:post-title (third posts))))))))

(test get-user-posts-all
  "ユーザーの全投稿を取得（下書き含む）"
  (with-empty-db
    (let* ((user1 (create-test-user :username "user1"))
           (user2 (create-test-user :username "user2"))
           (user1-id (lisp-blog.model.user:user-id user1))
           (user2-id (lisp-blog.model.user:user-id user2)))
      ;; user1の投稿3件
      (lisp-blog.service.post:create-post user1-id "User1 Post 1" "Content" "draft")
      (lisp-blog.service.post:create-post user1-id "User1 Post 2" "Content" "published")
      (lisp-blog.service.post:create-post user1-id "User1 Post 3" "Content" "draft")
      ;; user2の投稿1件
      (lisp-blog.service.post:create-post user2-id "User2 Post 1" "Content" "published")
      ;; user1の投稿を全て取得
      (let ((user1-posts (lisp-blog.service.post:get-user-posts user1-id)))
        (is (= 3 (length user1-posts)))
        (is (every (lambda (post) (= user1-id (lisp-blog.model.post:post-user-id post)))
                   user1-posts))))))

(test get-user-drafts-only
  "ユーザーの下書きのみ取得"
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
        (is (every (lambda (post)
                     (and (= user-id (lisp-blog.model.post:post-user-id post))
                          (string= "draft" (lisp-blog.model.post:post-status post))))
                   drafts))))))

;;; 投稿更新のテスト

(test update-post-success
  "投稿の更新"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Old Title"
                  "Old Content")))
      (lisp-blog.service.post:update-post post "New Title" "New Content")
      (is (string= "New Title" (lisp-blog.model.post:post-title post)))
      (is (string= "New Content" (lisp-blog.model.post:post-content post))))))

(test update-post-invalid-title
  "無効なタイトルでの更新"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Title"
                  "Content")))
      (signals error
        (lisp-blog.service.post:update-post post "" "New Content")))))

(test update-post-invalid-content
  "無効な本文での更新"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Title"
                  "Content")))
      (signals error
        (lisp-blog.service.post:update-post post "New Title" "")))))

;;; 投稿削除のテスト

(test delete-post-success
  "投稿の削除"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Delete Me"
                  "Content"))
           (post-id (lisp-blog.model.post:post-id post)))
      (is (lisp-blog.service.post:delete-post post))
      ;; 削除後は取得できない
      (is (null (lisp-blog.service.post:get-post-by-id post-id))))))

;;; ステータス変更のテスト

(test publish-draft-success
  "下書きの公開"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Draft Post"
                  "Content"
                  "draft")))
      (is (string= "draft" (lisp-blog.model.post:post-status post)))
      (lisp-blog.service.post:publish-draft post)
      (is (string= "published" (lisp-blog.model.post:post-status post))))))

(test publish-draft-already-published
  "既に公開済みの投稿を公開しようとする"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Published Post"
                  "Content"
                  "published")))
      (signals error
        (lisp-blog.service.post:publish-draft post)))))

(test unpublish-post-success
  "公開記事を下書きに戻す"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Published Post"
                  "Content"
                  "published")))
      (is (string= "published" (lisp-blog.model.post:post-status post)))
      (lisp-blog.service.post:unpublish-post post)
      (is (string= "draft" (lisp-blog.model.post:post-status post))))))

(test unpublish-post-already-draft
  "既に下書きの投稿を下書きに戻そうとする"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Draft Post"
                  "Content"
                  "draft")))
      (signals error
        (lisp-blog.service.post:unpublish-post post)))))

;;; 権限チェックのテスト

(test check-post-ownership-true
  "投稿の所有者"
  (with-empty-db
    (let* ((user (create-test-user))
           (user-id (lisp-blog.model.user:user-id user))
           (post (lisp-blog.service.post:create-post user-id "Post" "Content")))
      (is (lisp-blog.service.post:check-post-ownership post user-id)))))

(test check-post-ownership-false
  "投稿の所有者でない"
  (with-empty-db
    (let* ((user1 (create-test-user :username "user1"))
           (user2 (create-test-user :username "user2"))
           (user1-id (lisp-blog.model.user:user-id user1))
           (user2-id (lisp-blog.model.user:user-id user2))
           (post (lisp-blog.service.post:create-post user1-id "Post" "Content")))
      ;; user2は所有者ではない
      (is (not (lisp-blog.service.post:check-post-ownership post user2-id))))))

;;; エッジケースのテスト

(test create-post-max-title-length
  "最大長（255文字）のタイトル"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-title (make-string 255 :initial-element #\a))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  long-title
                  "Content")))
      (is (string= long-title (lisp-blog.model.post:post-title post))))))

(test create-post-max-content-length
  "最大長（100,000文字）の本文"
  (with-empty-db
    (let* ((user (create-test-user))
           (long-content (make-string 100000 :initial-element #\x))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Title"
                  long-content)))
      (is (string= long-content (lisp-blog.model.post:post-content post))))))

(test create-post-unicode-title
  "Unicode文字のタイトル"
  (with-empty-db
    (let* ((user (create-test-user))
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "日本語タイトル 🎉"
                  "Content")))
      (is (string= "日本語タイトル 🎉" (lisp-blog.model.post:post-title post))))))

(test create-post-markdown-content
  "Markdown形式の本文"
  (with-empty-db
    (let* ((user (create-test-user))
           (markdown "# Heading\n\n**Bold** and *italic*")
           (post (lisp-blog.service.post:create-post
                  (lisp-blog.model.user:user-id user)
                  "Markdown Post"
                  markdown)))
      (is (string= markdown (lisp-blog.model.post:post-content post))))))
