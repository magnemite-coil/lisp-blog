(in-package :lisp-blog-test)

;;; Post Service層のエラーハンドリングテストスイート

(def-suite post-service-error-tests
  :in lisp-blog-test-suite
  :description "Post Service層のConditionベースエラーハンドリングのテスト")

(in-suite post-service-error-tests)

;;; create-post のエラーハンドリングテスト

(test create-post-invalid-title-empty
  "空のタイトルでバリデーションエラーが発生すること"
  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 "" "Content"))

  (handler-case
      (lisp-blog.service.post:create-post 1 "" "Content")
    (lisp-blog.util.conditions:validation-error (e)
      (is (string= "title" (lisp-blog.util.conditions:error-field e)))
      (is (search "Invalid title" (lisp-blog.util.conditions:error-message e))))))

(test create-post-invalid-title-too-long
  "長すぎるタイトルでバリデーションエラーが発生すること"
  (let ((long-title (make-string 256 :initial-element #\a)))
    (signals lisp-blog.util.conditions:validation-error
      (lisp-blog.service.post:create-post 1 long-title "Content"))

    (handler-case
        (lisp-blog.service.post:create-post 1 long-title "Content")
      (lisp-blog.util.conditions:validation-error (e)
        (is (string= "title" (lisp-blog.util.conditions:error-field e)))))))

(test create-post-invalid-title-non-string
  "文字列でないタイトルでバリデーションエラーが発生すること"
  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 123 "Content"))

  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 nil "Content")))

(test create-post-invalid-content-empty
  "空の本文でバリデーションエラーが発生すること"
  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 "Title" ""))

  (handler-case
      (lisp-blog.service.post:create-post 1 "Title" "")
    (lisp-blog.util.conditions:validation-error (e)
      (is (string= "content" (lisp-blog.util.conditions:error-field e)))
      (is (search "Invalid content" (lisp-blog.util.conditions:error-message e))))))

(test create-post-invalid-content-too-long
  "長すぎる本文でバリデーションエラーが発生すること"
  (let ((long-content (make-string 100001 :initial-element #\a)))
    (signals lisp-blog.util.conditions:validation-error
      (lisp-blog.service.post:create-post 1 "Title" long-content))

    (handler-case
        (lisp-blog.service.post:create-post 1 "Title" long-content)
      (lisp-blog.util.conditions:validation-error (e)
        (is (string= "content" (lisp-blog.util.conditions:error-field e)))))))

(test create-post-invalid-content-non-string
  "文字列でない本文でバリデーションエラーが発生すること"
  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 "Title" 123))

  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 "Title" nil)))

(test create-post-invalid-status
  "不正なステータスでバリデーションエラーが発生すること"
  (signals lisp-blog.util.conditions:validation-error
    (lisp-blog.service.post:create-post 1 "Title" "Content" "invalid-status"))

  (handler-case
      (lisp-blog.service.post:create-post 1 "Title" "Content" "invalid-status")
    (lisp-blog.util.conditions:validation-error (e)
      (is (string= "status" (lisp-blog.util.conditions:error-field e)))
      (is (search "Invalid status" (lisp-blog.util.conditions:error-message e))))))

(test create-post-valid-statuses
  "有効なステータス（draft, published）で成功すること"
  (with-test-db
    (let ((user (create-test-user "postuser1" "password123")))
      ;; draft
      (let ((post1 (lisp-blog.service.post:create-post
                    (lisp-blog.model.user:user-id user)
                    "Test Post 1"
                    "Test content"
                    "draft")))
        (is (not (null post1)))
        (is (string= "draft" (lisp-blog.model.post:post-status post1))))

      ;; published
      (let ((post2 (lisp-blog.service.post:create-post
                    (lisp-blog.model.user:user-id user)
                    "Test Post 2"
                    "Test content"
                    "published")))
        (is (not (null post2)))
        (is (string= "published" (lisp-blog.model.post:post-status post2)))))))

(test create-post-multiple-validation-errors
  "複数のバリデーションエラー（最初のエラーで停止）"
  ;; タイトルとコンテンツの両方が不正な場合、最初にチェックされるタイトルでエラー
  (handler-case
      (lisp-blog.service.post:create-post 1 "" "")
    (lisp-blog.util.conditions:validation-error (e)
      (is (string= "title" (lisp-blog.util.conditions:error-field e))))))

;;; update-post のエラーハンドリングテスト

(test update-post-invalid-title
  "更新時の不正なタイトルでバリデーションエラーが発生すること"
  (with-test-db
    (let* ((user (create-test-user "updateuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Original Title"
                                  "Original Content")))

      ;; 空のタイトル
      (signals lisp-blog.util.conditions:validation-error
        (lisp-blog.service.post:update-post post "" "New content"))

      ;; 長すぎるタイトル
      (let ((long-title (make-string 256 :initial-element #\a)))
        (signals lisp-blog.util.conditions:validation-error
          (lisp-blog.service.post:update-post post long-title "New content"))))))

(test update-post-invalid-content
  "更新時の不正な本文でバリデーションエラーが発生すること"
  (with-test-db
    (let* ((user (create-test-user "updateuser2" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Original Title"
                                  "Original Content")))

      ;; 空の本文
      (signals lisp-blog.util.conditions:validation-error
        (lisp-blog.service.post:update-post post "New title" ""))

      ;; 長すぎる本文
      (let ((long-content (make-string 100001 :initial-element #\a)))
        (signals lisp-blog.util.conditions:validation-error
          (lisp-blog.service.post:update-post post "New title" long-content))))))

(test update-post-error-does-not-modify-data
  "バリデーションエラー時にデータが変更されないこと"
  (with-test-db
    (let* ((user (create-test-user "updateuser3" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Original Title"
                                  "Original Content"))
           (original-title (lisp-blog.model.post:post-title post))
           (original-content (lisp-blog.model.post:post-content post)))

      ;; バリデーションエラーを発生させる
      (handler-case
          (lisp-blog.service.post:update-post post "" "New content")
        (lisp-blog.util.conditions:validation-error (e)
          ;; エラーが発生してもデータは変更されていない
          (is (string= original-title (lisp-blog.model.post:post-title post)))
          (is (string= original-content (lisp-blog.model.post:post-content post))))))))

;;; publish-draft のエラーハンドリングテスト

(test publish-draft-already-published
  "既に公開済みの投稿を公開しようとするとビジネスロジックエラーが発生すること"
  (with-test-db
    (let* ((user (create-test-user "publishuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Test Content"
                                  "published")))

      ;; 公開済み投稿を再度公開しようとする
      (signals lisp-blog.util.conditions:business-logic-error
        (lisp-blog.service.post:publish-draft post))

      ;; エラーの詳細を確認
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        (lisp-blog.util.conditions:business-logic-error (e)
          (is (eq :post-already-published (lisp-blog.util.conditions:error-code e)))
          (is (search "already published" (lisp-blog.util.conditions:error-message e))))))))

(test publish-draft-success
  "下書きの公開が成功すること"
  (with-test-db
    (let* ((user (create-test-user "publishuser2" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Draft Post"
                                  "Draft Content"
                                  "draft")))

      ;; 公開前はdraft
      (is (string= "draft" (lisp-blog.model.post:post-status post)))

      ;; 公開処理
      (let ((published-post (lisp-blog.service.post:publish-draft post)))
        (is (not (null published-post)))
        (is (string= "published" (lisp-blog.model.post:post-status published-post)))))))

(test publish-draft-does-not-modify-on-error
  "エラー時にステータスが変更されないこと"
  (with-test-db
    (let* ((user (create-test-user "publishuser3" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Published Post"
                                  "Content"
                                  "published")))

      ;; エラーを発生させる
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        (lisp-blog.util.conditions:business-logic-error (e)
          ;; ステータスは変更されていない
          (is (string= "published" (lisp-blog.model.post:post-status post))))))))

;;; unpublish-post のエラーハンドリングテスト

(test unpublish-post-already-draft
  "既に下書き状態の投稿を下書きに戻そうとするとビジネスロジックエラーが発生すること"
  (with-test-db
    (let* ((user (create-test-user "unpublishuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Draft Post"
                                  "Draft Content"
                                  "draft")))

      ;; 下書き投稿を再度下書きに戻そうとする
      (signals lisp-blog.util.conditions:business-logic-error
        (lisp-blog.service.post:unpublish-post post))

      ;; エラーの詳細を確認
      (handler-case
          (lisp-blog.service.post:unpublish-post post)
        (lisp-blog.util.conditions:business-logic-error (e)
          (is (eq :post-already-draft (lisp-blog.util.conditions:error-code e)))
          (is (search "already a draft" (lisp-blog.util.conditions:error-message e))))))))

(test unpublish-post-success
  "公開記事の下書き化が成功すること"
  (with-test-db
    (let* ((user (create-test-user "unpublishuser2" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Published Post"
                                  "Published Content"
                                  "published")))

      ;; 下書き化前はpublished
      (is (string= "published" (lisp-blog.model.post:post-status post)))

      ;; 下書き化処理
      (let ((draft-post (lisp-blog.service.post:unpublish-post post)))
        (is (not (null draft-post)))
        (is (string= "draft" (lisp-blog.model.post:post-status draft-post)))))))

(test unpublish-post-does-not-modify-on-error
  "エラー時にステータスが変更されないこと"
  (with-test-db
    (let* ((user (create-test-user "unpublishuser3" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Draft Post"
                                  "Content"
                                  "draft")))

      ;; エラーを発生させる
      (handler-case
          (lisp-blog.service.post:unpublish-post post)
        (lisp-blog.util.conditions:business-logic-error (e)
          ;; ステータスは変更されていない
          (is (string= "draft" (lisp-blog.model.post:post-status post))))))))

;;; バリデーション関数のテスト

(test validate-title-edge-cases
  "タイトルバリデーションの境界値テスト"
  ;; 有効なタイトル
  (is (lisp-blog.service.post:validate-title "a"))  ; 最小長（1文字）
  (is (lisp-blog.service.post:validate-title (make-string 255 :initial-element #\a)))  ; 最大長（255文字）
  (is (lisp-blog.service.post:validate-title "Normal Title"))

  ;; 無効なタイトル
  (is (not (lisp-blog.service.post:validate-title "")))  ; 空文字
  (is (not (lisp-blog.service.post:validate-title (make-string 256 :initial-element #\a))))  ; 長すぎる
  (is (not (lisp-blog.service.post:validate-title nil)))  ; nil
  (is (not (lisp-blog.service.post:validate-title 123)))  ; 数値
  (is (not (lisp-blog.service.post:validate-title '("list")))))  ; リスト

(test validate-content-edge-cases
  "本文バリデーションの境界値テスト"
  ;; 有効な本文
  (is (lisp-blog.service.post:validate-content "a"))  ; 最小長（1文字）
  (is (lisp-blog.service.post:validate-content (make-string 100000 :initial-element #\a)))  ; 最大長（100,000文字）
  (is (lisp-blog.service.post:validate-content "Normal content"))

  ;; 無効な本文
  (is (not (lisp-blog.service.post:validate-content "")))  ; 空文字
  (is (not (lisp-blog.service.post:validate-content (make-string 100001 :initial-element #\a))))  ; 長すぎる
  (is (not (lisp-blog.service.post:validate-content nil)))  ; nil
  (is (not (lisp-blog.service.post:validate-content 123)))  ; 数値
  (is (not (lisp-blog.service.post:validate-content '("list")))))  ; リスト

(test validate-status-edge-cases
  "ステータスバリデーションのテスト"
  ;; 有効なステータス
  (is (lisp-blog.service.post:validate-status "draft"))
  (is (lisp-blog.service.post:validate-status "published"))

  ;; 無効なステータス
  (is (not (lisp-blog.service.post:validate-status "Draft")))  ; 大文字
  (is (not (lisp-blog.service.post:validate-status "PUBLISHED")))  ; 大文字
  (is (not (lisp-blog.service.post:validate-status "invalid")))
  (is (not (lisp-blog.service.post:validate-status "")))
  (is (not (lisp-blog.service.post:validate-status nil)))
  (is (not (lisp-blog.service.post:validate-status 123))))

;;; エラーハンドリングの階層的なテスト

(test catch-validation-error-as-parent-condition
  "validation-errorをlisp-blog-errorとしてキャッチできること"
  (let ((caught nil))
    (handler-case
        (lisp-blog.service.post:create-post 1 "" "Content")
      ;; 親条件でキャッチ
      (lisp-blog.util.conditions:lisp-blog-error (e)
        (setf caught t)
        (is (typep e 'lisp-blog.util.conditions:validation-error))))
    (is (eq t caught))))

(test catch-business-logic-error-as-parent-condition
  "business-logic-errorをlisp-blog-errorとしてキャッチできること"
  (with-test-db
    (let* ((user (create-test-user "catchuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Published Post"
                                  "Content"
                                  "published"))
           (caught nil))
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        ;; 親条件でキャッチ
        (lisp-blog.util.conditions:lisp-blog-error (e)
          (setf caught t)
          (is (typep e 'lisp-blog.util.conditions:business-logic-error))))
      (is (eq t caught)))))

(test multiple-error-types-handling
  "複数のエラータイプを個別にハンドリングできること"
  (with-test-db
    (let* ((user (create-test-user "multiuser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Content"
                                  "published"))
           (validation-caught nil)
           (business-caught nil))

      ;; バリデーションエラーのキャッチ
      (handler-case
          (lisp-blog.service.post:create-post 1 "" "Content")
        (lisp-blog.util.conditions:validation-error (e)
          (setf validation-caught t))
        (lisp-blog.util.conditions:business-logic-error (e)
          (setf business-caught t)))
      (is (eq t validation-caught))
      (is (eq nil business-caught))

      ;; ビジネスロジックエラーのキャッチ
      (setf validation-caught nil
            business-caught nil)
      (handler-case
          (lisp-blog.service.post:publish-draft post)
        (lisp-blog.util.conditions:validation-error (e)
          (setf validation-caught t))
        (lisp-blog.util.conditions:business-logic-error (e)
          (setf business-caught t)))
      (is (eq nil validation-caught))
      (is (eq t business-caught)))))

;;; check-post-ownership のテスト

(test check-post-ownership-valid-owner
  "投稿の所有者が正しく判定されること"
  (with-test-db
    (let* ((user (create-test-user "owneruser1" "password123"))
           (post (create-test-post (lisp-blog.model.user:user-id user)
                                  "Test Post"
                                  "Content")))
      (is (lisp-blog.service.post:check-post-ownership
           post
           (lisp-blog.model.user:user-id user))))))

(test check-post-ownership-invalid-owner
  "投稿の所有者でない場合にNILが返されること"
  (with-test-db
    (let* ((user1 (create-test-user "owneruser2" "password123"))
           (user2 (create-test-user "otheruser1" "password456"))
           (post (create-test-post (lisp-blog.model.user:user-id user1)
                                  "Test Post"
                                  "Content")))
      (is (not (lisp-blog.service.post:check-post-ownership
                post
                (lisp-blog.model.user:user-id user2)))))))
