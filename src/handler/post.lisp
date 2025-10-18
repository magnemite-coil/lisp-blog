(in-package :cl-user)
(defpackage lisp-blog.handler.post
  (:use :cl :caveman2)
  (:import-from :lisp-blog.service.post
                :create-post
                :get-post-by-id
                :get-published-posts
                :get-user-posts
                :get-user-drafts
                :update-post
                :delete-post
                :publish-draft
                :unpublish-post
                :check-post-ownership)
  (:import-from :lisp-blog.service.auth
                :get-user-by-session
                :get-user-by-id)
  (:import-from :lisp-blog.model.user
                :user-id
                :user-username)
  (:import-from :lisp-blog.model.post
                :post-id
                :post-user-id
                :post-title
                :post-content
                :post-status
                :post-created-at
                :post-updated-at)
  (:import-from :lisp-blog.util.json
                :json-success
                :json-error)
  (:import-from :jonathan
                :parse)
  (:import-from :lack.request
                :request-body-parameters
                :request-cookies
                :request-parameters)
  (:export :create-post-handler
           :list-posts-handler
           :get-post-handler
           :update-post-handler
           :delete-post-handler
           :publish-post-handler
           :unpublish-post-handler))
(in-package :lisp-blog.handler.post)

;;; ヘルパー関数

(defun get-json-param (params key)
  "JSONパラメータから値を取得

   params: パースされたJSONデータ（alist）
   key: キー（文字列）
   戻り値: 値または NIL"
  (cdr (assoc key params :test #'string=)))

(defun get-query-param (params key)
  "クエリパラメータから値を取得

   params: クエリパラメータ（alist）
   key: キー（文字列）
   戻り値: 値または NIL"
  (cdr (assoc key params :test #'string=)))

(defun get-session-id-from-cookies (cookies)
  "CookieからセッションIDを取得

   cookies: クッキーのalist
   戻り値: セッションID文字列または NIL"
  (cdr (assoc "session_id" cookies :test #'string=)))

(defun get-username-by-user-id (user-id)
  "ユーザーIDからユーザー名を取得

   user-id: ユーザーID
   戻り値: ユーザー名文字列または NIL"
  (let ((user (get-user-by-id user-id)))
    (when user
      (user-username user))))

(defun get-current-user ()
  "現在ログイン中のユーザーを取得

   戻り値: user オブジェクトまたは NIL"
  (let* ((cookies (lack.request:request-cookies *request*))
         (session-id (get-session-id-from-cookies cookies)))
    (when session-id
      (get-user-by-session session-id))))

(defun post-to-json (post &optional username)
  "postオブジェクトをJSON用のplistに変換

   post: postオブジェクト
   username: ユーザー名文字列（省略可）
   戻り値: plist"
  (let ((base-data (list :|id| (post-id post)
                         :|user_id| (post-user-id post)
                         :|title| (post-title post)
                         :|content| (post-content post)
                         :|status| (post-status post)
                         :|created_at| (format nil "~A" (post-created-at post))
                         :|updated_at| (format nil "~A" (post-updated-at post)))))
    (if username
        (append base-data (list :|username| username))
        base-data)))

;;; APIハンドラー

(defun create-post-handler (params)
  "POST /api/posts - 投稿作成

   リクエストボディ:
   {\"title\": \"...\", \"content\": \"...\", \"status\": \"draft\"}

   レスポンス:
   - 成功 (201): {\"success\": true, \"data\": {...}}
   - 失敗 (400): {\"success\": false, \"error\": \"...\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (progn
        ;; 認証チェック
        (let ((user (get-current-user)))
          (unless user
            (return-from create-post-handler
              (multiple-value-bind (json status headers)
                  (json-error "Authentication required" :status 401)
                (list status headers (list json)))))

          ;; リクエストボディ取得
          (let* ((json-data (lack.request:request-body-parameters *request*))
                 (title (get-json-param json-data "title"))
                 (content (get-json-param json-data "content"))
                 (status (or (get-json-param json-data "status") "draft")))

            (unless (and title content)
              (return-from create-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Title and content are required" :status 400)
                  (list status headers (list json)))))

            ;; 投稿作成
            (let ((post (create-post (user-id user) title content status)))
              (multiple-value-bind (json status headers)
                  (json-success (post-to-json post) :status 201)
                (list status headers (list json)))))))
    (error (e)
      (format t "Create post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error (format nil "Failed to create post: ~A" e) :status 400)
        (list status headers (list json))))))

(defun list-posts-handler (params)
  "GET /api/posts - 投稿一覧取得

   クエリパラメータ:
   - status: \"draft\" | \"published\" | 未指定（全て）

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": [...]}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (let* ((query-params (lack.request:request-parameters *request*))
             (status (get-query-param query-params "status")))

        (cond
          ;; 下書き一覧取得（認証必須）
          ((and status (string= status "draft"))
           (let ((user (get-current-user)))
             (unless user
               (return-from list-posts-handler
                 (multiple-value-bind (json status headers)
                     (json-error "Authentication required" :status 401)
                   (list status headers (list json)))))
             (let ((posts (get-user-drafts (user-id user))))
               (multiple-value-bind (json status headers)
                   (json-success (mapcar (lambda (post)
                                           (post-to-json post (get-username-by-user-id (post-user-id post))))
                                         posts))
                 (list status headers (list json))))))

          ;; 公開投稿一覧取得（認証不要）
          ((and status (string= status "published"))
           (let ((posts (get-published-posts)))
             (multiple-value-bind (json status headers)
                 (json-success (mapcar (lambda (post)
                                         (post-to-json post (get-username-by-user-id (post-user-id post))))
                                       posts))
               (list status headers (list json)))))

          ;; 全投稿取得（認証必須、自分の投稿のみ）
          (t
           (let ((user (get-current-user)))
             (unless user
               (return-from list-posts-handler
                 (multiple-value-bind (json status headers)
                     (json-error "Authentication required for listing all posts" :status 401)
                   (list status headers (list json)))))
             (let ((posts (get-user-posts (user-id user))))
               (multiple-value-bind (json status headers)
                   (json-success (mapcar (lambda (post)
                                           (post-to-json post (get-username-by-user-id (post-user-id post))))
                                         posts))
                 (list status headers (list json))))))))
    (error (e)
      (format t "List posts error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error "Failed to list posts" :status 400)
        (list status headers (list json))))))

(defun get-post-handler (params)
  "GET /api/posts/:id - 投稿詳細取得

   パスパラメータ:
   - id: 投稿ID

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {...}}
   - 失敗 (404): {\"success\": false, \"error\": \"Post not found\"}
   - 失敗 (403): {\"success\": false, \"error\": \"Permission denied\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}

   params: リクエストパラメータ"
  (handler-case
      (let* ((post-id (parse-integer (getf params :|id|)))
             (post (get-post-by-id post-id)))

        (unless post
          (return-from get-post-handler
            (multiple-value-bind (json status headers)
                (json-error "Post not found" :status 404)
              (list status headers (list json)))))

        ;; 下書きの場合は所有権チェック
        (when (string= (post-status post) "draft")
          (let ((user (get-current-user)))
            (unless user
              (return-from get-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Authentication required" :status 401)
                  (list status headers (list json)))))
            (unless (= (user-id user) (post-user-id post))
              (return-from get-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Permission denied" :status 403)
                  (list status headers (list json)))))))

        ;; 投稿を返す（ユーザー名を含める）
        (multiple-value-bind (json status headers)
            (json-success (post-to-json post (get-username-by-user-id (post-user-id post))))
          (list status headers (list json))))
    (error (e)
      (format t "Get post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error "Failed to get post" :status 400)
        (list status headers (list json))))))

(defun update-post-handler (params)
  "PUT /api/posts/:id - 投稿更新

   パスパラメータ:
   - id: 投稿ID

   リクエストボディ:
   {\"title\": \"...\", \"content\": \"...\"}

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {...}}
   - 失敗 (404): {\"success\": false, \"error\": \"Post not found\"}
   - 失敗 (403): {\"success\": false, \"error\": \"Permission denied\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}

   params: リクエストパラメータ"
  (handler-case
      (progn
        ;; 認証チェック
        (let ((user (get-current-user)))
          (unless user
            (return-from update-post-handler
              (multiple-value-bind (json status headers)
                  (json-error "Authentication required" :status 401)
                (list status headers (list json)))))

          ;; 投稿取得
          (let* ((post-id (parse-integer (getf params :|id|)))
                 (post (get-post-by-id post-id)))

            (unless post
              (return-from update-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Post not found" :status 404)
                  (list status headers (list json)))))

            ;; 所有権チェック
            (unless (= (user-id user) (post-user-id post))
              (return-from update-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Permission denied" :status 403)
                  (list status headers (list json)))))

            ;; リクエストボディ取得
            (let* ((json-data (lack.request:request-body-parameters *request*))
                   (title (get-json-param json-data "title"))
                   (content (get-json-param json-data "content")))

              (unless (and title content)
                (return-from update-post-handler
                  (multiple-value-bind (json status headers)
                      (json-error "Title and content are required" :status 400)
                    (list status headers (list json)))))

              ;; 投稿更新
              (update-post post title content)
              (multiple-value-bind (json status headers)
                  (json-success (post-to-json post))
                (list status headers (list json)))))))
    (error (e)
      (format t "Update post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error (format nil "Failed to update post: ~A" e) :status 400)
        (list status headers (list json))))))

(defun delete-post-handler (params)
  "DELETE /api/posts/:id - 投稿削除

   パスパラメータ:
   - id: 投稿ID

   レスポンス:
   - 成功 (200): {\"success\": true, \"message\": \"Post deleted successfully\"}
   - 失敗 (404): {\"success\": false, \"error\": \"Post not found\"}
   - 失敗 (403): {\"success\": false, \"error\": \"Permission denied\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}

   params: リクエストパラメータ"
  (handler-case
      (progn
        ;; 認証チェック
        (let ((user (get-current-user)))
          (unless user
            (return-from delete-post-handler
              (multiple-value-bind (json status headers)
                  (json-error "Authentication required" :status 401)
                (list status headers (list json)))))

          ;; 投稿取得
          (let* ((post-id (parse-integer (getf params :|id|)))
                 (post (get-post-by-id post-id)))

            (unless post
              (return-from delete-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Post not found" :status 404)
                  (list status headers (list json)))))

            ;; 所有権チェック
            (unless (= (user-id user) (post-user-id post))
              (return-from delete-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Permission denied" :status 403)
                  (list status headers (list json)))))

            ;; 投稿削除
            (delete-post post)
            (multiple-value-bind (json status headers)
                (json-success nil :message "Post deleted successfully")
              (list status headers (list json))))))
    (error (e)
      (format t "Delete post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error "Failed to delete post" :status 400)
        (list status headers (list json))))))

(defun publish-post-handler (params)
  "PUT /api/posts/:id/publish - 下書き公開

   パスパラメータ:
   - id: 投稿ID

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {...}}
   - 失敗 (404): {\"success\": false, \"error\": \"Post not found\"}
   - 失敗 (403): {\"success\": false, \"error\": \"Permission denied\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}
   - 失敗 (400): {\"success\": false, \"error\": \"Post is already published\"}

   params: リクエストパラメータ"
  (handler-case
      (progn
        ;; 認証チェック
        (let ((user (get-current-user)))
          (unless user
            (return-from publish-post-handler
              (multiple-value-bind (json status headers)
                  (json-error "Authentication required" :status 401)
                (list status headers (list json)))))

          ;; 投稿取得
          (let* ((post-id (parse-integer (getf params :|id|)))
                 (post (get-post-by-id post-id)))

            (unless post
              (return-from publish-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Post not found" :status 404)
                  (list status headers (list json)))))

            ;; 所有権チェック
            (unless (= (user-id user) (post-user-id post))
              (return-from publish-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Permission denied" :status 403)
                  (list status headers (list json)))))

            ;; 下書き公開
            (publish-draft post)
            (multiple-value-bind (json status headers)
                (json-success (post-to-json post))
              (list status headers (list json))))))
    (error (e)
      (format t "Publish post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error (format nil "~A" e) :status 400)
        (list status headers (list json))))))

(defun unpublish-post-handler (params)
  "PUT /api/posts/:id/unpublish - 公開記事を下書きに戻す

   パスパラメータ:
   - id: 投稿ID

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {...}}
   - 失敗 (404): {\"success\": false, \"error\": \"Post not found\"}
   - 失敗 (403): {\"success\": false, \"error\": \"Permission denied\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Authentication required\"}
   - 失敗 (400): {\"success\": false, \"error\": \"Post is already a draft\"}

   params: リクエストパラメータ"
  (handler-case
      (progn
        ;; 認証チェック
        (let ((user (get-current-user)))
          (unless user
            (return-from unpublish-post-handler
              (multiple-value-bind (json status headers)
                  (json-error "Authentication required" :status 401)
                (list status headers (list json)))))

          ;; 投稿取得
          (let* ((post-id (parse-integer (getf params :|id|)))
                 (post (get-post-by-id post-id)))

            (unless post
              (return-from unpublish-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Post not found" :status 404)
                  (list status headers (list json)))))

            ;; 所有権チェック
            (unless (= (user-id user) (post-user-id post))
              (return-from unpublish-post-handler
                (multiple-value-bind (json status headers)
                    (json-error "Permission denied" :status 403)
                  (list status headers (list json)))))

            ;; 公開記事を下書きに戻す
            (unpublish-post post)
            (multiple-value-bind (json status headers)
                (json-success (post-to-json post))
              (list status headers (list json))))))
    (error (e)
      (format t "Unpublish post error: ~A~%" e)
      (multiple-value-bind (json status headers)
          (json-error (format nil "~A" e) :status 400)
        (list status headers (list json))))))
