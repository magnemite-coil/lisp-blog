(in-package :cl-user)
(defpackage lisp-blog.web
  (:use :cl :caveman2)
  (:import-from :lisp-blog.handler.auth
                :register-handler
                :login-handler
                :logout-handler
                :me-handler)
  (:import-from :lisp-blog.handler.post
                :create-post-handler
                :list-posts-handler
                :get-post-handler
                :update-post-handler
                :delete-post-handler
                :publish-post-handler
                :unpublish-post-handler)
  (:export :*app*))
(in-package :lisp-blog.web)

(defclass <web> (<app>) ())
(defvar *app* (make-instance '<web>))

;;
;; Routing rules
;;

;; 認証API

(defroute ("/api/auth/register" :method :POST) (&rest params)
  "POST /api/auth/register - ユーザー登録"
  (register-handler params))

(defroute ("/api/auth/login" :method :POST) (&rest params)
  "POST /api/auth/login - ログイン"
  (login-handler params))

(defroute ("/api/auth/logout" :method :POST) (&rest params)
  "POST /api/auth/logout - ログアウト"
  (logout-handler params))

(defroute ("/api/auth/me" :method :GET) (&rest params)
  "GET /api/auth/me - 現在のユーザー情報"
  (me-handler params))

;; 投稿API

(defroute ("/api/posts" :method :POST) (&rest params)
  "POST /api/posts - 投稿作成"
  (create-post-handler params))

(defroute ("/api/posts" :method :GET) (&rest params)
  "GET /api/posts - 投稿一覧取得"
  (list-posts-handler params))

(defroute ("/api/posts/:id" :method :GET) (&key id)
  "GET /api/posts/:id - 投稿詳細取得"
  (get-post-handler (list :|id| id)))

(defroute ("/api/posts/:id" :method :PUT) (&key id)
  "PUT /api/posts/:id - 投稿更新"
  (update-post-handler (list :|id| id)))

(defroute ("/api/posts/:id" :method :DELETE) (&key id)
  "DELETE /api/posts/:id - 投稿削除"
  (delete-post-handler (list :|id| id)))

(defroute ("/api/posts/:id/publish" :method :PUT) (&key id)
  "PUT /api/posts/:id/publish - 下書き公開"
  (publish-post-handler (list :|id| id)))

(defroute ("/api/posts/:id/unpublish" :method :PUT) (&key id)
  "PUT /api/posts/:id/unpublish - 公開記事を下書きに戻す"
  (unpublish-post-handler (list :|id| id)))

;; ヘルスチェック用エンドポイント
(defroute "/" ()
  "GET / - ヘルスチェック"
  (format nil "lisp-blog API is running"))
