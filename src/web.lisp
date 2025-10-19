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

;;
;; 静的ファイル配信設定
;;

(defparameter *static-directory*
  (asdf:system-relative-pathname :lisp-blog "static/")
  "静的ファイルのルートディレクトリ")

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

;; CORS プリフライトリクエスト対応
(defroute ("/api/*" :method :OPTIONS) (&rest params)
  "OPTIONS /api/* - CORS プリフライトリクエスト"
  (declare (ignore params))
  (list 200
        '(:content-type "text/plain"
          :access-control-allow-origin "http://localhost:5173"
          :access-control-allow-credentials "true"
          :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS"
          :access-control-allow-headers "Content-Type, Authorization"
          :access-control-max-age "86400")
        '("")))

;;
;; 静的ファイル配信ヘルパー関数
;;

(defun get-content-type (filename)
  "ファイル拡張子からContent-Typeを判定する"
  (let ((extension (pathname-type filename)))
    (cond
      ((string-equal extension "html") "text/html; charset=utf-8")
      ((string-equal extension "css")  "text/css; charset=utf-8")
      ((string-equal extension "js")   "application/javascript; charset=utf-8")
      ((string-equal extension "json") "application/json; charset=utf-8")
      ((string-equal extension "png")  "image/png")
      ((string-equal extension "jpg")  "image/jpeg")
      ((string-equal extension "jpeg") "image/jpeg")
      ((string-equal extension "gif")  "image/gif")
      ((string-equal extension "svg")  "image/svg+xml")
      ((string-equal extension "ico")  "image/x-icon")
      (t "application/octet-stream"))))

(defun serve-static-file (file-path)
  "静的ファイルを読み込んでHTTPレスポンスを返す"
  (if (and (probe-file file-path)
           (not (uiop:directory-pathname-p file-path)))
      ;; ファイルが存在する場合
      (with-open-file (stream file-path :direction :input :element-type '(unsigned-byte 8))
        (let* ((length (file-length stream))
               (content (make-array length :element-type '(unsigned-byte 8))))
          (read-sequence content stream)
          (list 200
                (list :content-type (get-content-type file-path))
                (list content))))
      ;; ファイルが存在しない場合は404
      (list 404
            '(:content-type "text/plain")
            '("File not found"))))

;;
;; 静的ファイル配信ルーティング
;;

;; /assets/* ルート（JS/CSSファイル配信）
(defroute ("/assets/*" :method :GET) (&key splat)
  "GET /assets/* - 静的ファイル配信（JS/CSS）"
  (let ((file-path (merge-pathnames
                     (format nil "assets/~{~A~^/~}" splat)
                     *static-directory*)))
    (serve-static-file file-path)))

;; SPAフォールバックルート（すべてのパスでindex.htmlを返す）
;; ただし /api/* は除外（上記のAPIルートが優先される）
(defroute "*" ()
  "GET * - SPAフォールバック（index.htmlを返す）"
  (let ((index-path (merge-pathnames "index.html" *static-directory*)))
    (if (probe-file index-path)
        ;; index.htmlを読み込んで返す
        (with-open-file (stream index-path :direction :input :element-type 'character)
          (let* ((length (file-length stream))
                 (content (make-string length)))
            (read-sequence content stream)
            (list 200
                  '(:content-type "text/html; charset=utf-8")
                  (list content))))
        ;; index.htmlが存在しない場合は404
        (list 404
              '(:content-type "text/plain")
              '("index.html not found. Please run: ./build.sh")))))
