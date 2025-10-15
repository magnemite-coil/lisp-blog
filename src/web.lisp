(in-package :cl-user)
(defpackage lisp-blog.web
  (:use :cl :caveman2)
  (:import-from :lisp-blog.handler.auth
                :register-handler
                :login-handler
                :logout-handler
                :me-handler)
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

;; ヘルスチェック用エンドポイント
(defroute "/" ()
  "GET / - ヘルスチェック"
  (format nil "lisp-blog API is running"))
