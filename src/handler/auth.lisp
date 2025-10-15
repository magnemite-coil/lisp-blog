(in-package :cl-user)
(defpackage lisp-blog.handler.auth
  (:use :cl :caveman2)
  (:import-from :lisp-blog.service.auth
                :register-user
                :authenticate-user
                :get-user-by-session)
  (:import-from :lisp-blog.middleware.session
                :delete-session)
  (:import-from :lisp-blog.util.json
                :json-success
                :json-error)
  (:import-from :lisp-blog.model.user
                :user-id
                :user-username
                :user-created-at)
  (:import-from :jonathan
                :parse)
  (:import-from :lack.request
                :request-body-parameters
                :request-cookies)
  (:export :register-handler
           :login-handler
           :logout-handler
           :me-handler))
(in-package :lisp-blog.handler.auth)

;;; ヘルパー関数

(defun get-json-param (params key)
  "JSONパラメータから値を取得

   params: パースされたJSONデータ（alist）
   key: キー（文字列）
   戻り値: 値または NIL"
  (cdr (assoc key params :test #'string=)))

(defun user-to-json (user)
  "userオブジェクトをJSON用のplistに変換

   user: userオブジェクト
   戻り値: plist"
  (list :|id| (user-id user)
        :|username| (user-username user)
        :|created_at| (format nil "~A" (user-created-at user))))

(defun set-session-cookie (session-id)
  "セッションIDをCookieに設定

   session-id: セッションID文字列
   戻り値: Set-Cookie ヘッダー文字列"
  (format nil "session_id=~A; Path=/; HttpOnly; SameSite=Lax; Max-Age=~A"
          session-id
          (* 7 24 60 60))) ; 7日間

(defun get-session-id-from-cookies (cookies)
  "CookieからセッションIDを取得

   cookies: クッキーのalist
   戻り値: セッションID文字列または NIL"
  (cdr (assoc "session_id" cookies :test #'string=)))

;;; APIハンドラー

(defun register-handler (params)
  "POST /api/auth/register - ユーザー登録

   リクエストボディ:
   {\"username\": \"testuser\", \"password\": \"password123\"}

   レスポンス:
   - 成功 (201): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (400/409): {\"success\": false, \"error\": \"...\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (let* ((json-data (lack.request:request-body-parameters *request*))
             (username (get-json-param json-data "username"))
             (password (get-json-param json-data "password")))

        (unless (and username password)
          (return-from register-handler
            (json-error "Username and password are required" :status 400)))

        (let ((result (register-user username password)))
          (if (getf result :success)
              ;; 成功
              (let ((user (getf result :user))
                    (session-id (getf result :session-id)))
                (multiple-value-bind (json status headers)
                    (json-success (user-to-json user) :status 201)
                  ;; Lack形式のレスポンス: (list status headers body)
                  ;; bodyは文字列のリストまたはバイトベクタである必要がある
                  (list status
                        (append headers
                                (list :set-cookie (set-session-cookie session-id)))
                        (list json))))
              ;; 失敗
              (let ((error (getf result :error)))
                (cond
                  ((string= error "invalid-username")
                   (json-error "Invalid username (3-50 chars, alphanumeric and underscore only)"
                              :status 400))
                  ((string= error "invalid-password")
                   (json-error "Invalid password (8-255 chars required)"
                              :status 400))
                  ((string= error "username-exists")
                   (json-error "Username already exists"
                              :status 409))
                  (t
                   (json-error "Registration failed"
                              :status 400)))))))
    (error (e)
      (format t "Register error: ~A~%" e)
      (json-error "Invalid request" :status 400))))

(defun login-handler (params)
  "POST /api/auth/login - ログイン

   リクエストボディ:
   {\"username\": \"testuser\", \"password\": \"password123\"}

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (401): {\"success\": false, \"error\": \"Invalid credentials\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (let* ((json-data (lack.request:request-body-parameters *request*))
             (username (get-json-param json-data "username"))
             (password (get-json-param json-data "password")))

        (unless (and username password)
          (return-from login-handler
            (json-error "Username and password are required" :status 400)))

        (let ((result (authenticate-user username password)))
          (if (getf result :success)
              ;; 成功
              (let ((user (getf result :user))
                    (session-id (getf result :session-id)))
                (multiple-value-bind (json status headers)
                    (json-success (user-to-json user))
                  ;; Lack形式のレスポンス: (list status headers body)
                  ;; bodyは文字列のリストまたはバイトベクタである必要がある
                  (list status
                        (append headers
                                (list :set-cookie (set-session-cookie session-id)))
                        (list json))))
              ;; 失敗
              (json-error "Invalid credentials" :status 401))))
    (error (e)
      (format t "Login error: ~A~%" e)
      (json-error "Invalid request" :status 400))))

(defun logout-handler (params)
  "POST /api/auth/logout - ログアウト

   Cookieからセッションを削除

   レスポンス:
   - 成功 (200): {\"success\": true, \"message\": \"Logged out successfully\"}
   - 失敗 (401): {\"success\": false, \"error\": \"Not authenticated\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (let* ((cookies (lack.request:request-cookies *request*))
             (session-id (get-session-id-from-cookies cookies)))

        (unless session-id
          (return-from logout-handler
            (json-error "Not authenticated" :status 401)))

        (delete-session session-id)

        (json-success nil
                     :message "Logged out successfully"))
    (error (e)
      (format t "Logout error: ~A~%" e)
      (json-error "Logout failed" :status 400))))

(defun me-handler (params)
  "GET /api/auth/me - 現在のユーザー情報を取得

   Cookieからセッションを取得して、ユーザー情報を返す

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (401): {\"success\": false, \"error\": \"Not authenticated\"}

   params: リクエストパラメータ"
  (declare (ignore params))
  (handler-case
      (let* ((cookies (lack.request:request-cookies *request*))
             (session-id (get-session-id-from-cookies cookies)))

        (unless session-id
          (return-from me-handler
            (json-error "Not authenticated" :status 401)))

        (let ((user (get-user-by-session session-id)))
          (if user
              (json-success (user-to-json user))
              (json-error "Not authenticated" :status 401))))
    (error (e)
      (format t "Me error: ~A~%" e)
      (json-error "Failed to get user info" :status 400))))
