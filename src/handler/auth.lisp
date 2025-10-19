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
                :json-success)
  (:import-from :lisp-blog.util.error
                :validation-error-response
                :auth-error-response
                :resource-error-response
                :condition-to-response
                :log-error)
  (:import-from :lisp-blog.util.conditions
                :lisp-blog-error
                :validation-error
                :authentication-error
                :resource-conflict-error)
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
  "JSONパラメータから値を取得"
  (cdr (assoc key params :test #'string=)))

(defun user-to-json (user)
  "userオブジェクトをJSON用のplistに変換"
  (list :|id| (user-id user)
        :|username| (user-username user)
        :|created_at| (format nil "~A" (user-created-at user))))

(defun set-session-cookie (session-id)
  "セッションIDをCookieに設定"
  (format nil "session_id=~A; Path=/; HttpOnly; SameSite=Lax; Max-Age=~A"
          session-id
          (* 7 24 60 60))) ; 7日間

(defun get-session-id-from-cookies (cookies)
  "CookieからセッションIDを取得"
  (cdr (assoc "session_id" cookies :test #'string=)))

;;; APIハンドラー

(defun register-handler (params)
  "POST /api/auth/register - ユーザー登録

   リクエストボディ:
   {\"username\": \"testuser\", \"password\": \"password123\"}

   レスポンス:
   - 成功 (201): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (400/409): {\"success\": false, \"error\": {...}}"
  (declare (ignore params))
  (handler-case
      (let* ((json-data (lack.request:request-body-parameters *request*))
             (username (get-json-param json-data "username"))
             (password (get-json-param json-data "password")))

        ;; 必須パラメータのチェック
        (unless (and username password)
          (return-from register-handler
            (multiple-value-bind (json status headers)
                (validation-error-response "Username and password are required")
              (list status headers (list json)))))

        ;; ユーザー登録処理
        (let ((result (register-user username password)))
          (if (getf result :success)
              ;; 成功
              (let ((user (getf result :user))
                    (session-id (getf result :session-id)))
                (multiple-value-bind (json status headers)
                    (json-success (user-to-json user) :status 201)
                  (list status
                        (append headers
                                (list :set-cookie (set-session-cookie session-id)))
                        (list json))))
              ;; 失敗（レガシー形式のエラー処理）
              (let ((error-type (getf result :error)))
                (multiple-value-bind (json status headers)
                    (cond
                      ((string= error-type "invalid-username")
                       (validation-error-response
                        "Invalid username (3-50 chars, alphanumeric and underscore only)"
                        :field "username"))
                      ((string= error-type "invalid-password")
                       (validation-error-response
                        "Invalid password (8-255 chars required)"
                        :field "password"))
                      ((string= error-type "username-exists")
                       (resource-error-response :already-exists
                                               :message "Username already exists"
                                               :resource-type "user"))
                      (t
                       (validation-error-response "Registration failed")))
                  (list status headers (list json)))))))

    ;; 構造化されたConditionのハンドリング
    (validation-error (e)
      (let ((error-id (log-error e :handler-name "register-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    (resource-conflict-error (e)
      (let ((error-id (log-error e :handler-name "register-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    (lisp-blog-error (e)
      (let ((error-id (log-error e :handler-name "register-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    ;; 予期しないエラー
    (error (e)
      (format t "Unexpected error in register-handler: ~A~%" e)
      (multiple-value-bind (json status headers)
          (validation-error-response "Invalid request")
        (list status headers (list json))))))

(defun login-handler (params)
  "POST /api/auth/login - ログイン

   リクエストボディ:
   {\"username\": \"testuser\", \"password\": \"password123\"}

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (401): {\"success\": false, \"error\": {...}}"
  (declare (ignore params))
  (handler-case
      (let* ((json-data (lack.request:request-body-parameters *request*))
             (username (get-json-param json-data "username"))
             (password (get-json-param json-data "password")))

        ;; 必須パラメータのチェック
        (unless (and username password)
          (return-from login-handler
            (multiple-value-bind (json status headers)
                (validation-error-response "Username and password are required")
              (list status headers (list json)))))

        ;; 認証処理
        (let ((result (authenticate-user username password)))
          (if (getf result :success)
              ;; 成功
              (let ((user (getf result :user))
                    (session-id (getf result :session-id)))
                (multiple-value-bind (json status headers)
                    (json-success (user-to-json user))
                  (list status
                        (append headers
                                (list :set-cookie (set-session-cookie session-id)))
                        (list json))))
              ;; 失敗
              (multiple-value-bind (json status headers)
                  (auth-error-response :invalid-credentials)
                (list status headers (list json))))))

    ;; 構造化されたConditionのハンドリング
    (authentication-error (e)
      (let ((error-id (log-error e :handler-name "login-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    (lisp-blog-error (e)
      (let ((error-id (log-error e :handler-name "login-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    ;; 予期しないエラー
    (error (e)
      (format t "Unexpected error in login-handler: ~A~%" e)
      (multiple-value-bind (json status headers)
          (validation-error-response "Invalid request")
        (list status headers (list json))))))

(defun logout-handler (params)
  "POST /api/auth/logout - ログアウト

   Cookieからセッションを削除

   レスポンス:
   - 成功 (200): {\"success\": true, \"message\": \"Logged out successfully\"}
   - 失敗 (401): {\"success\": false, \"error\": {...}}"
  (declare (ignore params))
  (handler-case
      (let* ((cookies (lack.request:request-cookies *request*))
             (session-id (get-session-id-from-cookies cookies)))

        (unless session-id
          (return-from logout-handler
            (multiple-value-bind (json status headers)
                (auth-error-response :required "Not authenticated")
              (list status headers (list json)))))

        (delete-session session-id)

        (multiple-value-bind (json status headers)
            (json-success nil :message "Logged out successfully")
          (list status headers (list json))))

    ;; 構造化されたConditionのハンドリング
    (authentication-error (e)
      (let ((error-id (log-error e :handler-name "logout-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    (lisp-blog-error (e)
      (let ((error-id (log-error e :handler-name "logout-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    ;; 予期しないエラー
    (error (e)
      (format t "Unexpected error in logout-handler: ~A~%" e)
      (multiple-value-bind (json status headers)
          (auth-error-response :required "Logout failed")
        (list status headers (list json))))))

(defun me-handler (params)
  "GET /api/auth/me - 現在のユーザー情報を取得

   Cookieからセッションを取得して、ユーザー情報を返す

   レスポンス:
   - 成功 (200): {\"success\": true, \"data\": {\"id\": 1, ...}}
   - 失敗 (401): {\"success\": false, \"error\": {...}}"
  (declare (ignore params))
  (handler-case
      (let* ((cookies (lack.request:request-cookies *request*))
             (session-id (get-session-id-from-cookies cookies)))

        (unless session-id
          (return-from me-handler
            (multiple-value-bind (json status headers)
                (auth-error-response :required "Not authenticated")
              (list status headers (list json)))))

        (let ((user (get-user-by-session session-id)))
          (if user
              (multiple-value-bind (json status headers)
                  (json-success (user-to-json user))
                (list status headers (list json)))
              (multiple-value-bind (json status headers)
                  (auth-error-response :session-expired "Session expired or invalid")
                (list status headers (list json))))))

    ;; 構造化されたConditionのハンドリング
    (authentication-error (e)
      (let ((error-id (log-error e :handler-name "me-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    (lisp-blog-error (e)
      (let ((error-id (log-error e :handler-name "me-handler")))
        (multiple-value-bind (json status headers)
            (condition-to-response e)
          (list status headers (list json)))))

    ;; 予期しないエラー
    (error (e)
      (format t "Unexpected error in me-handler: ~A~%" e)
      (multiple-value-bind (json status headers)
          (auth-error-response :required "Failed to get user info")
        (list status headers (list json))))))
