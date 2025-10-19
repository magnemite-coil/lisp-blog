(in-package :cl-user)
(defpackage lisp-blog.service.auth
  (:use :cl)
  (:import-from :lisp-blog.model.user
                :user
                :user-id
                :user-username
                :user-password
                :user-created-at)
  (:import-from :lisp-blog.util.crypto
                :hash-password
                :verify-password)
  (:import-from :lisp-blog.util.conditions
                :validation-error
                :authentication-error
                :resource-conflict-error)
  (:import-from :lisp-blog.middleware.session
                :create-session
                :get-session-user-id
                :delete-session)
  (:import-from :mito
                :find-dao
                :create-dao
                :object-id)
  (:import-from :sxql
                :where
                :limit)
  (:export :validate-username
           :validate-password
           :register-user
           :authenticate-user
           :get-user-by-id
           :get-user-by-session))
(in-package :lisp-blog.service.auth)

;;; バリデーション

(defun validate-username (username)
  "ユーザー名のバリデーション

   ルール:
   - 3-50文字
   - 英数字とアンダースコアのみ

   戻り値: T（有効）または NIL（無効）"
  (and (stringp username)
       (<= 3 (length username) 50)
       (cl-ppcre:scan "^[a-zA-Z0-9_]+$" username)))

(defun validate-password (password)
  "パスワードのバリデーション

   ルール:
   - 8-255文字

   戻り値: T（有効）または NIL（無効）"
  (and (stringp password)
       (<= 8 (length password) 255)))

;;; ユーザー登録

(defun username-exists-p (username)
  "ユーザー名が既に存在するかチェック

   username: ユーザー名文字列
   戻り値: T（存在する）または NIL（存在しない）"
  (let ((user (mito:find-dao 'user :username username)))
    (not (null user))))

(defun register-user (username password)
  "新規ユーザーを登録

   username: ユーザー名（3-50文字、英数字とアンダースコア）
   password: パスワード（8-255文字）

   戻り値:
   - 成功: (:user <user-object> :session-id <session-id>)
   - 失敗: Conditionを signal（validation-error または resource-conflict-error）

   発生する可能性のあるCondition:
   - validation-error - ユーザー名またはパスワードが無効
   - resource-conflict-error - ユーザー名が既に存在"
  (cond
    ((not (validate-username username))
     (error 'validation-error
            :message "Invalid username (3-50 chars, alphanumeric and underscore only)"
            :field "username"))

    ((not (validate-password password))
     (error 'validation-error
            :message "Invalid password (8-255 chars required)"
            :field "password"))

    ((username-exists-p username)
     (error 'resource-conflict-error
            :message "Username already exists"
            :resource-type "user"))

    (t
     ;; ユーザー作成
     (let* ((hashed-password (hash-password password))
            (user (mito:create-dao 'user
                                   :username username
                                   :password hashed-password))
            (session-id (create-session (mito:object-id user))))
       (list :user user
             :session-id session-id)))))

;;; 認証

(defun authenticate-user (username password)
  "ユーザー認証（ログイン）

   username: ユーザー名
   password: パスワード

   戻り値:
   - 成功: (:user <user-object> :session-id <session-id>)
   - 失敗: authentication-error を signal

   発生する可能性のあるCondition:
   - authentication-error - 認証情報が不正（ユーザー名またはパスワードが間違い）

   セキュリティ:
   - タイミング攻撃対策: ユーザーが存在しない場合もダミーのパスワード検証を実行"
  (let ((user (mito:find-dao 'user :username username)))
    (if user
        ;; ユーザーが存在する場合
        (if (verify-password password (user-password user))
            ;; パスワード正しい
            (let ((session-id (create-session (mito:object-id user))))
              (list :user user
                    :session-id session-id))
            ;; パスワード間違い
            (error 'authentication-error
                   :code :auth-invalid-credentials
                   :message "Invalid credentials"))
        ;; ユーザーが存在しない場合
        ;; タイミング攻撃対策: ダミーのパスワード検証を実行
        (progn
          (hash-password "dummy-password-for-timing-attack-prevention")
          (error 'authentication-error
                 :code :auth-invalid-credentials
                 :message "Invalid credentials")))))

;;; ユーザー取得

(defun get-user-by-id (user-id)
  "ユーザーIDからユーザーを取得

   user-id: ユーザーID（整数）
   戻り値: user オブジェクトまたは NIL"
  (mito:find-dao 'user :id user-id))

(defun get-user-by-session (session-id)
  "セッションIDからユーザーを取得

   session-id: セッションID文字列
   戻り値: user オブジェクトまたは NIL"
  (let ((user-id (get-session-user-id session-id)))
    (when user-id
      (get-user-by-id user-id))))
