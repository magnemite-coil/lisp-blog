(in-package :lisp-blog)

;;; ユーザー管理

(defclass user ()
  ((id :accessor user-id :initarg :id)
   (username :accessor user-username :initarg :username)
   (email :accessor user-email :initarg :email)
   (display-name :accessor user-display-name :initarg :display-name)
   (bio :accessor user-bio :initarg :bio)
   (created-at :accessor user-created-at :initarg :created-at)))

(defun create-user (username email password &optional display-name bio)
  "新しいユーザーを作成"
  ;; バリデーション（新しいシステムを使用）
  (validate-input :username username #'valid-username-p
                  "ユーザー名は3-50文字の英数字、アンダースコア、ハイフンである必要があります")
  (validate-input :email email #'valid-email-p
                  "有効なメールアドレス形式ではありません")
  (validate-input :password password #'valid-password-p
                  "パスワードは8-100文字である必要があります")

  (with-db
    ;; ユーザー名の重複チェック
    (when (query "SELECT id FROM users WHERE username = $1" username :single)
      (error 'validation-error :field :username :message "このユーザー名は既に使用されています"))

    ;; メールアドレスの重複チェック
    (when (query "SELECT id FROM users WHERE email = $1" email :single)
      (error 'validation-error :field :email :message "このメールアドレスは既に使用されています"))
    
    ;; ユーザーを作成
    (let ((password-hash (hash-password password)))
      (execute "INSERT INTO users (username, email, password_hash, display_name, bio) 
                VALUES ($1, $2, $3, $4, $5)"
               username email password-hash 
               (or display-name username) 
               (or bio "")))))


(defun authenticate-user (username password)
  "ユーザー認証を行い、成功したらユーザーIDを返す"
  (with-db
    (let ((result (query "SELECT id, password_hash FROM users WHERE username = $1" 
                         username :row)))  ; :single を :row に変更
      (when result
        (let ((user-id (first result))
              (password-hash (second result)))
          (when (verify-password password password-hash)
            user-id))))))






(defun get-user-by-id (user-id)
  "IDでユーザーを取得"
  (with-db
    (let ((result (query "SELECT id, username, email, display_name, bio, created_at 
                          FROM users WHERE id = $1" 
                         user-id :row)))  ; :single を :row に変更
      (when result
        (make-instance 'user
                       :id (nth 0 result)
                       :username (nth 1 result)
                       :email (nth 2 result)
                       :display-name (nth 3 result)
                       :bio (nth 4 result)
                       :created-at (nth 5 result))))))

(defun get-user-by-username (username)
  "ユーザー名でユーザーを取得"
  (with-db
    (let ((result (query "SELECT id, username, email, display_name, bio, created_at 
                          FROM users WHERE username = $1" 
                         username :row)))  ; :single を :row に変更
      (when result
        (make-instance 'user
                       :id (nth 0 result)
                       :username (nth 1 result)
                       :email (nth 2 result)
                       :display-name (nth 3 result)
                       :bio (nth 4 result)
                       :created-at (nth 5 result))))))





(defun update-user-profile (user-id &key display-name bio)
  "ユーザープロフィールを更新"
  (with-db
    (execute "UPDATE users SET 
              display_name = COALESCE($2, display_name),
              bio = COALESCE($3, bio),
              updated_at = NOW()
              WHERE id = $1"
             user-id display-name bio)))

;;; セッション管理ヘルパー

(defun get-current-user ()
  "現在ログイン中のユーザーを取得"
  (let ((session-id (cookie-in "session_id")))
    (when session-id
      (with-db
        ;; セッション確認とユーザー情報取得を1つのクエリで実行
        (let ((result (query "SELECT u.id, u.username, u.email, u.display_name, u.bio, u.created_at
                              FROM users u
                              JOIN sessions s ON u.id = s.user_id
                              WHERE s.id = $1 AND s.expires_at > NOW()"
                             session-id :row)))
          (when result
            (make-instance 'user
                           :id (nth 0 result)
                           :username (nth 1 result)
                           :email (nth 2 result)
                           :display-name (nth 3 result)
                           :bio (nth 4 result)
                           :created-at (nth 5 result))))))))

(defun require-login ()
  "ログインが必要な処理で使用"
  (unless (get-current-user)
    (setf (return-code*) +http-unauthorized+)
    (throw 'handler-done 
           (with-output-to-string (s)
             (yason:encode (list (cons "error" "Login required")) s)))))
