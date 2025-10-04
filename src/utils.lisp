(in-package :lisp-blog)

;;; パスワードハッシュ化

(defun hash-password (password)
  "パスワードをSHA256でハッシュ化"
  (let* ((salt (ironclad:make-random-salt))
         (digest (ironclad:pbkdf2-hash-password 
                  (babel:string-to-octets password)
                  :salt salt
                  :digest :sha256
                  :iterations 100000)))
    (concatenate 'string
                 (cl-base64:usb8-array-to-base64-string salt)
                 "$"
                 (cl-base64:usb8-array-to-base64-string digest))))

(defun verify-password (password hash)
  "パスワードとハッシュを照合"
  (let* ((parts (cl-ppcre:split "\\$" hash))
         (salt (cl-base64:base64-string-to-usb8-array (first parts)))
         (stored-hash (cl-base64:base64-string-to-usb8-array (second parts)))
         (computed-hash (ironclad:pbkdf2-hash-password
                         (babel:string-to-octets password)
                         :salt salt
                         :digest :sha256
                         :iterations 100000)))
    (equalp stored-hash computed-hash)))

;;; セッション管理

(defun generate-session-id ()
  "ユニークなセッションIDを生成"
  (format nil "~A" (uuid:make-v4-uuid)))


(defun get-session-user-id (session-id)
  "セッションIDからユーザーIDを取得"
  (when session-id
    (with-db
      (let ((result (query "SELECT user_id FROM sessions 
                           WHERE id = $1 AND expires_at > NOW()"
                           session-id :row)))  ; :single を :row に変更
        (when result
          (first result))))))



(defun create-session (user-id)
  "新しいセッションを作成"
  (let ((session-id (generate-session-id)))
    (with-db
      (execute "INSERT INTO sessions (id, user_id, expires_at) 
                VALUES ($1, $2, NOW() + INTERVAL '7 days')"
               session-id user-id))
    session-id))

(defun delete-session (session-id)
  "セッションを削除（ログアウト）"
  (when session-id
    (with-db
      (execute "DELETE FROM sessions WHERE id = $1" session-id))))

(defun clean-expired-sessions ()
  "期限切れセッションを削除"
  (with-db
    (execute "DELETE FROM sessions WHERE expires_at < NOW()")))

;;; バリデーション

(defun valid-username-p (username)
  "ユーザー名の妥当性チェック"
  (and username
       (>= (length username) 3)
       (<= (length username) 50)
       (cl-ppcre:scan "^[a-zA-Z0-9_-]+$" username)))

(defun valid-email-p (email)
  "メールアドレスの妥当性チェック"
  (and email
       (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email)))

(defun valid-password-p (password)
  "パスワードの妥当性チェック"
  (and password
       (>= (length password) 8)
       (<= (length password) 100)))
