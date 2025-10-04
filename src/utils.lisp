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

(defun valid-post-title-p (title)
  "投稿タイトルの妥当性チェック"
  (and title
       (string/= (string-trim " 　\t\n\r" title) "")
       (>= (length title) 1)
       (<= (length title) 255)))

(defun valid-post-content-p (content)
  "投稿本文の妥当性チェック"
  (and content
       (string/= (string-trim " 　\t\n\r" content) "")
       (>= (length content) 1)
       (<= (length content) 100000)))

(defun valid-display-name-p (display-name)
  "表示名の妥当性チェック"
  (and display-name
       (>= (length display-name) 1)
       (<= (length display-name) 100)))

;;; 入力サニタイゼーション

(defun sanitize-html (input)
  "HTMLタグを無効化（XSS対策）"
  (when input
    (cl-ppcre:regex-replace-all
     "[<>&\"']"
     input
     (lambda (match)
       (cond
         ((string= match "<") "&lt;")
         ((string= match ">") "&gt;")
         ((string= match "&") "&amp;")
         ((string= match "\"") "&quot;")
         ((string= match "'") "&#x27;")
         (t match))))))

(defun sanitize-input (input)
  "入力値の一般的なサニタイゼーション"
  (when input
    (string-trim " 　\t\n\r" (sanitize-html input))))

;;; バリデーションエラー管理

(define-condition validation-error (error)
  ((field :initarg :field :reader validation-error-field)
   (message :initarg :message :reader validation-error-message))
  (:report (lambda (condition stream)
             (format stream "Validation error in ~A: ~A"
                     (validation-error-field condition)
                     (validation-error-message condition)))))

(defun validate-input (field value validator error-message)
  "単一フィールドのバリデーション"
  (unless (funcall validator value)
    (error 'validation-error :field field :message error-message))
  value)

(defun validate-user-input (username email password &optional display-name)
  "ユーザー入力の包括的バリデーション"
  (validate-input :username username #'valid-username-p
                  "ユーザー名は3-50文字の英数字、アンダースコア、ハイフンである必要があります")
  (validate-input :email email #'valid-email-p
                  "有効なメールアドレス形式ではありません")
  (validate-input :password password #'valid-password-p
                  "パスワードは8-100文字である必要があります")
  (when display-name
    (validate-input :display-name display-name #'valid-display-name-p
                    "表示名は1-100文字である必要があります"))
  t)

(defun validate-post-input (title content)
  "投稿入力の包括的バリデーション"
  (validate-input :title title #'valid-post-title-p
                  "タイトルは1-255文字である必要があります")
  (validate-input :content content #'valid-post-content-p
                  "本文は1-100,000文字である必要があります")
  t)

;;; エラーレスポンス管理

(defun make-error-response (message &key field status-code (success nil))
  "統一されたエラーレスポンス形式"
  (let ((response (list (cons "success" success)
                        (cons "message" message))))
    (when field
      (push (cons "field" (string-downcase (symbol-name field))) response))
    (when status-code
      (setf (return-code*) status-code))
    response))

(defun make-success-response (data &optional message)
  "統一された成功レスポンス形式"
  (let ((response (list (cons "success" t))))
    (when message
      (push (cons "message" message) response))
    (when data
      (push (cons "data" data) response))
    response))

(defun make-validation-error-response (condition)
  "バリデーションエラー専用レスポンス"
  (make-error-response
   (validation-error-message condition)
   :field (validation-error-field condition)
   :status-code +http-bad-request+))

(defun respond-json (response)
  "JSON形式でレスポンスを送信"
  (setf (content-type*) "application/json; charset=utf-8")
  (with-output-to-string (s)
    (format s "{")
    (loop for (key . value) in response
          for first = t then nil
          unless first do (format s ",")
          do (format s "\"~A\":" key)
          do (cond
               ((stringp value) (format s "\"~A\"" value))
               ((symbolp value) (format s "\"~A\"" (string-downcase (symbol-name value))))
               ((numberp value) (format s "~A" value))
               ((eq value t) (format s "true"))
               ((eq value nil) (format s "false"))
               (t (format s "\"~A\"" value))))
    (format s "}")))

;;; エラーハンドリングマクロ

(defmacro with-validation-handler (&body body)
  "バリデーションエラーを適切に処理するマクロ"
  `(handler-case
       (progn ,@body)
     (validation-error (e)
       (respond-json (make-validation-error-response e)))
     (error (e)
       (respond-json (make-error-response
                      "内部エラーが発生しました"
                      :status-code +http-internal-server-error+)))))
