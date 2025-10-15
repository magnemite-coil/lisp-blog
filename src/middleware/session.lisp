(in-package :cl-user)
(defpackage lisp-blog.middleware.session
  (:use :cl)
  (:import-from :ironclad
                :random-data)
  (:import-from :cl-base64
                :usb8-array-to-base64-string)
  (:export :*redis-connection*
           :connect-redis
           :disconnect-redis
           :create-session
           :get-session
           :delete-session
           :get-session-user-id))
(in-package :lisp-blog.middleware.session)

;;; Redis接続管理

(defvar *redis-connection* nil
  "Redisへの接続")

(defparameter *session-ttl* (* 7 24 60 60)
  "セッションの有効期限（秒）: 7日間")

(defun connect-redis (&key (host "localhost") (port 6379))
  "Redisサーバーに接続"
  (unless *redis-connection*
    (setf *redis-connection*
          (redis:connect :host host :port port)))
  *redis-connection*)

(defun disconnect-redis ()
  "Redis接続を切断"
  (when *redis-connection*
    (redis:disconnect *redis-connection*)
    (setf *redis-connection* nil)))

;;; セッションID生成

(defun generate-session-id ()
  "暗号学的に安全なセッションIDを生成

   128ビット（16バイト）のランダムデータをBase64エンコード
   例: 'xK8vP2mN9qR5wT7yU1bE3fH6jL4sD9aG'"
  (let ((random-bytes (ironclad:random-data 16)))
    (cl-base64:usb8-array-to-base64-string random-bytes)))

;;; Redisキー生成

(defun session-key (session-id)
  "Redisのセッションキーをフォーマットする

   session:xK8vP2mN9qR5wT7yU1bE3fH6jL4sD9aG"
  (format nil "session:~A" session-id))

;;; セッション操作

(defun create-session (user-id)
  "新しいセッションを作成し、セッションIDを返す

   user-id: ユーザーID
   戻り値: セッションID文字列"
  (unless *redis-connection*
    (error "Redis connection not established"))

  (let* ((session-id (generate-session-id))
         (key (session-key session-id)))
    ;; Redisにセッションデータを保存（Hash）
    (redis:red-hset key "user_id" (format nil "~A" user-id))
    (redis:red-hset key "created_at"
                    (format nil "~A" (get-universal-time)))

    ;; TTL（有効期限）を設定
    (redis:red-expire key *session-ttl*)

    session-id))

(defun get-session (session-id)
  "セッションIDからセッションデータを取得

   session-id: セッションID文字列
   戻り値: セッションデータ（plist）または NIL"
  (unless *redis-connection*
    (error "Redis connection not established"))

  (let ((key (session-key session-id)))
    (let ((user-id-str (redis:red-hget key "user_id")))
      (when user-id-str
        (list :user-id (parse-integer user-id-str)
              :session-id session-id)))))

(defun get-session-user-id (session-id)
  "セッションIDからユーザーIDを取得

   session-id: セッションID文字列
   戻り値: ユーザーID（整数）または NIL"
  (let ((session (get-session session-id)))
    (when session
      (getf session :user-id))))

(defun delete-session (session-id)
  "セッションを削除（ログアウト）

   session-id: セッションID文字列
   戻り値: T（成功）または NIL（セッションが存在しない）"
  (unless *redis-connection*
    (error "Redis connection not established"))

  (let ((key (session-key session-id)))
    (plusp (redis:red-del key))))
