(in-package :cl-user)
(defpackage lisp-blog
  (:use :cl)
  (:import-from :lisp-blog.config
                :*database*)
  (:import-from :lisp-blog.db
                :connect-db
                :disconnect-db)
  (:import-from :lisp-blog.middleware.session
                :connect-redis
                :disconnect-redis)
  (:import-from :lisp-blog.web
                :*app*)
  (:export :start
           :stop))
(in-package :lisp-blog)

(defvar *server* nil
  "Clackサーバーインスタンス")

(defun start (&key (port 8080))
  "サーバーを起動"
  (format t "~%Starting lisp-blog server on port ~A...~%" port)

  ;; データベース接続
  (format t "Connecting to PostgreSQL...~%")
  (connect-db)

  ;; Redis接続
  (format t "Connecting to Redis...~%")
  (connect-redis)

  ;; サーバー起動
  (format t "Starting Woo server...~%")
  (setf *server* (clack:clackup *app* :port port :server :woo))

  (format t "~%✅ Server started successfully!~%")
  (format t "   API endpoint: http://localhost:~A/~%~%" port)

  *server*)

(defun stop ()
  "サーバーを停止"
  (format t "~%Stopping server...~%")

  ;; Redis切断
  (format t "Disconnecting from Redis...~%")
  (disconnect-redis)

  ;; データベース切断
  (format t "Disconnecting from PostgreSQL...~%")
  (disconnect-db)

  ;; サーバー停止
  (when *server*
    (clack:stop *server*)
    (setf *server* nil))

  (format t "~%✅ Server stopped successfully!~%~%"))
