(in-package :cl-user)
(defpackage lisp-blog
  (:use :cl)
  (:import-from :lisp-blog.config
                :*database*)
  (:import-from :lisp-blog.db
                :connect-db
                :disconnect-db)
  (:import-from :lisp-blog.web
                :*app*)
  (:export :start
           :stop))
(in-package :lisp-blog)

(defun start (&key (port 8080))
  "サーバーを起動"
  (connect-db)
  (clack:clackup *app* :port port :server :woo))

(defun stop ()
  "サーバーを停止"
  (disconnect-db))
