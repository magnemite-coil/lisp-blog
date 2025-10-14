(in-package :cl-user)
(defpackage lisp-blog.db
  (:use :cl)
  (:import-from :lisp-blog.config
                :*database*)
  (:export :connect-db
           :disconnect-db))
(in-package :lisp-blog.db)

(defun connect-db ()
  "データベースに接続する（コネクションプーリング有効）"
  (apply #'mito:connect-toplevel *database*))

(defun disconnect-db ()
  "データベース接続を切断する"
  (mito:disconnect-toplevel))
