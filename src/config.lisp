(in-package :cl-user)
(defpackage lisp-blog.config
  (:use :cl)
  (:export :*database*
           :*test-database*))
(in-package :lisp-blog.config)

(defvar *database*
  '(:postgres
    :database-name "blogdb"
    :username "bloguser"
    :password ""
    :host "localhost"
    :port 5432)
  "データベース接続設定")

(defvar *test-database*
  '(:postgres
    :database-name "lisp_blog_test"
    :username "bloguser"
    :password ""
    :host "localhost"
    :port 5432)
  "テスト用データベース接続設定")
