(in-package :cl-user)
(defpackage lisp-blog.web
  (:use :cl :caveman2)
  (:export :*app*))
(in-package :lisp-blog.web)

(defclass <web> (<app>) ())
(defvar *app* (make-instance '<web>))

;; ルーティングは後で追加
