(in-package :cl-user)
(defpackage lisp-blog.model.user
  (:use :cl :mito)
  (:export :user
           :user-id
           :user-username
           :user-password
           :user-created-at
           :user-updated-at))
(in-package :lisp-blog.model.user)

(deftable user ()
  ((username :col-type (:varchar 50)
             :initarg :username
             :accessor user-username
             :documentation "ログインに使用するユーザー名（一意）")
   (password :col-type (:varchar 255)
             :initarg :password
             :accessor user-password
             :documentation "PBKDF2でハッシュ化されたパスワード"))
  (:unique-keys username)
  (:documentation "システムユーザーモデル"))
