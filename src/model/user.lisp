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

;;; アクセサー関数（Mitoが自動生成するフィールド用）

(defun user-id (user)
  "userのIDを取得"
  (mito:object-id user))

(defun user-created-at (user)
  "userの作成日時を取得"
  (slot-value user 'mito.dao.mixin::created-at))

(defun user-updated-at (user)
  "userの更新日時を取得"
  (slot-value user 'mito.dao.mixin::updated-at))
