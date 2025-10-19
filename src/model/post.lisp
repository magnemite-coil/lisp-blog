(in-package :cl-user)
(defpackage lisp-blog.model.post
  (:use :cl :mito)
  (:export :post
           :post-id
           :post-user-id
           :post-title
           :post-content
           :post-status
           :post-created-at
           :post-updated-at))
(in-package :lisp-blog.model.post)

(deftable post ()
  ((user-id :col-type :bigint
            :initarg :user-id
            :accessor post-user-id
            :documentation "投稿者のユーザーID（外部キー）")
   (title :col-type (:varchar 255)
          :initarg :title
          :accessor post-title
          :documentation "投稿タイトル")
   (content :col-type :text
            :initarg :content
            :accessor post-content
            :documentation "投稿本文（Markdown形式）")
   (status :col-type (:varchar 20)
           :initarg :status
           :accessor post-status
           :initform "draft"
           :documentation "投稿ステータス（draft | published）"))
  (:indexes (user-id status created-at))  ; 個別カラムインデックス
  (:keys (user-id status created-at))     ; 複合インデックス（user_id, status, created_at）
  (:documentation "ブログ投稿モデル"))

;;; アクセサー関数（Mitoが自動生成するフィールド用）

(defun post-id (post)
  "postのIDを取得"
  (mito:object-id post))

(defun post-created-at (post)
  "postの作成日時を取得"
  (slot-value post 'mito.dao.mixin::created-at))

(defun post-updated-at (post)
  "postの更新日時を取得"
  (slot-value post 'mito.dao.mixin::updated-at))
