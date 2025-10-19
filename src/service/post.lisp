(in-package :cl-user)
(defpackage lisp-blog.service.post
  (:use :cl)
  (:import-from :lisp-blog.model.post
                :post
                :post-id
                :post-user-id
                :post-title
                :post-content
                :post-status
                :post-created-at
                :post-updated-at)
  (:import-from :lisp-blog.util.conditions
                :validation-error
                :business-logic-error)
  (:import-from :mito
                :create-dao
                :find-dao
                :select-dao
                :save-dao
                :delete-dao
                :object-id)
  (:import-from :sxql
                :where
                :order-by
                :limit)
  (:export :validate-title
           :validate-content
           :validate-status
           :create-post
           :get-post-by-id
           :get-published-posts
           :get-user-posts
           :get-user-drafts
           :update-post
           :delete-post
           :publish-draft
           :unpublish-post
           :check-post-ownership))
(in-package :lisp-blog.service.post)

;;; バリデーション関数

(defun validate-title (title)
  "投稿タイトルのバリデーション

   ルール:
   - 1-255文字

   戻り値: T（有効）または NIL（無効）"
  (and (stringp title)
       (<= 1 (length title) 255)))

(defun validate-content (content)
  "投稿本文のバリデーション

   ルール:
   - 1-100,000文字

   戻り値: T（有効）または NIL（無効）"
  (and (stringp content)
       (<= 1 (length content) 100000)))

(defun validate-status (status)
  "投稿ステータスのバリデーション

   ルール:
   - 'draft' または 'published' のみ

   戻り値: T（有効）または NIL（無効）"
  (member status '("draft" "published") :test #'string=))

;;; 投稿作成

(defun create-post (user-id title content &optional (status "draft"))
  "新規投稿を作成

   user-id: 投稿者のユーザーID
   title: 投稿タイトル（1-255文字）
   content: 投稿本文（1-100,000文字）
   status: 投稿ステータス（'draft' または 'published'、デフォルト: 'draft'）

   戻り値:
   - 成功: post オブジェクト
   - 失敗: Conditionを signal（validation-error）"
  (unless (validate-title title)
    (error 'validation-error
           :message "Invalid title (1-255 chars required)"
           :field "title"))

  (unless (validate-content content)
    (error 'validation-error
           :message "Invalid content (1-100,000 chars required)"
           :field "content"))

  (unless (validate-status status)
    (error 'validation-error
           :message "Invalid status (must be 'draft' or 'published')"
           :field "status"))

  (mito:create-dao 'post
                   :user-id user-id
                   :title title
                   :content content
                   :status status))

;;; 投稿取得

(defun get-post-by-id (post-id)
  "投稿IDから投稿を取得

   post-id: 投稿ID（整数）
   戻り値: post オブジェクトまたは NIL"
  (mito:find-dao 'post :id post-id))

(defun get-published-posts ()
  "公開済み投稿を全て取得

   戻り値: post オブジェクトのリスト（新しい順）"
  (mito:select-dao 'post
    (sxql:where (:= :status "published"))
    (sxql:order-by (:desc :created-at))))

(defun get-user-posts (user-id)
  "ユーザーの全投稿を取得（下書き含む）

   user-id: ユーザーID
   戻り値: post オブジェクトのリスト（新しい順）"
  (mito:select-dao 'post
    (sxql:where (:= :user-id user-id))
    (sxql:order-by (:desc :created-at))))

(defun get-user-drafts (user-id)
  "ユーザーの下書き一覧を取得

   user-id: ユーザーID
   戻り値: post オブジェクトのリスト（新しい順）"
  (mito:select-dao 'post
    (sxql:where (:and (:= :user-id user-id)
                      (:= :status "draft")))
    (sxql:order-by (:desc :created-at))))

;;; 投稿更新

(defun update-post (post title content)
  "投稿を更新

   post: 更新する post オブジェクト
   title: 新しいタイトル（1-255文字）
   content: 新しい本文（1-100,000文字）

   戻り値: 更新された post オブジェクト
   失敗: Conditionを signal（validation-error）"
  (unless (validate-title title)
    (error 'validation-error
           :message "Invalid title (1-255 chars required)"
           :field "title"))

  (unless (validate-content content)
    (error 'validation-error
           :message "Invalid content (1-100,000 chars required)"
           :field "content"))

  (setf (post-title post) title)
  (setf (post-content post) content)
  (mito:save-dao post)
  post)

(defun delete-post (post)
  "投稿を削除

   post: 削除する post オブジェクト
   戻り値: T（削除成功）"
  (mito:delete-dao post)
  t)

(defun publish-draft (post)
  "下書きを公開する

   post: 公開する post オブジェクト
   戻り値: 更新された post オブジェクト
   失敗: Conditionを signal（business-logic-error）"
  (when (string= (post-status post) "published")
    (error 'business-logic-error
           :code :post-already-published
           :message "Post is already published"))

  (setf (post-status post) "published")
  (mito:save-dao post)
  post)

(defun unpublish-post (post)
  "公開記事を下書きに戻す

   post: 下書きに戻す post オブジェクト
   戻り値: 更新された post オブジェクト
   失敗: Conditionを signal（business-logic-error）"
  (when (string= (post-status post) "draft")
    (error 'business-logic-error
           :code :post-already-draft
           :message "Post is already a draft"))

  (setf (post-status post) "draft")
  (mito:save-dao post)
  post)

;;; 権限チェック

(defun check-post-ownership (post user-id)
  "投稿の所有権を確認

   post: 確認する post オブジェクト
   user-id: ユーザーID

   戻り値: T（所有者）または NIL（所有者でない）"
  (= user-id (post-user-id post)))
