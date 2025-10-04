(in-package :lisp-blog)

(defclass post ()
  ((id :accessor post-id :initarg :id)
   (user-id :accessor post-user-id :initarg :user-id)
   (title :accessor post-title :initarg :title)
   (content :accessor post-content :initarg :content)
   (author-name :accessor post-author-name :initarg :author-name)
   (created-at :accessor post-created-at :initarg :created-at)
   (updated-at :accessor post-updated-at :initarg :updated-at)))

(defun get-all-posts ()
  "全ての投稿を取得（著者情報付き）"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content, 
                                u.display_name, p.created_at, p.updated_at
                         FROM posts p
                         JOIN users u ON p.user_id = u.id
                         ORDER BY p.created_at DESC")))
      (mapcar (lambda (post)
                (make-instance 'post
                               :id (nth 0 post)
                               :user-id (nth 1 post)
                               :title (nth 2 post)
                               :content (nth 3 post)
                               :author-name (nth 4 post)
                               :created-at (nth 5 post)
                               :updated-at (nth 6 post)))
              posts))))

(defun get-post-by-id (id)
  "IDで投稿を取得"
  (with-db
    (let ((post (query "SELECT p.id, p.user_id, p.title, p.content, 
                               u.display_name, p.created_at, p.updated_at
                        FROM posts p
                        JOIN users u ON p.user_id = u.id
                        WHERE p.id = $1" 
                       id :row)))  ; :single を :row に変更
      (when post
        (make-instance 'post
                       :id (nth 0 post)
                       :user-id (nth 1 post)
                       :title (nth 2 post)
                       :content (nth 3 post)
                       :author-name (nth 4 post)
                       :created-at (nth 5 post)
                       :updated-at (nth 6 post))))))









(defun get-posts-by-user (user-id)
  "特定ユーザーの投稿を取得"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content, 
                                u.display_name, p.created_at, p.updated_at
                         FROM posts p
                         JOIN users u ON p.user_id = u.id
                         WHERE p.user_id = $1
                         ORDER BY p.created_at DESC" 
                        user-id)))
      (mapcar (lambda (post)
                (make-instance 'post
                               :id (nth 0 post)
                               :user-id (nth 1 post)
                               :title (nth 2 post)
                               :content (nth 3 post)
                               :author-name (nth 4 post)
                               :created-at (nth 5 post)
                               :updated-at (nth 6 post)))
              posts))))

(defun create-post (user-id title content)
  "新しい投稿を作成"
  (with-db
    (execute "INSERT INTO posts (user_id, title, content) VALUES ($1, $2, $3)"
             user-id title content)))

(defun update-post (post-id user-id title content)
  "投稿を更新（作成者のみ）"
  (with-db
    (let ((affected (execute "UPDATE posts 
                              SET title = $3, content = $4, updated_at = NOW()
                              WHERE id = $1 AND user_id = $2"
                             post-id user-id title content)))
      (> affected 0))))

(defun delete-post (post-id user-id)
  "投稿を削除（作成者のみ）"
  (with-db
    (let ((affected (execute "DELETE FROM posts WHERE id = $1 AND user_id = $2"
                             post-id user-id)))
      (> affected 0))))
