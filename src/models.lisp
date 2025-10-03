(in-package :lisp-blog)

(defclass post ()
  ((id :accessor post-id :initarg :id)
   (title :accessor post-title :initarg :title)
   (content :accessor post-content :initarg :content)
   (author :accessor post-author :initarg :author)
   (created-at :accessor post-created-at :initarg :created-at)
   (updated-at :accessor post-updated-at :initarg :updated-at)))

(defun get-all-posts ()
  "全ての投稿を取得"
  (with-db
    (let ((posts (query "SELECT id, title, content, author, created_at, updated_at 
                         FROM posts ORDER BY created_at DESC")))
      (mapcar (lambda (post)
                (make-instance 'post
                               :id (first post)
                               :title (second post)
                               :content (third post)
                               :author (fourth post)
                               :created-at (fifth post)
                               :updated-at (sixth post)))
              posts))))

(defun get-post-by-id (id)
  "IDで投稿を取得"
  (with-db
    (let ((post (query "SELECT id, title, content, author, created_at, updated_at 
                        FROM posts WHERE id = $1" id :single)))
      (when post
        (make-instance 'post
                       :id (first post)
                       :title (second post)
                       :content (third post)
                       :author (fourth post)
                       :created-at (fifth post)
                       :updated-at (sixth post))))))

(defun create-post (title content author)
  "新しい投稿を作成"
  (with-db
    (execute "INSERT INTO posts (title, content, author) VALUES ($1, $2, $3)"
             title content author)))

(defun delete-post (id)
  "投稿を削除"
  (with-db
    (execute "DELETE FROM posts WHERE id = $1" id)))
