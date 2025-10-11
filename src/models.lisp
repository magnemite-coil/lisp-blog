(in-package :lisp-blog)

(defclass post ()
  ((id :accessor post-id :initarg :id)
   (user-id :accessor post-user-id :initarg :user-id)
   (title :accessor post-title :initarg :title)
   (content :accessor post-content :initarg :content)
   (status :accessor post-status :initarg :status)
   (author-name :accessor post-author-name :initarg :author-name)
   (created-at :accessor post-created-at :initarg :created-at)
   (updated-at :accessor post-updated-at :initarg :updated-at)))

(defun get-all-posts ()
  "全ての投稿を取得（著者情報付き、statusカラム含む）"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                u.display_name, p.created_at, p.updated_at, p.status
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
                               :updated-at (nth 6 post)
                               :status (nth 7 post)))
              posts))))

(defun get-published-posts ()
  "公開済みの投稿のみを取得（一般ユーザー向け）"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                u.display_name, p.created_at, p.updated_at, p.status
                         FROM posts p
                         JOIN users u ON p.user_id = u.id
                         WHERE p.status = 'published'
                         ORDER BY p.created_at DESC")))
      (mapcar (lambda (post)
                (make-instance 'post
                               :id (nth 0 post)
                               :user-id (nth 1 post)
                               :title (nth 2 post)
                               :content (nth 3 post)
                               :author-name (nth 4 post)
                               :created-at (nth 5 post)
                               :updated-at (nth 6 post)
                               :status (nth 7 post)))
              posts))))

(defun count-published-posts ()
  "公開済み投稿の総数を取得"
  (with-db
    (query "SELECT COUNT(*) FROM posts WHERE status = 'published'"
           :single)))

(defun get-published-posts-paginated (page per-page)
  "公開済み投稿をページネーション付きで取得

  Parameters:
    page     - ページ番号 (1始まり)
    per-page - 1ページあたりの件数

  Returns:
    投稿のリスト (postクラスのインスタンス)"
  (let ((offset (* (- page 1) per-page)))
    (with-db
      (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                  u.display_name, p.created_at, p.updated_at, p.status
                           FROM posts p
                           JOIN users u ON p.user_id = u.id
                           WHERE p.status = 'published'
                           ORDER BY p.created_at DESC
                           LIMIT $1 OFFSET $2"
                          per-page offset)))
        (mapcar (lambda (post)
                  (make-instance 'post
                                 :id (nth 0 post)
                                 :user-id (nth 1 post)
                                 :title (nth 2 post)
                                 :content (nth 3 post)
                                 :author-name (nth 4 post)
                                 :created-at (nth 5 post)
                                 :updated-at (nth 6 post)
                                 :status (nth 7 post)))
                posts)))))

(defun get-post-by-id (id)
  "IDで投稿を取得（statusカラム含む）"
  (with-db
    (let ((post (query "SELECT p.id, p.user_id, p.title, p.content,
                               u.display_name, p.created_at, p.updated_at, p.status
                        FROM posts p
                        JOIN users u ON p.user_id = u.id
                        WHERE p.id = $1"
                       id :row)))
      (when post
        (make-instance 'post
                       :id (nth 0 post)
                       :user-id (nth 1 post)
                       :title (nth 2 post)
                       :content (nth 3 post)
                       :author-name (nth 4 post)
                       :created-at (nth 5 post)
                       :updated-at (nth 6 post)
                       :status (nth 7 post))))))









(defun get-posts-by-user (user-id)
  "特定ユーザーの投稿を取得（下書き含む、本人のみアクセス可能）"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                u.display_name, p.created_at, p.updated_at, p.status
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
                               :updated-at (nth 6 post)
                               :status (nth 7 post)))
              posts))))

(defun count-user-posts (user-id)
  "特定ユーザーの投稿総数を取得（公開・下書き両方）"
  (with-db
    (query "SELECT COUNT(*) FROM posts WHERE user_id = $1"
           user-id
           :single)))

(defun get-user-posts-paginated (user-id page per-page)
  "特定ユーザーの投稿をページネーション付きで取得（公開・下書き両方）

  Parameters:
    user-id  - ユーザーID
    page     - ページ番号 (1始まり)
    per-page - 1ページあたりの件数

  Returns:
    投稿のリスト (postクラスのインスタンス)"
  (let ((offset (* (- page 1) per-page)))
    (with-db
      (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                  u.display_name, p.created_at, p.updated_at, p.status
                           FROM posts p
                           JOIN users u ON p.user_id = u.id
                           WHERE p.user_id = $1
                           ORDER BY p.created_at DESC
                           LIMIT $2 OFFSET $3"
                          user-id per-page offset)))
        (mapcar (lambda (post)
                  (make-instance 'post
                                 :id (nth 0 post)
                                 :user-id (nth 1 post)
                                 :title (nth 2 post)
                                 :content (nth 3 post)
                                 :author-name (nth 4 post)
                                 :created-at (nth 5 post)
                                 :updated-at (nth 6 post)
                                 :status (nth 7 post)))
                posts)))))

(defun get-user-drafts (user-id)
  "特定ユーザーの下書きのみを取得"
  (with-db
    (let ((posts (query "SELECT p.id, p.user_id, p.title, p.content,
                                u.display_name, p.created_at, p.updated_at, p.status
                         FROM posts p
                         JOIN users u ON p.user_id = u.id
                         WHERE p.user_id = $1 AND p.status = 'draft'
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
                               :updated-at (nth 6 post)
                               :status (nth 7 post)))
              posts))))

(defun create-post (user-id title content &optional (status "published"))
  "新しい投稿を作成（status指定可能、デフォルトは公開）"
  (with-db
    (execute "INSERT INTO posts (user_id, title, content, status) VALUES ($1, $2, $3, $4)"
             user-id title content status)))

(defun update-post (post-id user-id title content &optional status)
  "投稿を更新（作成者のみ、status変更可能）"
  (with-db
    (let ((affected (if status
                        (execute "UPDATE posts
                                  SET title = $3, content = $4, status = $5, updated_at = NOW()
                                  WHERE id = $1 AND user_id = $2"
                                 post-id user-id title content status)
                        (execute "UPDATE posts
                                  SET title = $3, content = $4, updated_at = NOW()
                                  WHERE id = $1 AND user_id = $2"
                                 post-id user-id title content))))
      (> affected 0))))

(defun publish-draft (post-id user-id)
  "下書きを公開状態に変更（作成者のみ）"
  (with-db
    (let ((affected (execute "UPDATE posts
                              SET status = 'published', updated_at = NOW()
                              WHERE id = $1 AND user_id = $2 AND status = 'draft'"
                             post-id user-id)))
      (> affected 0))))

(defun unpublish-post (post-id user-id)
  "公開記事を下書きに戻す（作成者のみ）"
  (with-db
    (let ((affected (execute "UPDATE posts
                              SET status = 'draft', updated_at = NOW()
                              WHERE id = $1 AND user_id = $2 AND status = 'published'"
                             post-id user-id)))
      (> affected 0))))

(defun delete-post (post-id user-id)
  "投稿を削除（作成者のみ）"
  (with-db
    (let ((affected (execute "DELETE FROM posts WHERE id = $1 AND user_id = $2"
                             post-id user-id)))
      (> affected 0))))
