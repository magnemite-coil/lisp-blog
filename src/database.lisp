(in-package :lisp-blog)

(defparameter *db-spec* 
  '("blogdb" "bloguser" "" "localhost")
  "データベース接続仕様")

(defmacro with-db (&body body)
  "データベース接続を自動管理"
  `(with-connection *db-spec*
     ,@body))

(defun init-db ()
  "データベーステーブルの初期化"
  (with-db
    (execute "CREATE TABLE IF NOT EXISTS posts (
                id SERIAL PRIMARY KEY,
                title VARCHAR(255) NOT NULL,
                content TEXT NOT NULL,
                author VARCHAR(100) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    (execute "CREATE TABLE IF NOT EXISTS comments (
                id SERIAL PRIMARY KEY,
                post_id INTEGER REFERENCES posts(id) ON DELETE CASCADE,
                author VARCHAR(100) NOT NULL,
                content TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    (format t "Database initialized successfully.~%")))
