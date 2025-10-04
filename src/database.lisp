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
    ;; ユーザーテーブル
    (execute "CREATE TABLE IF NOT EXISTS users (
                id SERIAL PRIMARY KEY,
                username VARCHAR(50) UNIQUE NOT NULL,
                email VARCHAR(255) UNIQUE NOT NULL,
                password_hash VARCHAR(255) NOT NULL,
                display_name VARCHAR(100),
                bio TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    
    ;; セッションテーブル
    (execute "CREATE TABLE IF NOT EXISTS sessions (
                id VARCHAR(36) PRIMARY KEY,
                user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                expires_at TIMESTAMP NOT NULL
              )")
    
    ;; 投稿テーブル（user_idカラムを追加）
    (execute "CREATE TABLE IF NOT EXISTS posts (
                id SERIAL PRIMARY KEY,
                user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
                title VARCHAR(255) NOT NULL,
                content TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    
    ;; コメントテーブル
    (execute "CREATE TABLE IF NOT EXISTS comments (
                id SERIAL PRIMARY KEY,
                post_id INTEGER REFERENCES posts(id) ON DELETE CASCADE,
                user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
                content TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    
    ;; インデックスの作成
    (execute "CREATE INDEX IF NOT EXISTS idx_posts_user_id ON posts(user_id)")
    (execute "CREATE INDEX IF NOT EXISTS idx_sessions_user_id ON sessions(user_id)")
    (execute "CREATE INDEX IF NOT EXISTS idx_sessions_expires ON sessions(expires_at)")
    
    (format t "Database initialized successfully.~%")))

(defun drop-all-tables ()
  "全てのテーブルを削除（開発用）"
  (with-db
    (execute "DROP TABLE IF EXISTS comments CASCADE")
    (execute "DROP TABLE IF EXISTS posts CASCADE")
    (execute "DROP TABLE IF EXISTS sessions CASCADE")
    (execute "DROP TABLE IF EXISTS users CASCADE")
    (format t "All tables dropped.~%")))
