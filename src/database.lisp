(in-package :lisp-blog)

(defparameter *db-spec*
  '("blogdb" "bloguser" "" "localhost")
  "データベース接続仕様")

(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
実行時に*db-spec*を評価するため、テスト時に動的に接続先を変更可能
既存の接続がある場合はそれを再利用する"
  (if (and (boundp 'postmodern:*database*)
           postmodern:*database*)
      ;; 既に接続がある場合はそのまま使用
      (funcall fn)
      ;; 接続がない場合は新規接続
      ;; with-connection マクロを使わず、apply を使って実行時に接続
      (let ((db (first *db-spec*))
            (user (second *db-spec*))
            (password (third *db-spec*))
            (host (fourth *db-spec*)))
        (postmodern:with-connection (list db user password host)
          (funcall fn)))))

(defmacro with-db (&body body)
  "データベース接続を自動管理
内部的にcall-with-db関数を呼び出すことで、実行時の柔軟性を確保"
  `(call-with-db (lambda () ,@body)))

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

(defun add-draft-status-column ()
  "postsテーブルにstatusカラムを追加（下書き機能用）"
  (with-db
    (handler-case
        (progn
          ;; statusカラムを追加（デフォルト値'published'で既存データ保護）
          (execute "ALTER TABLE posts ADD COLUMN status VARCHAR(20) DEFAULT 'published'")
          ;; 既存レコードに明示的にpublished値を設定
          (execute "UPDATE posts SET status = 'published' WHERE status IS NULL")
          ;; CHECK制約を追加してstatus値を制限
          (execute "ALTER TABLE posts ADD CONSTRAINT posts_status_check
                    CHECK (status IN ('draft', 'published'))")
          (format t "Successfully added status column to posts table.~%")
          (format t "Existing posts marked as 'published'.~%"))
      ;; カラムが既に存在する場合のエラーハンドリング
      (cl-postgres:database-error (e)
        (if (search "already exists" (princ-to-string e))
            (format t "Status column already exists, skipping migration.~%")
            (error e))))))

(defun migrate-database ()
  "データベースマイグレーションを実行"
  (format t "Starting database migration...~%")
  (add-draft-status-column)
  (format t "Database migration completed.~%"))
