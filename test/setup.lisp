(in-package :cl-user)
(defpackage lisp-blog-test
  (:use :cl :fiveam)
  (:import-from :lisp-blog.config
                :*test-database*)
  (:import-from :lisp-blog.db
                :connect-db
                :disconnect-db)
  (:import-from :lisp-blog.middleware.session
                :*redis-connection*
                :connect-redis
                :disconnect-redis)
  (:export :run-tests
           :lisp-blog-test-suite
           :setup-test-environment
           :teardown-test-environment))
(in-package :lisp-blog-test)

;;; テストスイート定義

(def-suite lisp-blog-test-suite
  :description "lisp-blogの全テストスイート")

(defun run-tests ()
  "全テストを実行"
  (run! 'lisp-blog-test-suite))

;;; テスト用データベース接続

(defun connect-test-db ()
  "テスト用データベースに接続"
  (apply #'mito:connect-toplevel *test-database*))

(defun disconnect-test-db ()
  "テスト用データベース接続を切断"
  (mito:disconnect-toplevel))

;;; テスト用Redis接続

(defun connect-test-redis ()
  "テスト用Redis DB 1に接続"
  (setf *redis-connection*
        (redis:connect :host "localhost" :port 6379))
  ;; DB 1を選択（本番はDB 0、テストはDB 1）
  (redis:red-select 1))

(defun disconnect-test-redis ()
  "テスト用Redis接続を切断"
  (when *redis-connection*
    (redis:red-quit)
    (setf *redis-connection* nil)))

;;; クリーンアップ

(defun cleanup-test-db ()
  "テスト用DBの全テーブルを削除"
  (handler-case
      (progn
        (mito:execute-sql "DROP TABLE IF EXISTS \"post\" CASCADE")
        (mito:execute-sql "DROP TABLE IF EXISTS \"user\" CASCADE"))
    (error (e)
      (format t "Warning: cleanup-test-db failed: ~A~%" e))))

(defun cleanup-test-redis ()
  "テスト用Redis DBを全削除"
  (when *redis-connection*
    (redis:red-flushdb)))

;;; テーブル作成

(defun setup-test-tables ()
  "テスト用テーブルを作成"
  (mito:ensure-table-exists 'lisp-blog.model.user:user)
  (mito:ensure-table-exists 'lisp-blog.model.post:post))

;;; テスト初期化・終了処理

(defun setup-test-environment ()
  "テスト環境全体を初期化"
  (connect-test-db)
  (connect-test-redis)
  (cleanup-test-db)
  (cleanup-test-redis)
  (setup-test-tables))

(defun teardown-test-environment ()
  "テスト環境全体をクリーンアップ"
  (cleanup-test-redis)
  (disconnect-test-redis)
  (disconnect-test-db))
