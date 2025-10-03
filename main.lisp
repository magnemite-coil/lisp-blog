(load "~/quicklisp/setup.lisp")
(ql:quickload :lisp-blog)

(in-package :lisp-blog)

;; データベース初期化
(init-db)

;; サーバー起動
(start-server :port 8080)

;; サーバーを実行し続ける
(format t "~%========================================~%")
(format t "Server running at http://localhost:8080~%")
(format t "Press Ctrl+C to stop the server~%")
(format t "========================================~%~%")

(handler-case
    (loop (sleep 1))
  (sb-sys:interactive-interrupt ()
    (format t "~%Shutting down...~%")
    (stop-server)
    (sb-ext:exit)))
