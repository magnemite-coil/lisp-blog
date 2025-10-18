(ql:quickload :lisp-blog)
(lisp-blog:start :port 8080)

;; サーバーを起動後、REPLループに入って終了しないようにする
(loop (sleep 1))
