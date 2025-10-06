(in-package :lisp-blog)

(defparameter *acceptor* nil)

(defun get-static-directory ()
  "静的ファイルのディレクトリを取得"
  (let ((dir (merge-pathnames "static/" 
                              (asdf:system-source-directory :lisp-blog))))
    (ensure-directories-exist dir)
    dir))

(defun start-server (&key (port 8080))
  "Webサーバーを起動"
  (when *acceptor*
    (stop-server))

  ;; 翻訳ファイルをロード
  (load-translations)

  (let ((static-dir (get-static-directory)))
    (format t "Static directory: ~a~%" static-dir)
    (format t "Directory exists: ~a~%" (probe-file static-dir))

    (setf *acceptor*
          (make-instance 'easy-acceptor
                         :port port
                         :document-root static-dir))

    ;; 静的ファイル用のハンドラーを明示的に追加
    (push (create-folder-dispatcher-and-handler "/static/" static-dir)
          hunchentoot:*dispatch-table*)

    (start *acceptor*)
    (format t "Server started on http://localhost:~a~%" port)))

(defun stop-server ()
  "Webサーバーを停止"
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped~%")))
