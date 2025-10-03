(in-package :lisp-blog)

(defparameter *acceptor* nil)

(defun start-server (&key (port 8080))
  "Webサーバーを起動"
  (when *acceptor*
    (stop-server))
  (setf *acceptor* 
        (make-instance 'easy-acceptor 
                       :port port
                       :document-root #P"static/"))
  (start *acceptor*)
  (format t "Server started on http://localhost:~a~%" port))

(defun stop-server ()
  "Webサーバーを停止"
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped~%")))
