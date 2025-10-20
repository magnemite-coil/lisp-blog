(in-package :cl-user)
(defpackage lisp-blog.middleware.cors
  (:use :cl)
  (:import-from :lack.component
                :call)
  (:export :cors-middleware))
(in-package :lisp-blog.middleware.cors)

(defun cors-middleware (app)
  "CORSヘッダーを追加するミドルウェア"
  (lambda (env)
    (let ((response (funcall (if (functionp app) app (lambda (e) (call app e))) env)))
      ;; レスポンスヘッダーにCORS設定を追加
      (destructuring-bind (status headers body) response
        (let ((new-headers (copy-list headers)))
          ;; CORSヘッダーを追加（既存のものは上書きしない）
          (unless (getf new-headers :access-control-allow-origin)
            (setf (getf new-headers :access-control-allow-origin) "http://localhost:5173"))
          (unless (getf new-headers :access-control-allow-credentials)
            (setf (getf new-headers :access-control-allow-credentials) "true"))
          (unless (getf new-headers :access-control-allow-methods)
            (setf (getf new-headers :access-control-allow-methods) "GET, POST, PUT, DELETE, OPTIONS"))
          (unless (getf new-headers :access-control-allow-headers)
            (setf (getf new-headers :access-control-allow-headers) "Content-Type, Authorization, Cookie"))
          (list status new-headers body))))))

