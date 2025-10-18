(in-package :cl-user)
(defpackage lisp-blog.util.json
  (:use :cl)
  (:import-from :jonathan
                :to-json)
  (:export :json-response
           :json-success
           :json-error))
(in-package :lisp-blog.util.json)

(defun json-response (data &key (status 200))
  "JSON形式のHTTPレスポンスを返す

   data: JSONに変換するデータ（ハッシュテーブル、plist、alist等）
   status: HTTPステータスコード（デフォルト200）

   戻り値: (values json-string status headers)"
  (values (jonathan:to-json data)
          status
          '(:content-type "application/json; charset=utf-8"
            :access-control-allow-origin "http://localhost:5173"
            :access-control-allow-credentials "true"
            :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS"
            :access-control-allow-headers "Content-Type, Authorization")))

(defun json-success (data &key (status 200) message)
  "成功レスポンスのJSON

   data: レスポンスデータ
   status: HTTPステータスコード（デフォルト200）
   message: オプションのメッセージ

   レスポンス形式:
   {\"success\": true, \"data\": ...}
   または
   {\"success\": true, \"data\": ..., \"message\": \"...\"}"
  (let ((response (list :|success| t
                       :|data| data)))
    (when message
      (setf response (append response (list :|message| message))))
    (json-response response :status status)))

(defun json-error (error-message &key (status 400))
  "エラーレスポンスのJSON

   error-message: エラーメッセージ文字列
   status: HTTPステータスコード（デフォルト400）

   レスポンス形式:
   {\"success\": false, \"error\": \"エラーメッセージ\"}"
  (json-response (list :|success| :false
                      :|error| error-message)
                :status status))
