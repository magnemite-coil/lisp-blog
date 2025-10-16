(in-package :lisp-blog-test)

;;; JSON処理のテストスイート

(def-suite json-tests
  :in lisp-blog-test-suite
  :description "JSON処理のテスト")

(in-suite json-tests)

;;; json-success のテスト

(test json-success-response
  "成功レスポンスのJSON生成"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success (list :|id| 1 :|username| "test"))
    (is (= 200 status))
    (is (string= "application/json; charset=utf-8"
                 (getf headers :content-type)))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (getf parsed :|success|))
      (is (= 1 (getf (getf parsed :|data|) :|id|)))
      (is (string= "test" (getf (getf parsed :|data|) :|username|))))))

(test json-success-with-message
  "メッセージ付き成功レスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success nil :message "Operation successful")
    (is (= 200 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (getf parsed :|success|))
      (is (string= "Operation successful" (getf parsed :|message|))))))

(test json-success-with-custom-status
  "カスタムステータスコード付き成功レスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success (list :|id| 1) :status 201)
    (is (= 201 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (getf parsed :|success|))
      (is (= 1 (getf (getf parsed :|data|) :|id|))))))

(test json-success-with-nil-data
  "nilデータの成功レスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success nil)
    (is (= 200 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (getf parsed :|success|))
      ;; nilはJSONのnullになる
      (is (null (getf parsed :|data|))))))

(test json-success-with-empty-list
  "空リストの成功レスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success (list))
    (is (= 200 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (getf parsed :|success|)))))

(test json-success-with-array-data
  "配列データの成功レスポンス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success
        (list (list :|id| 1 :|name| "item1")
              (list :|id| 2 :|name| "item2")))
    (is (= 200 status))
    (let* ((parsed (jonathan:parse json :as :plist))
           (data (getf parsed :|data|)))
      (is (getf parsed :|success|))
      (is (= 2 (length data)))
      (is (= 1 (getf (first data) :|id|)))
      (is (string= "item1" (getf (first data) :|name|))))))

;;; json-error のテスト

(test json-error-response
  "エラーレスポンスのJSON生成"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Invalid input" :status 400)
    (is (= 400 status))
    (is (string= "application/json; charset=utf-8"
                 (getf headers :content-type)))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (not (getf parsed :|success|)))
      (is (string= "Invalid input" (getf parsed :|error|))))))

(test json-error-default-status
  "エラーレスポンスのデフォルトステータス"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Error message")
    ;; デフォルトは400
    (is (= 400 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (not (getf parsed :|success|)))
      (is (string= "Error message" (getf parsed :|error|))))))

(test json-error-various-status-codes
  "各種ステータスコードのエラーレスポンス"
  ;; 401 Unauthorized
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Not authenticated" :status 401)
    (is (= 401 status)))

  ;; 403 Forbidden
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Permission denied" :status 403)
    (is (= 403 status)))

  ;; 404 Not Found
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Not found" :status 404)
    (is (= 404 status)))

  ;; 409 Conflict
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Already exists" :status 409)
    (is (= 409 status)))

  ;; 500 Internal Server Error
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Server error" :status 500)
    (is (= 500 status))))

(test json-error-empty-message
  "空のエラーメッセージ"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "")
    (is (= 400 status))
    (let ((parsed (jonathan:parse json :as :plist)))
      (is (not (getf parsed :|success|)))
      (is (string= "" (getf parsed :|error|))))))

(test json-error-long-message
  "長いエラーメッセージ"
  (let ((long-message (make-string 1000 :initial-element #\a)))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.json:json-error long-message)
      (is (= 400 status))
      (let ((parsed (jonathan:parse json :as :plist)))
        (is (not (getf parsed :|success|)))
        (is (string= long-message (getf parsed :|error|)))))))

(test json-error-special-characters
  "特殊文字を含むエラーメッセージ"
  (let ((message "Error: Invalid input \"test\" with <tags>"))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.json:json-error message)
      (let ((parsed (jonathan:parse json :as :plist)))
        (is (not (getf parsed :|success|)))
        ;; JSONエスケープされているはず
        (is (string= message (getf parsed :|error|)))))))

(test json-error-unicode-message
  "Unicode文字を含むエラーメッセージ"
  (let ((message "エラー: 無効な入力です"))
    (multiple-value-bind (json status headers)
        (lisp-blog.util.json:json-error message)
      (let ((parsed (jonathan:parse json :as :plist)))
        (is (not (getf parsed :|success|)))
        (is (string= message (getf parsed :|error|)))))))

;;; Content-Typeヘッダーのテスト

(test json-response-content-type
  "Content-Typeヘッダーが正しく設定されている"
  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-success nil)
    (is (string= "application/json; charset=utf-8"
                 (getf headers :content-type))))

  (multiple-value-bind (json status headers)
      (lisp-blog.util.json:json-error "Error")
    (is (string= "application/json; charset=utf-8"
                 (getf headers :content-type)))))
