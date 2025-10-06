(in-package :lisp-blog-test)

;;; utils.lisp のテストスイート

(def-suite utils-suite
  :description "utils.lisp の関数をテストするスイート")

(in-suite utils-suite)

;;; パスワードハッシュ化のテスト

(test test-hash-password-creates-hash
  "パスワードをハッシュ化すると元のパスワードと異なる文字列が返る"
  (let* ((password "mypassword123")
         (hash (lisp-blog::hash-password password)))
    (is (stringp hash))
    (is (not (string= password hash)))))

(test test-hash-password-contains-salt
  "ハッシュにソルトとハッシュが $ で分割されて含まれている"
  (let ((hash (lisp-blog::hash-password "password")))
    (is (find #\$ hash))
    (is (= 1 (count #\$ hash)))))

(test test-hash-password-different-salts
  "同じパスワードでも異なるハッシュが生成される（ソルトがランダム）"
  (let ((hash1 (lisp-blog::hash-password "password123"))
        (hash2 (lisp-blog::hash-password "password123")))
    (is (not (string= hash1 hash2)))))

(test test-verify-password-correct
  "正しいパスワードの検証に成功する"
  (let* ((password "testpassword")
         (hash (lisp-blog::hash-password password)))
    (is (lisp-blog::verify-password password hash))))

(test test-verify-password-incorrect
  "間違ったパスワードの検証に失敗する"
  (let ((hash (lisp-blog::hash-password "correctpassword")))
    (is (not (lisp-blog::verify-password "wrongpassword" hash)))))

(test test-verify-password-empty-string
  "空文字列での検証に失敗する"
  (let ((hash (lisp-blog::hash-password "password")))
    (is (not (lisp-blog::verify-password "" hash)))))

;;; セッションID生成のテスト

(test test-generate-session-id-returns-string
  "セッションIDが文字列を返す"
  (let ((session-id (lisp-blog::generate-session-id)))
    (is (stringp session-id))))

(test test-generate-session-id-not-empty
  "セッションIDが空文字列ではない"
  (let ((session-id (lisp-blog::generate-session-id)))
    (is (> (length session-id) 0))))

(test test-generate-session-id-unique
  "複数回生成で異なるIDが生成される"
  (let ((ids (loop repeat 100 collect (lisp-blog::generate-session-id))))
    (is (= 100 (length (remove-duplicates ids :test #'string=))))))

;;; バリデーション関数のテスト

(test test-valid-username-correct
  "正常なユーザー名が有効と判定される"
  (is (lisp-blog::valid-username-p "user123")))

(test test-valid-username-min-length
  "最小長（3文字）のユーザー名が有効"
  (is (lisp-blog::valid-username-p "abc")))

(test test-valid-username-too-short
  "3文字未満のユーザー名が無効"
  (is (not (lisp-blog::valid-username-p "ab"))))

(test test-valid-username-too-long
  "50文字超過のユーザー名が無効"
  (is (not (lisp-blog::valid-username-p (make-string 51 :initial-element #\a)))))

(test test-valid-username-invalid-chars
  "不正な文字を含むユーザー名が無効"
  (is (not (lisp-blog::valid-username-p "user@name"))))

(test test-valid-username-empty
  "空文字列のユーザー名が無効"
  (is (not (lisp-blog::valid-username-p ""))))

(test test-valid-username-nil
  "nilのユーザー名が無効"
  (is (not (lisp-blog::valid-username-p nil))))

(test test-valid-email-correct
  "正常なメールアドレスが有効"
  (is (lisp-blog::valid-email-p "user@example.com")))

(test test-valid-email-subdomain
  "サブドメイン付きメールアドレスが有効"
  (is (lisp-blog::valid-email-p "user@mail.example.com")))

(test test-valid-email-no-at
  "@がないメールアドレスが無効"
  (is (not (lisp-blog::valid-email-p "userexample.com"))))

(test test-valid-email-nil
  "nilのメールアドレスが無効"
  (is (not (lisp-blog::valid-email-p nil))))

(test test-valid-password-correct
  "正常なパスワードが有効"
  (is (lisp-blog::valid-password-p "password123")))

(test test-valid-password-min-length
  "最小長（8文字）のパスワードが有効"
  (is (lisp-blog::valid-password-p "12345678")))

(test test-valid-password-too-short
  "8文字未満のパスワードが無効"
  (is (not (lisp-blog::valid-password-p "1234567"))))

(test test-valid-password-nil
  "nilのパスワードが無効"
  (is (not (lisp-blog::valid-password-p nil))))

;;; サニタイゼーション関数のテスト

(test test-sanitize-html-script-tag
  "scriptタグがエスケープされる"
  (is (string= "&lt;script&gt;alert(1)&lt;/script&gt;"
               (lisp-blog::sanitize-html "<script>alert(1)</script>"))))

(test test-sanitize-html-double-quote
  "ダブルクォートがエスケープされる"
  (is (search "&quot;" (lisp-blog::sanitize-html "Hello \"World\""))))

(test test-sanitize-html-nil
  "nilを渡すとnilが返る"
  (is (null (lisp-blog::sanitize-html nil))))

(test test-sanitize-input-whitespace
  "前後の空白が削除される"
  (is (string= "hello" (lisp-blog::sanitize-input "  hello  "))))

(test test-sanitize-input-html-and-whitespace
  "HTMLエスケープと空白削除が同時に行われる"
  (let ((result (lisp-blog::sanitize-input "  <b>hello</b>  ")))
    (is (string= "&lt;b&gt;hello&lt;/b&gt;" result))))
