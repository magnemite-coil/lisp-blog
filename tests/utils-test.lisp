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

(test test-valid-username-with-hyphen
  "ハイフン付きユーザー名が有効"
  (is (lisp-blog::valid-username-p "user-name")))

(test test-valid-username-with-underscore
  "アンダースコア付きユーザー名が有効"
  (is (lisp-blog::valid-username-p "user_name")))

(test test-valid-username-spaces
  "スペースを含むユーザー名が無効"
  (is (not (lisp-blog::valid-username-p "user name"))))

(test test-valid-username-max-length
  "最大長（50文字）のユーザー名が有効"
  (is (lisp-blog::valid-username-p (make-string 50 :initial-element #\a))))

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

(test test-valid-email-no-domain
  "ドメインなしメールアドレスが無効"
  (is (not (lisp-blog::valid-email-p "user@"))))

(test test-valid-email-no-tld
  "TLDなしメールアドレスが無効"
  (is (not (lisp-blog::valid-email-p "user@example"))))

(test test-valid-email-empty
  "空文字列のメールアドレスが無効"
  (is (not (lisp-blog::valid-email-p ""))))

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

(test test-valid-password-max-length
  "最大長（100文字）のパスワードが有効"
  (is (lisp-blog::valid-password-p (make-string 100 :initial-element #\a))))

(test test-valid-password-too-long
  "100文字超過のパスワードが無効"
  (is (not (lisp-blog::valid-password-p (make-string 101 :initial-element #\a)))))

(test test-valid-password-empty
  "空文字列のパスワードが無効"
  (is (not (lisp-blog::valid-password-p ""))))

;;; 投稿バリデーション関数のテスト

(test test-valid-post-title-correct
  "正常な投稿タイトルが有効"
  (is (lisp-blog::valid-post-title-p "Sample Post Title")))

(test test-valid-post-title-min-length
  "最小長（1文字）の投稿タイトルが有効"
  (is (lisp-blog::valid-post-title-p "A")))

(test test-valid-post-title-max-length
  "最大長（255文字）の投稿タイトルが有効"
  (is (lisp-blog::valid-post-title-p (make-string 255 :initial-element #\a))))

(test test-valid-post-title-too-long
  "255文字超過の投稿タイトルが無効"
  (is (not (lisp-blog::valid-post-title-p (make-string 256 :initial-element #\a)))))

(test test-valid-post-title-empty
  "空文字列の投稿タイトルが無効"
  (is (not (lisp-blog::valid-post-title-p ""))))

(test test-valid-post-title-whitespace-only
  "空白のみの投稿タイトルが無効"
  (is (not (lisp-blog::valid-post-title-p "   "))))

(test test-valid-post-title-nil
  "nilの投稿タイトルが無効"
  (is (not (lisp-blog::valid-post-title-p nil))))

(test test-valid-post-content-correct
  "正常な投稿本文が有効"
  (is (lisp-blog::valid-post-content-p "This is a blog post content.")))

(test test-valid-post-content-min-length
  "最小長（1文字）の投稿本文が有効"
  (is (lisp-blog::valid-post-content-p "A")))

(test test-valid-post-content-max-length
  "最大長（100,000文字）の投稿本文が有効"
  (is (lisp-blog::valid-post-content-p (make-string 100000 :initial-element #\a))))

(test test-valid-post-content-too-long
  "100,000文字超過の投稿本文が無効"
  (is (not (lisp-blog::valid-post-content-p (make-string 100001 :initial-element #\a)))))

(test test-valid-post-content-empty
  "空文字列の投稿本文が無効"
  (is (not (lisp-blog::valid-post-content-p ""))))

(test test-valid-post-content-whitespace-only
  "空白のみの投稿本文が無効"
  (is (not (lisp-blog::valid-post-content-p "   "))))

(test test-valid-post-content-nil
  "nilの投稿本文が無効"
  (is (not (lisp-blog::valid-post-content-p nil))))

(test test-valid-display-name-correct
  "正常な表示名が有効"
  (is (lisp-blog::valid-display-name-p "John Doe")))

(test test-valid-display-name-min-length
  "最小長（1文字）の表示名が有効"
  (is (lisp-blog::valid-display-name-p "A")))

(test test-valid-display-name-max-length
  "最大長（100文字）の表示名が有効"
  (is (lisp-blog::valid-display-name-p (make-string 100 :initial-element #\a))))

(test test-valid-display-name-too-long
  "100文字超過の表示名が無効"
  (is (not (lisp-blog::valid-display-name-p (make-string 101 :initial-element #\a)))))

(test test-valid-display-name-empty
  "空文字列の表示名が無効"
  (is (not (lisp-blog::valid-display-name-p ""))))

(test test-valid-display-name-nil
  "nilの表示名が無効"
  (is (not (lisp-blog::valid-display-name-p nil))))

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

(test test-sanitize-html-img-tag
  "imgタグがエスケープされる"
  (is (string= "&lt;img src=x onerror=alert(1)&gt;"
               (lisp-blog::sanitize-html "<img src=x onerror=alert(1)>"))))

(test test-sanitize-html-ampersand
  "アンパサンドがエスケープされる"
  (is (string= "Tom &amp; Jerry"
               (lisp-blog::sanitize-html "Tom & Jerry"))))

(test test-sanitize-html-single-quote
  "シングルクォートがエスケープされる"
  (is (search "&#x27;" (lisp-blog::sanitize-html "It's fine"))))

(test test-sanitize-html-mixed
  "複数の特殊文字が同時にエスケープされる"
  (let ((result (lisp-blog::sanitize-html "<b>Hello & \"World\"</b>")))
    (is (search "&lt;" result))
    (is (search "&gt;" result))
    (is (search "&amp;" result))
    (is (search "&quot;" result))))

(test test-sanitize-html-empty
  "空文字列を渡すと空文字列が返る"
  (is (string= "" (lisp-blog::sanitize-html ""))))

(test test-sanitize-input-tabs
  "タブ文字が削除される"
  (is (string= "hello" (lisp-blog::sanitize-input "\thello\t"))))

(test test-sanitize-input-newlines
  "改行文字が削除される"
  (is (string= "hello" (lisp-blog::sanitize-input "\nhello\n"))))

(test test-sanitize-input-nil
  "nilを渡すとnilが返る"
  (is (null (lisp-blog::sanitize-input nil))))

;;; バリデーションエラー管理のテスト

(test test-validate-input-success
  "有効な値の検証は値をそのまま返す"
  (is (string= "test" (lisp-blog::validate-input :test "test" #'lisp-blog::valid-username-p "エラー"))))

(test test-validate-input-failure
  "無効な値の検証はvalidation-errorを発生させる"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-input :test "ab" #'lisp-blog::valid-username-p "短すぎます")))

(test test-validate-input-error-field
  "validation-errorのfieldフィールドが正しい"
  (handler-case
      (lisp-blog::validate-input :username "ab" #'lisp-blog::valid-username-p "短すぎます")
    (lisp-blog::validation-error (e)
      (is (eq :username (lisp-blog::validation-error-field e))))))

(test test-validate-input-error-message
  "validation-errorのmessageフィールドが正しい"
  (handler-case
      (lisp-blog::validate-input :username "ab" #'lisp-blog::valid-username-p "短すぎます")
    (lisp-blog::validation-error (e)
      (is (string= "短すぎます" (lisp-blog::validation-error-message e))))))

(test test-validate-user-input-all-valid
  "すべて有効なユーザー入力の検証は成功する"
  (is (lisp-blog::validate-user-input "testuser" "test@example.com" "password123")))

(test test-validate-user-input-invalid-username
  "無効なユーザー名でvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-user-input "ab" "test@example.com" "password123")))

(test test-validate-user-input-invalid-email
  "無効なメールアドレスでvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-user-input "testuser" "invalid-email" "password123")))

(test test-validate-user-input-invalid-password
  "無効なパスワードでvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-user-input "testuser" "test@example.com" "short")))

(test test-validate-user-input-optional-display-name
  "表示名を省略できる"
  (is (lisp-blog::validate-user-input "testuser" "test@example.com" "password123")))

(test test-validate-user-input-with-display-name
  "表示名を指定できる"
  (is (lisp-blog::validate-user-input "testuser" "test@example.com" "password123" "Test User")))

(test test-validate-user-input-invalid-display-name
  "無効な表示名でvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-user-input "testuser" "test@example.com" "password123" "")))

(test test-validate-post-input-valid
  "有効な投稿入力の検証は成功する"
  (is (lisp-blog::validate-post-input "Test Title" "Test content")))

(test test-validate-post-input-invalid-title
  "無効なタイトルでvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-post-input "" "Test content")))

(test test-validate-post-input-invalid-content
  "無効な本文でvalidation-errorが発生"
  (signals lisp-blog::validation-error
    (lisp-blog::validate-post-input "Test Title" "")))

;;; エラーレスポンス管理のテスト

(test test-make-error-response-basic
  "基本的なエラーレスポンスを生成"
  (let ((response (lisp-blog::make-error-response "エラーメッセージ")))
    (is (assoc "success" response :test #'string=))
    (is (assoc "message" response :test #'string=))
    (is (null (cdr (assoc "success" response :test #'string=))))
    (is (string= "エラーメッセージ" (cdr (assoc "message" response :test #'string=))))))

(test test-make-error-response-with-field
  "フィールド付きエラーレスポンスを生成"
  (let ((response (lisp-blog::make-error-response "エラー" :field :username)))
    (is (assoc "field" response :test #'string=))
    (is (string= "username" (cdr (assoc "field" response :test #'string=))))))

(test test-make-success-response-basic
  "基本的な成功レスポンスを生成"
  (let ((response (lisp-blog::make-success-response nil)))
    (is (assoc "success" response :test #'string=))
    (is (cdr (assoc "success" response :test #'string=)))))

(test test-make-success-response-with-message
  "メッセージ付き成功レスポンスを生成"
  (let ((response (lisp-blog::make-success-response nil "成功しました")))
    (is (string= "成功しました" (cdr (assoc "message" response :test #'string=))))))

(test test-make-success-response-with-data
  "データ付き成功レスポンスを生成"
  (let ((response (lisp-blog::make-success-response "test-data")))
    (is (assoc "data" response :test #'string=))
    (is (string= "test-data" (cdr (assoc "data" response :test #'string=))))))

;; Note: respond-json tests require HTTP context and are tested in handlers-test.lisp

;;; Note: データベース依存のセッション管理関数テストは Phase 3 で実装予定
;;; (create-session, get-session-user-id, delete-session, clean-expired-sessions)
;;;
;;; これらの関数は with-db マクロを使用しており、マクロ展開時に *db-spec* を
;;; 参照するため、テスト実行時の動的な設定変更では対応できません。
;;; Phase 3 では、データベース統合テスト用のインフラを整備して実装します。
