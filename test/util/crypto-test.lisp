(in-package :lisp-blog-test)

;;; 暗号化関数のテストスイート

(def-suite crypto-tests
  :in lisp-blog-test-suite
  :description "暗号化関数のテスト")

(in-suite crypto-tests)

;;; パスワードハッシュ化のテスト

(test password-hashing
  "パスワードハッシュ化のテスト"
  (let* ((password "password123")
         (hash (lisp-blog.util.crypto:hash-password password)))
    ;; ハッシュが生成される
    (is (stringp hash))
    (is (> (length hash) 0))
    ;; 同じパスワードでも異なるハッシュ（ソルトのため）
    (let ((hash2 (lisp-blog.util.crypto:hash-password password)))
      (is (not (string= hash hash2))))))

(test password-verification-success
  "パスワード検証の成功ケース"
  (let* ((password "password123")
         (hash (lisp-blog.util.crypto:hash-password password)))
    (is (lisp-blog.util.crypto:verify-password password hash))))

(test password-verification-failure
  "パスワード検証の失敗ケース"
  (let* ((password "password123")
         (hash (lisp-blog.util.crypto:hash-password password)))
    ;; 間違ったパスワード
    (is (not (lisp-blog.util.crypto:verify-password "wrongpassword" hash)))
    ;; 大文字小文字が違う
    (is (not (lisp-blog.util.crypto:verify-password "PASSWORD123" hash)))
    ;; 空文字列
    (is (not (lisp-blog.util.crypto:verify-password "" hash)))))

(test password-hash-format
  "パスワードハッシュのフォーマット確認"
  (let ((hash (lisp-blog.util.crypto:hash-password "test")))
    ;; Base64エンコードされたソルトとハッシュが$で区切られている
    (is (find #\$ hash))
    (let ((parts (cl-ppcre:split "\\$" hash)))
      (is (= 2 (length parts)))
      ;; 各パートがBase64文字列（長さがある）
      (is (every (lambda (part) (> (length part) 0)) parts)))))

(test password-hash-different-salts
  "異なるソルトで異なるハッシュが生成される"
  (let ((password "samepassword"))
    (let ((hash1 (lisp-blog.util.crypto:hash-password password))
          (hash2 (lisp-blog.util.crypto:hash-password password))
          (hash3 (lisp-blog.util.crypto:hash-password password)))
      ;; 全て異なるハッシュ
      (is (not (string= hash1 hash2)))
      (is (not (string= hash2 hash3)))
      (is (not (string= hash1 hash3)))
      ;; でも全て検証は成功
      (is (lisp-blog.util.crypto:verify-password password hash1))
      (is (lisp-blog.util.crypto:verify-password password hash2))
      (is (lisp-blog.util.crypto:verify-password password hash3)))))

(test password-empty-string
  "空文字列のパスワードハッシュ化"
  (let ((hash (lisp-blog.util.crypto:hash-password "")))
    (is (stringp hash))
    (is (> (length hash) 0))
    (is (lisp-blog.util.crypto:verify-password "" hash))))

(test password-long-string
  "長いパスワードのハッシュ化"
  (let* ((long-password (make-string 1000 :initial-element #\a))
         (hash (lisp-blog.util.crypto:hash-password long-password)))
    (is (stringp hash))
    (is (lisp-blog.util.crypto:verify-password long-password hash))))

(test password-special-characters
  "特殊文字を含むパスワード"
  (let* ((password "p@ssw0rd!#$%^&*()")
         (hash (lisp-blog.util.crypto:hash-password password)))
    (is (lisp-blog.util.crypto:verify-password password hash))
    (is (not (lisp-blog.util.crypto:verify-password "p@ssw0rd" hash)))))

(test password-unicode-characters
  "Unicode文字を含むパスワード"
  (let* ((password "パスワード123")
         (hash (lisp-blog.util.crypto:hash-password password)))
    (is (lisp-blog.util.crypto:verify-password password hash))
    (is (not (lisp-blog.util.crypto:verify-password "パスワード" hash)))))
