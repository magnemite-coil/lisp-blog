(in-package :cl-user)
(defpackage lisp-blog.util.crypto
  (:use :cl)
  (:export :hash-password
           :verify-password))
(in-package :lisp-blog.util.crypto)

(defun hash-password (password)
  "パスワードをPBKDF2でハッシュ化（SHA256, 100,000回反復）"
  (let* ((salt (ironclad:make-random-salt))
         (digest (ironclad:pbkdf2-hash-password
                  (babel:string-to-octets password)
                  :salt salt
                  :digest :sha256
                  :iterations 100000)))
    (concatenate 'string
                 (cl-base64:usb8-array-to-base64-string salt)
                 "$"
                 (cl-base64:usb8-array-to-base64-string digest))))

(defun verify-password (password hash)
  "パスワードとハッシュを照合"
  (let* ((parts (cl-ppcre:split "\\$" hash))
         (salt (cl-base64:base64-string-to-usb8-array (first parts)))
         (stored-hash (cl-base64:base64-string-to-usb8-array (second parts)))
         (computed-hash (ironclad:pbkdf2-hash-password
                         (babel:string-to-octets password)
                         :salt salt
                         :digest :sha256
                         :iterations 100000)))
    (equalp stored-hash computed-hash)))
