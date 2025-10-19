(in-package :cl-user)
(defpackage lisp-blog.util.conditions
  (:use :cl)
  (:export :lisp-blog-error
           :validation-error
           :authentication-error
           :authorization-error
           :resource-not-found-error
           :resource-conflict-error
           :business-logic-error
           :system-error
           ;; アクセサ
           :error-code
           :error-message
           :error-details
           :error-field
           :resource-type
           :resource-id))
(in-package :lisp-blog.util.conditions)

;;; 基底条件

(define-condition lisp-blog-error (error)
  ((code
    :initarg :code
    :reader error-code
    :documentation "エラーコード（キーワード）")
   (message
    :initarg :message
    :reader error-message
    :documentation "エラーメッセージ")
   (details
    :initarg :details
    :initform nil
    :reader error-details
    :documentation "追加の詳細情報（plist）"))
  (:documentation "lisp-blogシステムの基底エラー条件"))

(defmethod print-object ((condition lisp-blog-error) stream)
  "lisp-blog-errorの読みやすい出力形式"
  (print-unreadable-object (condition stream :type t)
    (format stream "~A: ~A"
            (error-code condition)
            (error-message condition))))

;;; バリデーションエラー

(define-condition validation-error (lisp-blog-error)
  ((field
    :initarg :field
    :initform nil
    :reader error-field
    :documentation "エラーが発生したフィールド名"))
  (:default-initargs :code :validation-error)
  (:documentation "バリデーションエラー

入力値の検証に失敗した場合に発生します。
例: 必須フィールドの欠落、フォーマット不正、値の範囲外など"))

;;; 認証エラー

(define-condition authentication-error (lisp-blog-error)
  ()
  (:default-initargs :code :authentication-error)
  (:documentation "認証エラー

ユーザーの認証に失敗した場合に発生します。
例: ログインが必要、セッション期限切れ、認証情報が不正など"))

;;; 認可エラー（権限不足）

(define-condition authorization-error (lisp-blog-error)
  ()
  (:default-initargs :code :authorization-error)
  (:documentation "認可エラー（権限不足）

ユーザーは認証されているが、操作に必要な権限がない場合に発生します。
例: 他人の投稿を編集しようとした、管理者専用機能へのアクセスなど"))

;;; リソース未検出エラー

(define-condition resource-not-found-error (lisp-blog-error)
  ((resource-type
    :initarg :resource-type
    :initform "resource"
    :reader resource-type
    :documentation "リソースの種類（例: 'user', 'post'）")
   (resource-id
    :initarg :resource-id
    :initform nil
    :reader resource-id
    :documentation "リソースのID"))
  (:default-initargs :code :resource-not-found)
  (:documentation "リソース未検出エラー

指定されたリソースが見つからない場合に発生します。
例: 存在しない投稿IDの指定、削除済みユーザーへのアクセスなど"))

;;; リソース競合エラー

(define-condition resource-conflict-error (lisp-blog-error)
  ((resource-type
    :initarg :resource-type
    :initform "resource"
    :reader resource-type
    :documentation "リソースの種類"))
  (:default-initargs :code :resource-conflict)
  (:documentation "リソース競合エラー

リソースの競合が発生した場合に発生します。
例: ユーザー名の重複、同時編集による競合など"))

;;; ビジネスロジックエラー

(define-condition business-logic-error (lisp-blog-error)
  ()
  (:documentation "ビジネスロジックエラー

ビジネスルールに違反した場合に発生します。
例: 既に公開済みの投稿を再度公開しようとした、
     下書き状態でない投稿を下書きに戻そうとしたなど"))

;;; システムエラー

(define-condition system-error (lisp-blog-error)
  ()
  (:default-initargs :code :system-error)
  (:documentation "システムエラー

予期しないシステムエラーが発生した場合に発生します。
例: データベース接続エラー、外部サービスエラーなど"))
