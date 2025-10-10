;;;; setup-cache.lisp
;;;;
;;;; プロジェクト専用のFASLキャッシュ設定
;;;; このファイルをロードすると、コンパイル済みファイル(.fasl)が
;;;; プロジェクトディレクトリ内の .fasl-cache/ に保存されます。
;;;;
;;;; 使い方:
;;;;   sbcl --load setup-cache.lisp --eval "(ql:quickload :lisp-blog)"

(in-package :cl-user)

(defun setup-project-cache ()
  "プロジェクト内のキャッシュディレクトリを設定"
  (let* ((project-root (uiop:getcwd))
         (cache-dir (merge-pathnames ".fasl-cache/" project-root)))

    ;; キャッシュディレクトリが存在しない場合は作成
    (ensure-directories-exist cache-dir)

    ;; ASDFの出力先を設定
    (asdf:initialize-output-translations
     `(:output-translations
       ;; プロジェクト内のファイルは .fasl-cache/ に出力
       (,(namestring project-root)
        (,(namestring cache-dir) :implementation))
       ;; Quicklisp等の外部ライブラリはデフォルト位置
       :inherit-configuration))

    (format t "~%✅ FASL cache directory configured: ~A~%" cache-dir)
    (format t "   All compiled files will be stored in this directory.~%~%")))

;; 自動的に設定を適用
(setup-project-cache)
