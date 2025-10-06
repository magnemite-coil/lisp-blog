;;;; i18n.lisp - Internationalization support
;;;; Simple hash-table based translation system

(in-package :lisp-blog)

;;; グローバル変数

(defparameter *current-locale* :ja
  "現在のロケール（デフォルト: 日本語）")

(defparameter *translations* (make-hash-table :test 'equal)
  "全翻訳を格納するハッシュテーブル
   キー形式: 'locale.translation-key' (例: 'ja.login.title')")

(defparameter *supported-locales* '(:ja :en)
  "サポートする言語のリスト")

;;; 翻訳関数

(defun t! (key &optional (locale *current-locale*))
  "翻訳を取得する
   引数:
     key - 翻訳キー (例: 'login.title')
     locale - ロケール（省略時は *current-locale* を使用）
   戻り値:
     翻訳されたテキスト（見つからない場合はキーをそのまま返す）"
  (let ((full-key (format nil "~A.~A" locale key)))
    (gethash full-key *translations* key)))

(defun set-translation (locale key value)
  "翻訳を設定する
   引数:
     locale - ロケール (:ja, :en等)
     key - 翻訳キー
     value - 翻訳テキスト"
  (let ((full-key (format nil "~A.~A" locale key)))
    (setf (gethash full-key *translations*) value)))

(defun load-translations ()
  "全ての翻訳ファイルをロード"
  (dolist (locale *supported-locales*)
    (let* ((filename (format nil "src/locales/~A.lisp" locale))
           (file (merge-pathnames filename *project-root*)))
      (when (probe-file file)
        (load file)
        (format t "Loaded translations: ~A~%" locale)))))

(defun get-locale-from-cookie ()
  "クッキーからロケールを取得"
  (let ((locale-str (hunchentoot:cookie-in "locale")))
    (when locale-str
      (let ((locale (intern (string-upcase locale-str) :keyword)))
        (when (member locale *supported-locales*)
          locale)))))

(defun set-current-locale-from-request ()
  "リクエストから現在のロケールを設定
   優先順位: クッキー > デフォルト"
  (let ((locale (or (get-locale-from-cookie)
                    *current-locale*)))
    (when (member locale *supported-locales*)
      (setf *current-locale* locale))))
