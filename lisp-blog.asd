(defsystem "lisp-blog"
  :version "0.3.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:caveman2              ; Webフレームワーク
               :clack                  ; ミドルウェア層
               :lack                   ; ミドルウェアコンポーネント
               :lack-middleware-session ; セッション管理
               :woo                    ; HTTPサーバー
               :mito                   ; ORM
               :sxql                   ; SQLビルダー
               :ironclad               ; パスワードハッシュ化
               :cl-base64              ; Base64エンコード
               :jonathan               ; JSON（高速）
               :local-time             ; タイムスタンプ
               :cl-ppcre               ; 正規表現
               :babel                  ; 文字列エンコーディング
               :cl-redis)              ; Redis クライアント
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "db" :depends-on ("config"))
                 (:module "util"
                  :components
                  ((:file "crypto")
                   (:file "json")))
                 (:module "model"
                  :depends-on ("db")
                  :components
                  ((:file "user")
                   (:file "post")))
                 (:module "middleware"
                  :components
                  ((:file "session")))
                 (:module "service"
                  :depends-on ("model" "util" "middleware")
                  :components
                  ((:file "auth")
                   (:file "post")))
                 (:module "handler"
                  :depends-on ("service" "util" "model" "middleware")
                  :components
                  ((:file "auth")
                   (:file "post")))
                 (:file "web" :depends-on ("db" "model" "middleware" "service" "handler"))
                 (:file "main" :depends-on ("config" "db" "web")))))
  :description "A blog system with Caveman2 + Mito"
  :in-order-to ((test-op (test-op "lisp-blog/tests"))))

(defsystem "lisp-blog/tests"
  :author "Your Name"
  :license "MIT"
  :depends-on ("lisp-blog"
               "fiveam")
  :components ((:module "test"
                :components
                ((:file "setup")
                 (:file "fixtures" :depends-on ("setup"))
                 (:module "util"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "crypto-test")
                   (:file "json-test")))
                 ;; Phase 4.3以降で実装予定
                 ;; (:module "model"
                 ;;  :depends-on ("setup" "fixtures")
                 ;;  :components
                 ;;  ((:file "user-test")
                 ;;   (:file "post-test")))
                 ;; (:module "service"
                 ;;  :depends-on ("setup" "fixtures")
                 ;;  :components
                 ;;  ((:file "auth-test")
                 ;;   (:file "post-test")))
                 )))
  :description "Test system for lisp-blog")
