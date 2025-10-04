(defsystem "lisp-blog"
  :version "0.2.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:hunchentoot
               :spinneret
               :postmodern
               :yason
               :cl-ppcre
               :local-time
               :ironclad          ; パスワードハッシュ化
               :cl-base64         ; Base64エンコード
               :uuid)             ; セッションID生成
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "database" :depends-on ("package"))
                 (:file "models" :depends-on ("package" "database" "utils"))
                 (:file "auth" :depends-on ("package" "database" "models" "utils"))
                 (:file "handlers" :depends-on ("package" "models" "auth"))
                 (:file "server" :depends-on ("package" "handlers")))))
  :description "A blog system with user authentication")
