(defsystem "lisp-blog"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:hunchentoot
               :spinneret
               :postmodern
               :cl-ppcre
               :yason ;; This line is added by Gemini
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "database" :depends-on ("package"))
                 (:file "models" :depends-on ("package" "database"))
                 (:file "handlers" :depends-on ("package" "models"))
                 (:file "server" :depends-on ("package" "handlers")))))
  :description "A blog system built with Common Lisp")
