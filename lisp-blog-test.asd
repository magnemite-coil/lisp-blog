(defsystem "lisp-blog-test"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:lisp-blog :fiveam)
  :components ((:module "test"
                :components
                ((:file "setup")
                 (:file "fixtures" :depends-on ("setup"))
                 (:module "util"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "crypto-test")
                   (:file "json-test")
                   (:file "conditions-test")
                   (:file "error-test")))
                 (:module "model"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "user-test")
                   (:file "post-test")))
                 (:module "middleware"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "session-test")))
                 (:module "service"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "auth-test")
                   (:file "post-test")
                   (:file "post-error-test")))
                 (:module "handler"
                  :depends-on ("setup" "fixtures")
                  :components
                  ((:file "auth-test")
                   (:file "post-test")
                   (:file "error-integration-test"))))))
  :description "Test system for lisp-blog"
  :perform (test-op (op c) (symbol-call :lisp-blog-test :run-tests)))
