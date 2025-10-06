(defsystem "lisp-blog-test"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:lisp-blog :fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "setup" :depends-on ("package"))
                 (:file "utils-test" :depends-on ("setup"))
                 (:file "run-tests" :depends-on ("utils-test")))))
  :description "Test system for lisp-blog"
  :perform (test-op (op c) (symbol-call :lisp-blog-test :run-tests)))
