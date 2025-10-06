(in-package :lisp-blog-test)

(defun run-tests ()
  "すべてのテストスイートを実行"
  (format t "~%=== Running lisp-blog Test Suite ===~%~%")
  (let ((results (run 'utils-suite)))
    (format t "~%=== Test Results ===~%")
    (explain! results)
    results))

(defun run-utils-tests ()
  "utils.lisp のテストのみ実行"
  (run 'utils-suite))
