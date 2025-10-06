(in-package :lisp-blog-test)

(defun run-tests ()
  "すべてのテストスイートを実行"
  ;; テスト用データベース設定に切り替え（グローバルに設定）
  (format t "~%=== Configuring test environment ===~%")
  (setf lisp-blog::*db-spec* *test-db-spec*)
  (format t "Test DB: ~A~%"  (first lisp-blog::*db-spec*))

  (format t "~%=== Setting up test database ===~%")
  (setup-test-db)

  (format t "~%=== Running lisp-blog Test Suite ===~%~%")
  (let ((results (run 'utils-suite)))
    (format t "~%=== Test Results ===~%")
    (explain! results)
    results))

(defun run-utils-tests ()
  "utils.lisp のテストのみ実行"
  (run 'utils-suite))
