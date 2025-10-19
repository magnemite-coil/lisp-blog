#!/bin/bash
#
# run-tests.sh - テスト実行スクリプト
#
# 使い方:
#   ./run-tests.sh          # 全テストを実行
#   ./run-tests.sh -v       # 詳細モード（全出力を表示）
#

set -e

VERBOSE=0

# オプション解析
while getopts "v" opt; do
  case $opt in
    v)
      VERBOSE=1
      ;;
    \?)
      echo "Usage: $0 [-v]" >&2
      exit 1
      ;;
  esac
done

# 一時ファイル作成
TEST_SCRIPT=$(mktemp /tmp/run-lisp-blog-tests.XXXXXX.lisp)

cat > "$TEST_SCRIPT" << 'EOF'
(ql:quickload :lisp-blog/tests :silent t)
(in-package :lisp-blog-test)

;; テスト環境セットアップ
(format t "~%=== Setting up test environment ===~%")
(setup-test-environment)

;; 全テスト実行
(format t "~%=== Running all tests ===~%")
(run-tests)

;; クリーンアップ
(format t "~%=== Cleanup ===~%")
(teardown-test-environment)

(sb-ext:exit :code 0)
EOF

echo "Starting lisp-blog test suite..."
echo ""

if [ $VERBOSE -eq 1 ]; then
  # 詳細モード: 全出力を表示
  sbcl --noinform --load "$TEST_SCRIPT"
  EXIT_CODE=$?
else
  # 通常モード: 最後の50行のみ表示
  sbcl --noinform --load "$TEST_SCRIPT" 2>&1 | tail -50
  EXIT_CODE=$?
fi

# 一時ファイル削除
rm -f "$TEST_SCRIPT"

exit $EXIT_CODE
