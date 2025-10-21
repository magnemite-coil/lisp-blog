#!/bin/bash
#
# run-tests-coverage.sh - SBCLのsb-coverを使ったカバレッジテスト実行スクリプト
#
# 使い方:
#   ./run-tests-coverage.sh          # カバレッジ付きテスト実行
#   ./run-tests-coverage.sh -v       # 詳細モード（全出力を表示）
#
# 注意:
#   sb-coverはパフォーマンスオーバーヘッドが大きいため、
#   テスト実行時間が通常の2-3倍になります。
#

set -e

VERBOSE=0
COVERAGE_DIR="coverage-report"

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

# カバレッジレポートディレクトリの準備
if [ -d "$COVERAGE_DIR" ]; then
  echo "Removing old coverage report..."
  rm -rf "$COVERAGE_DIR"
fi
mkdir -p "$COVERAGE_DIR"

# 一時ファイル作成
TEST_SCRIPT=$(mktemp /tmp/run-lisp-blog-coverage.XXXXXX.lisp)

cat > "$TEST_SCRIPT" << 'EOF'
;; sb-coverを有効化
(require :sb-cover)

;; カバレッジ収集を有効にして再コンパイル
(format t "~%=== Enabling coverage instrumentation ===~%")
(declaim (optimize sb-cover:store-coverage-data))

;; システムをリロード（カバレッジ付き）
(format t "~%=== Loading system with coverage instrumentation ===~%")
(asdf:load-system :lisp-blog :force t)
(ql:quickload :lisp-blog/tests :silent t)

;; 最適化設定を元に戻す
(declaim (optimize (sb-cover:store-coverage-data 0)))

(in-package :lisp-blog-test)

;; テスト環境セットアップ
(format t "~%=== Setting up test environment ===~%")
(setup-test-environment)

;; 全テスト実行
(format t "~%=== Running all tests with coverage ===~%")
(format t "~%Note: This may take 2-3x longer than normal due to coverage overhead~%")
(run-tests)

;; クリーンアップ
(format t "~%=== Cleanup ===~%")
(teardown-test-environment)

;; カバレッジレポート生成
(format t "~%=== Generating coverage report ===~%")
(sb-cover:report "coverage-report/")
(format t "~%Coverage report generated in: coverage-report/~%")
(format t "Open coverage-report/cover-index.html in a browser to view the report~%")

(sb-ext:exit :code 0)
EOF

echo "Starting lisp-blog test suite with coverage..."
echo "This will take longer than normal tests (2-3x) due to coverage overhead."
echo ""

if [ $VERBOSE -eq 1 ]; then
  # 詳細モード: 全出力を表示
  sbcl --noinform --load "$TEST_SCRIPT"
  EXIT_CODE=$?
else
  # 通常モード: 出力を表示
  sbcl --noinform --load "$TEST_SCRIPT" 2>&1
  EXIT_CODE=$?
fi

# 一時ファイル削除
rm -f "$TEST_SCRIPT"

echo ""
echo "=================================================="
echo "Coverage report available at:"
echo "  file://$(pwd)/$COVERAGE_DIR/cover-index.html"
echo "=================================================="

exit $EXIT_CODE
