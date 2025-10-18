#!/bin/bash
set -e

echo "=== lisp-blog Test Suite ==="
echo ""

# テスト用データベースが存在しない場合は作成
if ! psql -U bloguser -lqt | cut -d \| -f 1 | grep -qw lisp_blog_test; then
  echo "Creating test database..."
  createdb -U bloguser lisp_blog_test
fi

# Redisが起動しているか確認
if ! redis-cli ping > /dev/null 2>&1; then
  echo "Error: Redis is not running"
  echo "Please start Redis server: redis-server"
  exit 1
fi

# テスト実行
echo "Running tests..."
sbcl --noinform --non-interactive \
  --eval "(ql:quickload :lisp-blog/tests :silent t)" \
  --eval "(lisp-blog-test:setup-test-environment)" \
  --eval "(lisp-blog-test:run-tests)" \
  --eval "(lisp-blog-test:teardown-test-environment)"

echo ""
echo "=== Tests completed ==="
