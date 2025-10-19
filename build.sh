#!/bin/bash

set -e  # エラーが発生したら即座に終了

echo "========================================"
echo " Frontend Build & Deploy Script"
echo "========================================"
echo ""

# ステップ1: 古い静的ファイルを削除
echo "[1/4] Cleaning old static files..."
if [ -d "static" ]; then
  rm -rf static/*
  echo "      ✓ Removed old files from static/"
else
  mkdir -p static
  echo "      ✓ Created static/ directory"
fi
echo ""

# ステップ2: フロントエンド依存関係のインストール確認
echo "[2/4] Checking frontend dependencies..."
cd frontend
if [ ! -d "node_modules" ]; then
  echo "      Installing dependencies..."
  npm install
else
  echo "      ✓ Dependencies already installed"
fi
echo ""

# ステップ3: フロントエンドビルド
echo "[3/4] Building frontend..."
npm run build
echo "      ✓ Frontend build completed"
echo ""

# ステップ4: ビルド成果物をstatic/にコピー
echo "[4/4] Copying build artifacts to static/..."
cd ..
cp -r frontend/dist/* static/
echo "      ✓ Files copied to static/"
echo ""

# 完了メッセージ
echo "========================================"
echo " ✓ Build completed successfully!"
echo "========================================"
echo "Static files are now in: $(pwd)/static/"
echo ""
echo "To start the server:"
echo "  sbcl --load start-server.lisp"
echo ""
echo "Then open your browser to:"
echo "  http://localhost:8080"
echo ""
