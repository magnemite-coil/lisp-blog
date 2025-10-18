# Common Lisp Blog System

Common Lisp (SBCL)、Caveman2、Mito、PostgreSQL、Redis、React 18、TypeScriptを使用したモダンなブログシステムです。

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Common Lisp](https://img.shields.io/badge/Common%20Lisp-SBCL-green.svg)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue.svg)
![React](https://img.shields.io/badge/React-18-blue.svg)
![TypeScript](https://img.shields.io/badge/TypeScript-5-blue.svg)

## 特徴

- 🔐 完全な認証システム（登録、ログイン、セッション管理）
- 📝 投稿のCRUD操作（作成、読み取り、更新、削除）
- 📄 下書き機能（公開/非公開の切り替え）
- ⚛️ React 18による最新のフロントエンド
- 🔷 TypeScriptによる型安全な開発
- 🎨 Tailwind CSSによるモダンなデザイン
- 🗄️ PostgreSQLによる堅牢なデータ管理
- 🚀 Redisによる高速セッション管理
- ✅ 307テストケース（100%成功率）
- ⚡ Common Lispによる高速なバックエンド

## 技術スタック

### バックエンド
- **Common Lisp**: SBCL 2.5.9+
- **Webフレームワーク**: Caveman2
- **ORM**: Mito（コネクションプーリング付き）
- **データベース**: PostgreSQL 15+
- **セッション**: Redis 7+
- **セキュリティ**: PBKDF2パスワードハッシュ化（100,000回反復）
- **テストフレームワーク**: FiveAM

### フロントエンド（Phase 5 - 実装予定）
- **フレームワーク**: React 18
- **言語**: TypeScript 5
- **ビルドツール**: Vite
- **ルーティング**: React Router v6
- **状態管理**: TanStack Query (React Query)
- **フォーム**: React Hook Form
- **スタイリング**: Tailwind CSS
- **マークダウン**: React Markdown

## 必要要件

- SBCL 2.5.9以上
- PostgreSQL 15以上
- Redis 7以上
- Node.js 18以上（フロントエンド開発時）
- Roswell（推奨）
- Quicklisp

---

## インストール手順

### macOS

#### 1. Homebrewのインストール（未インストールの場合）

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

#### 2. 必要なパッケージのインストール

```bash
# SBCLのインストール
brew install sbcl

# Roswellのインストール
brew install roswell

# PostgreSQLのインストール
brew install postgresql@15

# Redisのインストール
brew install redis

# PostgreSQLの起動
brew services start postgresql@15

# Redisの起動
brew services start redis

# Node.js のインストール（フロントエンド開発時）
brew install node
```

#### 3. Quicklispのインストール

```bash
ros install quicklisp
```

または手動でインストール：

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql:add-to-init-file)" \
     --quit
```

#### 4. PostgreSQLの設定

```bash
# 本番用データベースの作成
createdb blogdb

# テスト用データベースの作成
createdb lisp_blog_test

# ユーザーの作成（オプション）
createuser bloguser

# パスワード設定が必要な場合
psql blogdb
# psql内で実行:
# ALTER USER bloguser WITH PASSWORD 'your_password';
# GRANT ALL PRIVILEGES ON DATABASE blogdb TO bloguser;
# GRANT ALL PRIVILEGES ON DATABASE lisp_blog_test TO bloguser;
# \q
```

---

### Ubuntu

#### 1. システムパッケージの更新

```bash
sudo apt update && sudo apt upgrade -y
```

#### 2. 必要なパッケージのインストール

```bash
# SBCLのインストール
sudo apt install -y sbcl

# ビルドツールのインストール
sudo apt install -y curl build-essential automake autoconf git

# PostgreSQLのインストール
sudo apt install -y postgresql postgresql-contrib

# Redisのインストール
sudo apt install -y redis-server

# Node.jsのインストール（フロントエンド開発時）
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install -y nodejs

# PostgreSQLの起動と自動起動設定
sudo systemctl start postgresql
sudo systemctl enable postgresql

# Redisの起動と自動起動設定
sudo systemctl start redis-server
sudo systemctl enable redis-server
```

#### 3. Roswellのインストール

```bash
# Roswellのインストール
curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh

# パスの設定
echo 'export PATH="$HOME/.roswell/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Quicklispのインストール
ros install quicklisp
```

または手動でQuicklispをインストール：

```bash
cd ~
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql:add-to-init-file)" \
     --quit
```

#### 4. PostgreSQLの設定

```bash
# postgresユーザーに切り替え
sudo -i -u postgres

# データベースとユーザーの作成
createdb blogdb
createdb lisp_blog_test
createuser bloguser

# psqlで権限設定
psql
# psql内で実行:
# ALTER USER bloguser WITH SUPERUSER;
# \q

# 元のユーザーに戻る
exit
```

---

## プロジェクトのセットアップ

### 1. シンボリックリンクの作成

```bash
# Quicklispのlocal-projectsディレクトリにシンボリックリンクを作成
ln -s ~/projects/lisp-blog ~/quicklisp/local-projects/lisp-blog

# または Roswellを使用している場合
ln -s ~/projects/lisp-blog ~/.roswell/local-projects/lisp-blog
```

### 2. 依存関係のインストール

```bash
cd ~/projects/lisp-blog
sbcl --eval "(ql:quickload :lisp-blog)" --quit
```

---

## 実行方法

### 1. バックエンドサーバーの起動

```bash
cd ~/projects/lisp-blog
sbcl --load main.lisp
```

サーバーは `http://localhost:5000` で起動します。

### 2. フロントエンド開発サーバーの起動（Phase 5実装後）

別のターミナルで：

```bash
cd ~/projects/lisp-blog/frontend
npm install
npm run dev
```

開発サーバーは `http://localhost:5173` で起動します。

### 3. サーバーの停止

ターミナルで `Ctrl+C` を押すとサーバーが停止します。

---

## REPL内での実行（開発時）

開発中は、REPLを使って対話的に実行することもできます：

```bash
sbcl
```

REPL内で：

```lisp
(load "~/quicklisp/setup.lisp")
(ql:quickload :lisp-blog)
(in-package :lisp-blog)

;; データベース初期化（初回のみ）
(lisp-blog.db:ensure-tables-exist)

;; サーバー起動
(lisp-blog.main:start :port 5000)

;; サーバー停止
(lisp-blog.main:stop)
```

---

## テスト実行

### 全テストの実行

```bash
./run-tests.sh
```

### テストスイート詳細

- **Phase 4.1**: テスト環境セットアップ（2テスト）
- **Phase 4.2**: ユーティリティ関数テスト（106テスト）
- **Phase 4.3**: データモデルテスト（62テスト）
- **Phase 4.4**: ビジネスロジックテスト（91テスト）
- **Phase 4.5**: ハンドラーテスト（46テスト）

**合計**: 307テストケース（100%成功率）

---

## データベース操作

### データベースのリセット

```bash
# データベースを削除して再作成
dropdb blogdb
dropdb lisp_blog_test
createdb blogdb
createdb lisp_blog_test

# または、テーブルのみ削除
psql blogdb -c "DROP TABLE IF EXISTS sessions, posts, users CASCADE;"
```

その後、再度 `(lisp-blog.db:ensure-tables-exist)` を実行してください。

### データベースの確認

```bash
# PostgreSQLに接続
psql blogdb

# テーブル一覧を表示
\dt

# usersテーブルの内容を表示
SELECT * FROM users;

# postsテーブルの内容を表示
SELECT * FROM posts;

# 終了
\q
```

### Redisセッションの確認

```bash
# Redis CLIに接続
redis-cli

# 全キーの表示
KEYS *

# セッション詳細の確認
HGETALL session:<session-id>

# 終了
exit
```

---

## トラブルシューティング

### PostgreSQL接続エラー

```
Database error: could not connect to server
```

**解決方法:**

1. PostgreSQLが起動しているか確認：

```bash
# macOS
brew services list | grep postgresql

# Ubuntu
sudo systemctl status postgresql
```

2. データベースとユーザーが作成されているか確認：

```bash
psql -l  # データベース一覧
psql -U bloguser -d blogdb  # 接続テスト
```

3. 接続情報を確認：`src/config.lisp` の `*database*` を確認

### Redis接続エラー

```
Redis connection error
```

**解決方法:**

```bash
# macOS
brew services list | grep redis

# Ubuntu
sudo systemctl status redis-server

# Redisの再起動
# macOS
brew services restart redis

# Ubuntu
sudo systemctl restart redis-server
```

### ポート使用中エラー

```
Error: Address already in use
```

**解決方法:**

```bash
# 使用中のポートを確認
lsof -i :5000

# プロセスを終了
kill -9 <PID>
```

### Quicklispがシステムを見つけられない

```
Error: System "lisp-blog" not found
```

**解決方法:**

```lisp
;; REPLで実行
(ql:register-local-projects)
(ql:quickload :lisp-blog)
```

または、シンボリックリンクを再作成：

```bash
rm ~/quicklisp/local-projects/lisp-blog
ln -s ~/projects/lisp-blog ~/quicklisp/local-projects/lisp-blog
```

### テスト失敗時

```bash
# キャッシュをクリアして再実行
./clean-cache.sh -y
./run-tests.sh
```

---

## APIエンドポイント

### 認証API

#### POST /api/auth/register
ユーザー登録

**リクエスト:**
```json
{
  "username": "testuser",
  "password": "password123"
}
```

**レスポンス:**
```json
{
  "id": 1,
  "username": "testuser"
}
```

#### POST /api/auth/login
ログイン

**リクエスト:**
```json
{
  "username": "testuser",
  "password": "password123"
}
```

**レスポンス:**
```json
{
  "user": {
    "id": 1,
    "username": "testuser"
  }
}
```

#### POST /api/auth/logout
ログアウト

**レスポンス:**
```json
{
  "message": "Logged out successfully"
}
```

#### GET /api/auth/me
現在のユーザー情報取得

**レスポンス:**
```json
{
  "id": 1,
  "username": "testuser",
  "created_at": "2025-10-18T10:00:00Z"
}
```

### 投稿API（Phase 3実装予定）

#### POST /api/posts
投稿作成（認証必須）

#### GET /api/posts
投稿一覧取得

**クエリパラメータ:**
- `user_id`: 特定ユーザーの投稿のみ取得
- `published`: 公開済み投稿のみ（true/false）

#### GET /api/posts/:id
投稿詳細取得

#### PUT /api/posts/:id
投稿更新（認証必須、作成者のみ）

#### DELETE /api/posts/:id
投稿削除（認証必須、作成者のみ）

#### PUT /api/posts/:id/publish
下書きを公開（認証必須、作成者のみ）

#### PUT /api/posts/:id/unpublish
公開記事を下書きに戻す（認証必須、作成者のみ）

---

## プロジェクト構成

```
lisp-blog/
├── lisp-blog.asd          # システム定義（依存関係、コンパイル順序）
├── main.lisp              # エントリーポイント
├── setup-cache.lisp       # FASLキャッシュ設定
├── run-tests.sh           # テスト実行スクリプト
├── clean-cache.sh         # キャッシュクリーンアップ
├── src/
│   ├── config.lisp        # 設定（データベース、Redis接続情報）
│   ├── db.lisp            # DB接続管理（Mito + コネクションプール）
│   ├── web.lisp           # Caveman2ルーティング
│   ├── main.lisp          # サーバー起動・停止
│   ├── util/
│   │   ├── crypto.lisp    # パスワードハッシュ化、バリデーション
│   │   └── json.lisp      # JSONレスポンスヘルパー
│   ├── model/
│   │   ├── user.lisp      # Userモデル（CRUD操作）
│   │   └── post.lisp      # Postモデル（CRUD操作、下書き機能）
│   ├── middleware/
│   │   └── session.lisp   # Redisセッション管理
│   ├── service/
│   │   ├── auth.lisp      # 認証ビジネスロジック
│   │   └── post.lisp      # 投稿ビジネスロジック
│   └── handler/
│       ├── auth.lisp      # 認証APIハンドラー
│       └── post.lisp      # 投稿APIハンドラー
├── test/
│   ├── setup.lisp         # テスト環境セットアップ
│   ├── fixtures.lisp      # テストデータフィクスチャ
│   ├── util/              # ユーティリティテスト
│   ├── model/             # モデルテスト
│   ├── middleware/        # ミドルウェアテスト
│   ├── service/           # サービステスト
│   └── handler/           # ハンドラーテスト
├── docs/
│   └── design/
│       ├── phase1-mito-migration.md    # Phase 1設計書
│       ├── phase2-auth-api.md          # Phase 2設計書
│       ├── phase4-fiveam-tests.md      # Phase 4設計書
│       └── phase5-react-frontend.md    # Phase 5設計書
└── frontend/              # React 18 + TypeScript（Phase 5実装予定）
    ├── src/
    │   ├── api/           # APIクライアント
    │   ├── components/    # Reactコンポーネント
    │   ├── pages/         # ページコンポーネント
    │   ├── hooks/         # カスタムフック
    │   ├── types/         # TypeScript型定義
    │   └── utils/         # ユーティリティ関数
    ├── public/
    └── index.html
```

---

## 開発フロー

### ブランチ戦略

Conventional Commits 1.0.0 に従います。

**ブランチ命名規則:**
```
<type>/<short-description>

例:
feat/password-reset
fix/session-timeout
docs/api-documentation
```

**コミットメッセージ形式:**
```
<type>[optional scope]: <description>

例:
feat(auth): add password reset functionality
fix(post): validate post title length
docs: update installation guide
```

### 開発サイクル

1. **機能追加前**: `docs/design/` に詳細な計画を記載
2. **作業中**: 段階的に実装（小さなステップに分割）
3. **作業完了後**:
   - `PROGRESS.md` に詳細な記録（非公開）
   - `CHANGELOG.md` に公開用リリースノート

---

## カスタマイズ

### ポート番号の変更

`main.lisp` を編集：

```lisp
(lisp-blog.main:start :port 3000)  ;; 任意のポート番号
```

### セッション有効期限の変更

`src/middleware/session.lisp` を編集：

```lisp
(defparameter *session-ttl* (* 7 24 60 60))  ;; 7日間（秒単位）
```

### 機能拡張候補

- コメント機能
- 画像アップロード
- タグ機能
- 検索・フィルタリング機能
- ページネーション
- ユーザープロフィール編集
- パスワードリセット
- メール通知

---

## セキュリティ

### 実装済み対策

- ✅ **SQLインジェクション対策**: パラメータ化クエリ使用
- ✅ **パスワード保護**: PBKDF2ハッシュ化（100,000回反復）
- ✅ **XSS対策**: 入力サニタイゼーション
- ✅ **セッション管理**: Redis + HttpOnly Cookie
- ✅ **入力バリデーション**: バックエンドで厳密なチェック

### 本番環境向け追加対策（未実装）

- ⚠️ HTTPS の使用（Let's Encrypt等）
- ⚠️ CSRF対策（トークン検証）
- ⚠️ レート制限（ブルートフォース攻撃対策）
- ⚠️ CORS設定の厳密化
- ⚠️ ロギングと監視
- ⚠️ データベースバックアップ

---

## ライセンス

MIT License

---

## 開発者向け情報

### キャッシュ管理

このプロジェクトでは、コンパイル済みファイル（.fasl）をプロジェクト内の `.fasl-cache/` ディレクトリに保存します。

**キャッシュの場所:**
```
lisp-blog/.fasl-cache/sbcl-<version>-<platform>/
```

**キャッシュのクリーンアップ:**
```bash
# 対話モード（確認あり）
./clean-cache.sh

# 自動モード（確認なし）
./clean-cache.sh -y
```

---

## プロジェクト進捗

### 完了フェーズ

- ✅ **Phase 1**: Mito + Caveman2 マイグレーション
- ✅ **Phase 2**: 認証API実装
- ✅ **Phase 4**: FiveAMテスト体制確立（307テスト）

### 進行中フェーズ

- 🔄 **Phase 5**: React 18 + TypeScript フロントエンド実装

### 未実装フェーズ

- ⏳ **Phase 3**: 投稿API実装（CRUD + 下書き機能）

詳細は `PROGRESS.md` をご覧ください。

---

## 参考リンク

### Common Lisp
- [SBCL Documentation](http://www.sbcl.org/manual/)
- [Caveman2](https://github.com/fukamachi/caveman)
- [Mito](https://github.com/fukamachi/mito)
- [FiveAM](https://common-lisp.net/project/fiveam/)

### Frontend
- [React 18 Documentation](https://react.dev/)
- [TypeScript Documentation](https://www.typescriptlang.org/)
- [Vite Documentation](https://vitejs.dev/)
- [TanStack Query](https://tanstack.com/query/latest)
- [React Router](https://reactrouter.com/)
- [Tailwind CSS Documentation](https://tailwindcss.com/)

### Database & Cache
- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [Redis Documentation](https://redis.io/documentation)
