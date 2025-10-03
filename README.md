# Common Lisp Blog System

Common Lisp (SBCL)、Hunchentoot、Spinneret、PostgreSQL、Postmodern、Vue 3、Tailwind CSS、Masonryを使用したモダンなブログシステムです。

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Common Lisp](https://img.shields.io/badge/Common%20Lisp-SBCL-green.svg)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue.svg)

## 特徴

- 📝 投稿の作成、表示、削除機能
- 🎨 Vue 3によるリアクティブなUI
- 💅 Tailwind CSSによるモダンなデザイン
- 🧱 Masonryレイアウトによる美しいグリッド表示
- 🗄️ PostgreSQLによる堅牢なデータ管理
- ⚡ Common Lispによる高速なバックエンド

## 技術スタック

### バックエンド
- **Common Lisp**: SBCL 2.5.9+
- **Webサーバー**: Hunchentoot
- **HTML生成**: Spinneret
- **データベース**: PostgreSQL 15+
- **DB接続**: Postmodern

### フロントエンド
- **フレームワーク**: Vue 3
- **スタイリング**: Tailwind CSS
- **レイアウト**: Masonry

## 必要要件

- SBCL 2.5.9以上
- PostgreSQL 15以上
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

# PostgreSQLの起動
brew services start postgresql@15
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
# データベースの作成
createdb blogdb

# ユーザーの作成（オプション）
createuser bloguser

# パスワード設定が必要な場合
psql blogdb
# psql内で実行:
# ALTER USER bloguser WITH PASSWORD 'your_password';
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

# PostgreSQLの起動と自動起動設定
sudo systemctl start postgresql
sudo systemctl enable postgresql
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

### 1. プロジェクトのクローン/作成

```bash
# プロジェクトディレクトリの作成
mkdir -p ~/projects/lisp-blog
cd ~/projects/lisp-blog
```

### 2. ディレクトリ構造の作成

```bash
mkdir -p src static/css static/js
```

プロジェクト構造：

```
lisp-blog/
├── README.md
├── lisp-blog.asd
├── main.lisp
├── src/
│   ├── package.lisp
│   ├── database.lisp
│   ├── models.lisp
│   ├── handlers.lisp
│   └── server.lisp
└── static/
    ├── css/
    │   └── style.css
    └── js/
        └── app.js
```

### 3. ファイルの作成

各ファイルを以下の内容で作成してください。

#### lisp-blog.asd

```lisp
(defsystem "lisp-blog"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:hunchentoot
               :spinneret
               :postmodern
               :yason
               :cl-ppcre
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "database" :depends-on ("package"))
                 (:file "models" :depends-on ("package" "database"))
                 (:file "handlers" :depends-on ("package" "models"))
                 (:file "server" :depends-on ("package" "handlers")))))
  :description "A blog system built with Common Lisp")
```

#### src/package.lisp

```lisp
(defpackage :lisp-blog
  (:use :cl :hunchentoot :spinneret :postmodern)
  (:export #:start-server
           #:stop-server
           #:init-db))
```

#### src/database.lisp

```lisp
(in-package :lisp-blog)

(defparameter *db-spec* 
  '("blogdb" "bloguser" "" "localhost")
  "データベース接続仕様")

(defmacro with-db (&body body)
  "データベース接続を自動管理"
  `(with-connection *db-spec*
     ,@body))

(defun init-db ()
  "データベーステーブルの初期化"
  (with-db
    (execute "CREATE TABLE IF NOT EXISTS posts (
                id SERIAL PRIMARY KEY,
                title VARCHAR(255) NOT NULL,
                content TEXT NOT NULL,
                author VARCHAR(100) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    (execute "CREATE TABLE IF NOT EXISTS comments (
                id SERIAL PRIMARY KEY,
                post_id INTEGER REFERENCES posts(id) ON DELETE CASCADE,
                author VARCHAR(100) NOT NULL,
                content TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
              )")
    (format t "Database initialized successfully.~%")))
```

その他のファイル（models.lisp、handlers.lisp、server.lisp、main.lisp、static/css/style.css、static/js/app.js）は、前述の手順書を参照してください。

### 4. シンボリックリンクの作成

```bash
# Quicklispのlocal-projectsディレクトリにシンボリックリンクを作成
ln -s ~/projects/lisp-blog ~/quicklisp/local-projects/lisp-blog

# または Roswellを使用している場合
ln -s ~/projects/lisp-blog ~/.roswell/local-projects/lisp-blog
```

### 5. データベース設定の確認

`src/database.lisp`でデータベース接続情報を確認・修正してください：

```lisp
(defparameter *db-spec* 
  '("blogdb" "bloguser" "" "localhost")
  ;; ("データベース名" "ユーザー名" "パスワード" "ホスト")
```

パスワードを設定している場合は、3番目のパラメータにパスワードを指定してください。

---

## 実行方法

### 1. 依存関係のインストールとサーバー起動

```bash
cd ~/projects/lisp-blog
sbcl --load main.lisp
```

初回実行時、Quicklispが必要なライブラリを自動的にダウンロードします。

### 2. ブラウザでアクセス

サーバーが起動したら、ブラウザで以下のURLにアクセスしてください：

```
http://localhost:8080
```

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
(init-db)

;; サーバー起動
(start-server :port 8080)

;; サーバー停止
(stop-server)
```

---

## データベース操作

### データベースのリセット

```bash
# データベースを削除して再作成
dropdb blogdb
createdb blogdb

# または、テーブルのみ削除
psql blogdb -c "DROP TABLE IF EXISTS comments, posts CASCADE;"
```

その後、再度 `(init-db)` を実行してください。

### データベースの確認

```bash
# PostgreSQLに接続
psql blogdb

# テーブル一覧を表示
\dt

# postsテーブルの内容を表示
SELECT * FROM posts;

# 終了
\q
```

---

## トラブルシューティング

### PostgreSQL接続エラー

```
Error: No database connection selected.
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

3. 接続情報を確認：`src/database.lisp` の `*db-spec*` を確認

### ポート使用中エラー

```
Error: Address already in use
```

**解決方法:**

```bash
# 使用中のポートを確認
lsof -i :8080

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

### yasonライブラリが見つからない

```
Error: Component "yason" not found
```

**解決方法:**

```lisp
(ql:quickload :yason)
```

---

## Vue 3、Tailwind CSS、Masonryの使い方

### Vue 3

Vue 3は、リアクティブなユーザーインターフェースを構築するためのJavaScriptフレームワークです。

**基本的な使い方:**

```javascript
const { createApp } = Vue;

createApp({
  data() {
    return {
      message: 'Hello Vue!'  // リアクティブなデータ
    }
  },
  methods: {
    updateMessage() {
      this.message = 'Updated!'  // データを更新すると自動的にUIも更新
    }
  }
}).mount('#app');
```

**主要な機能:**

- `v-for`: リストのレンダリング
- `v-model`: 双方向データバインディング
- `@click`: イベントハンドリング
- `{{ }}`: データの表示

### Tailwind CSS

Tailwind CSSは、ユーティリティファーストのCSSフレームワークです。

**よく使うクラス:**

```html
<!-- レイアウト -->
<div class="container mx-auto">中央寄せコンテナ</div>
<div class="flex justify-between">フレックスボックス</div>
<div class="grid grid-cols-3">グリッドレイアウト</div>

<!-- スペーシング -->
<div class="p-6">パディング（全方向）</div>
<div class="mt-4 mb-2">マージン（上下）</div>

<!-- 色とスタイル -->
<div class="bg-blue-600 text-white">背景と文字色</div>
<div class="rounded-lg shadow-md">角丸と影</div>

<!-- レスポンシブ -->
<div class="w-full md:w-1/2 lg:w-1/3">レスポンシブ幅</div>
```

### Masonry

Masonryは、Pinterest風のグリッドレイアウトを作成します。

**CSS Columnsによる実装:**

```css
.masonry-grid {
  column-count: 3;      /* 3列 */
  column-gap: 1.5rem;   /* 列間のスペース */
}

.masonry-item {
  break-inside: avoid;  /* アイテムが列をまたがない */
  margin-bottom: 1.5rem;
}

/* レスポンシブ対応 */
@media (max-width: 1024px) {
  .masonry-grid {
    column-count: 2;  /* タブレット: 2列 */
  }
}

@media (max-width: 640px) {
  .masonry-grid {
    column-count: 1;  /* モバイル: 1列 */
  }
}
```

---

## APIエンドポイント

### GET /api/posts

全ての投稿を取得します。

**レスポンス例:**

```json
[
  {
    "id": 1,
    "title": "最初の投稿",
    "content": "これはテスト投稿です。",
    "author": "太郎",
    "created_at": "2025-10-03 10:00:00"
  }
]
```

### POST /api/posts/create

新しい投稿を作成します。

**パラメータ:**
- `title`: 投稿タイトル（必須）
- `content`: 投稿内容（必須）
- `author`: 著者名（必須）

**レスポンス例:**

```json
{
  "status": "success"
}
```

---

## カスタマイズ

### ポート番号の変更

`main.lisp` を編集：

```lisp
(start-server :port 3000)  ;; 任意のポート番号
```

### スタイルのカスタマイズ

`static/css/style.css` を編集して、独自のスタイルを追加できます。

### 機能の追加

- コメント機能
- 画像アップロード
- マークダウン対応
- タグ機能
- 検索機能

---

## ライセンス

MIT License

## 作者

Your Name

## 貢献

プルリクエストを歓迎します！

---

## 参考リンク

- [SBCL Documentation](http://www.sbcl.org/manual/)
- [Hunchentoot](https://edicl.github.io/hunchentoot/)
- [Postmodern](https://marijnhaverbeke.nl/postmodern/)
- [Vue 3 Documentation](https://vuejs.org/)
- [Tailwind CSS Documentation](https://tailwindcss.com/)
- [Masonry Layout](https://masonry.desandro.com/)
