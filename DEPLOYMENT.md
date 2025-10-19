# デプロイガイド

このドキュメントでは、lisp-blogアプリケーションを本番環境にデプロイする手順を説明します。

---

## 📋 前提条件

以下がサーバーにインストールされていることを確認してください：

- **SBCL** (Steel Bank Common Lisp)
- **Quicklisp** (Common Lispパッケージマネージャー)
- **PostgreSQL** (データベース)
- **Redis** (セッション管理)
- **Node.js** と **npm** (フロントエンドビルド用)
- **Nginx** (リバースプロキシ、推奨)

---

## 🚀 デプロイ手順

### ステップ1: リポジトリのクローン

```bash
cd /var/www/  # または任意のディレクトリ
git clone https://github.com/your-username/lisp-blog.git
cd lisp-blog
```

---

### ステップ2: データベースのセットアップ

```bash
# PostgreSQLにログイン
sudo -u postgres psql

# データベースとユーザーを作成
CREATE DATABASE blogdb;
CREATE USER bloguser WITH PASSWORD 'your-secure-password';
GRANT ALL PRIVILEGES ON DATABASE blogdb TO bloguser;
\q
```

`src/config.lisp` のデータベース接続情報を更新：

```lisp
(defparameter *db-spec*
  '(:postgres
    :database-name "blogdb"
    :username "bloguser"
    :password "your-secure-password"
    :host "localhost"
    :port 5432))
```

---

### ステップ3: Redisの起動

```bash
# Redisを起動
sudo systemctl start redis
sudo systemctl enable redis  # 自動起動を有効化

# 動作確認
redis-cli ping  # 「PONG」と返ってくればOK
```

---

### ステップ4: Common Lisp依存関係のインストール

```bash
# SBCLを起動
sbcl

# Quicklispで依存関係をインストール
* (ql:quickload :lisp-blog)
* (exit)
```

---

### ステップ5: フロントエンドのビルド

```bash
# ビルドスクリプトを実行
./build.sh
```

これにより、以下が実行されます：
1. `frontend/` ディレクトリで `npm run build` を実行
2. ビルド成果物を `static/` ディレクトリにコピー

---

### ステップ6: データベースマイグレーション

```bash
sbcl

# REPLで
* (ql:quickload :lisp-blog)
* (in-package :lisp-blog)
* (lisp-blog:init-db)  # テーブル作成
* (exit)
```

---

### ステップ7: systemdサービスの設定（推奨）

サーバーを自動起動するために、systemdサービスを作成します。

`/etc/systemd/system/lisp-blog.service` を作成：

```ini
[Unit]
Description=Lisp Blog Application
After=network.target postgresql.service redis.service

[Service]
Type=simple
User=www-data
WorkingDirectory=/var/www/lisp-blog
ExecStart=/usr/local/bin/sbcl --load start-server.lisp
Restart=on-failure
RestartSec=5s

# 環境変数
Environment="PATH=/usr/local/bin:/usr/bin:/bin"
Environment="HOME=/var/www"

# ログ
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

サービスを有効化して起動：

```bash
sudo systemctl daemon-reload
sudo systemctl enable lisp-blog
sudo systemctl start lisp-blog

# 状態確認
sudo systemctl status lisp-blog

# ログ確認
sudo journalctl -u lisp-blog -f
```

---

### ステップ8: Nginxの設定

Nginxをリバースプロキシとして設定し、SSL/TLSを終端します。

`/etc/nginx/sites-available/lisp-blog` を作成：

```nginx
# HTTPからHTTPSへリダイレクト
server {
    listen 80;
    server_name your-domain.com;
    return 301 https://$server_name$request_uri;
}

# HTTPS設定
server {
    listen 443 ssl http2;
    server_name your-domain.com;

    # SSL証明書（Let's Encryptを使用する場合）
    ssl_certificate /etc/letsencrypt/live/your-domain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/your-domain.com/privkey.pem;

    # セキュリティヘッダー
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    # 静的ファイルキャッシュ
    location /assets/ {
        proxy_pass http://localhost:8080;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # APIリクエスト
    location /api/ {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # その他のリクエスト
    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Nginxを有効化：

```bash
sudo ln -s /etc/nginx/sites-available/lisp-blog /etc/nginx/sites-enabled/
sudo nginx -t  # 設定ファイルのテスト
sudo systemctl reload nginx
```

---

### ステップ9: SSL証明書の取得（Let's Encrypt）

```bash
# Certbotをインストール
sudo apt-get install certbot python3-certbot-nginx

# 証明書を取得
sudo certbot --nginx -d your-domain.com

# 自動更新を有効化
sudo systemctl enable certbot.timer
```

---

## ✅ デプロイ確認

デプロイが成功したか確認します：

```bash
# サービス状態確認
sudo systemctl status lisp-blog

# Nginxステータス
sudo systemctl status nginx

# アプリケーションにアクセス
curl https://your-domain.com
```

ブラウザで `https://your-domain.com` にアクセスし、ログインページが表示されることを確認してください。

---

## 🔄 アップデート手順

コードを更新した場合：

```bash
cd /var/www/lisp-blog

# 1. 最新コードを取得
git pull origin main

# 2. フロントエンドを再ビルド
./build.sh

# 3. サービスを再起動
sudo systemctl restart lisp-blog
```

---

## 📊 ログ確認

### アプリケーションログ

```bash
# リアルタイムでログを表示
sudo journalctl -u lisp-blog -f

# 直近100行を表示
sudo journalctl -u lisp-blog -n 100
```

### Nginxログ

```bash
# アクセスログ
sudo tail -f /var/log/nginx/access.log

# エラーログ
sudo tail -f /var/log/nginx/error.log
```

---

## 🔒 セキュリティ設定

### Cookie設定の変更

本番環境では、`src/handler/auth.lisp` のCookie設定を変更してください：

```lisp
;; 開発環境（現在の設定）
(format nil "session_id=~A; HttpOnly; Path=/; Max-Age=604800"
        session-id)

;; 本番環境（推奨）
(format nil "session_id=~A; HttpOnly; Secure; SameSite=Strict; Path=/; Max-Age=604800"
        session-id)
```

**変更箇所**:
- `Secure` フラグを追加（HTTPS接続でのみCookie送信）
- `SameSite=Strict` を追加（CSRF攻撃対策）

### ファイアウォール設定

```bash
# UFWを使用する場合
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw allow 22/tcp  # SSH
sudo ufw enable
```

---

## ⚠️ トラブルシューティング

### サービスが起動しない

```bash
# ログを確認
sudo journalctl -u lisp-blog -n 100

# 手動で起動してエラーを確認
cd /var/www/lisp-blog
sbcl --load start-server.lisp
```

### ポート8080が既に使用されている

```bash
# ポートを使用しているプロセスを確認
sudo lsof -i:8080

# プロセスを停止
sudo kill <PID>
```

### 静的ファイルが表示されない

```bash
# static/ ディレクトリの権限を確認
ls -la static/

# 権限を修正
sudo chown -R www-data:www-data /var/www/lisp-blog/static
sudo chmod -R 755 /var/www/lisp-blog/static
```

### データベース接続エラー

```bash
# PostgreSQLが起動しているか確認
sudo systemctl status postgresql

# 接続をテスト
psql -h localhost -U bloguser -d blogdb
```

---

## 📚 参考リンク

- [SBCL Manual](http://www.sbcl.org/manual/)
- [Caveman2 Documentation](https://github.com/fukamachi/caveman)
- [Nginx Documentation](https://nginx.org/en/docs/)
- [Let's Encrypt](https://letsencrypt.org/)

---

## 💡 ヒント

- **定期的なバックアップ**: PostgreSQLデータベースを定期的にバックアップしてください
- **モニタリング**: アプリケーションのログを監視し、エラーを早期に発見してください
- **セキュリティアップデート**: OSとソフトウェアを定期的にアップデートしてください
- **パフォーマンス**: 負荷が高い場合は、データベースにインデックスを追加してください

---

**以上でデプロイ作業は完了です！**

問題が発生した場合は、ログを確認し、必要に応じてGitHub Issuesで質問してください。
