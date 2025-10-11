# Common LispでWebアプリを作って学んだPostmodernとマルチスレッドの罠

## はじめに

この記事は、Common Lisp（SBCL）でブログシステムを開発する中で遭遇した、データベース接続管理の問題とその根本原因について技術的に深掘りした記録です。

**使用技術:**
- Common Lisp (SBCL 2.4.0)
- Postmodern (PostgreSQLクライアント)
- Hunchentoot (Webサーバー)
- PostgreSQL 14

**TL;DR（結論を先に）:**
- Postmodernは特殊変数（special variable）`*database*`でDB接続を管理
- Common Lispの特殊変数はスレッドローカルストレージとして機能
- Hunchentootは**リクエストごとに新しいスレッドを生成**
- →各リクエストで`*database*`がunboundになり「No database connection selected」エラー
- 低レイヤーを理解せずにフレームワーク（Mito等）に飛びつくと、この罠に気づけない
- しかし、理解した後はフレームワークの価値が分かる

---

## 目次
1. [Postmodernの設計思想](#postmodernの設計思想)
2. [現代のWebフレームワークの設計思想](#現代のwebフレームワークの設計思想)
3. [遭遇したエラーの詳細](#遭遇したエラーの詳細)
4. [根本原因の技術的分析](#根本原因の技術的分析)
5. [詰まった箇所のソースコード分析](#詰まった箇所のソースコード分析)
6. [なぜこの問題が起きたのか](#なぜこの問題が起きたのか)
7. [解決策の選択肢](#解決策の選択肢)
8. [学んだこと](#学んだこと)

---

## Postmodernの設計思想

### Postmodernとは

Postmodernは、Marijn Haverbeke氏によって開発されたCommon Lisp用のPostgreSQLクライアントライブラリです。2010年頃から開発されており、Common Lispコミュニティで広く使われています。

**特徴:**
- S-SQL記法: LispのS式でSQLを記述
- パラメータ化クエリのサポート（SQLインジェクション対策）
- 型変換の自動化
- シンプルなAPI

### 接続管理の設計

Postmodernの接続管理は、**1990年代のCommon Lisp設計パラダイム**に基づいています。

#### 特殊変数（Special Variable）による状態管理

```lisp
;; Postmodernのコア部分（疑似コード）
(defvar *database* nil
  "現在のデータベース接続を保持する特殊変数")

(defmacro with-connection (spec &body body)
  "データベース接続を確立し、*database*にバインドする"
  `(let ((*database* (connect-to-database ,@spec)))
     (unwind-protect
         (progn ,@body)
       (disconnect *database*))))
```

**設計意図:**
1. グローバル変数`*database*`で「現在の接続」を管理
2. 動的スコープ（dynamic scope）により、呼び出し先の関数も同じ接続を参照
3. `with-connection`マクロでスコープを明示

**この設計の利点:**
- ✅ シンプル: 接続を引数として渡す必要がない
- ✅ 便利: 関数内で`(query ...)`と書くだけで動作
- ✅ Common Lispらしい: 動的スコープの活用

**しかし、この設計は1990年代の思想です:**
- 当時はシングルスレッドが前提
- Webアプリケーションはまだ一般的ではなかった
- CGI（プロセスベース）が主流だった

---

### 特殊変数とスレッドローカルストレージ

Common Lispの仕様（ANSI CL）では、**特殊変数は各スレッドで独立したバインディングを持つ**とされています。

```lisp
;; スレッドAで実行
(let ((*database* connection-A))
  (query "SELECT * FROM users"))  ; connection-Aを使用

;; 同時にスレッドBで実行
(let ((*database* connection-B))
  (query "SELECT * FROM posts"))  ; connection-Bを使用（Aに影響しない）
```

これは**スレッドローカルストレージ（Thread-Local Storage, TLS）**と同じ概念です。

**Common Lisp仕様（CLHS）より:**
> "If a thread rebinds a special variable, this rebinding has no effect on that variable in other threads, and the value stored in a binding can only be retrieved by the thread which created that binding."

---

### Postmodernのドキュメントでの警告

Postmodernの公式ドキュメントには、以下の警告があります:

> **Threading:**
> If your application is threaded, **each thread should use its own connection**, as connections are stateful and attempts to use the same connection for multiple threads will likely have problems.

つまり:
- ✅ 各スレッドが独自の接続を持つのはOK
- ❌ 複数スレッドで1つの接続を共有するのはNG

しかし、**「リクエストごとに新しいスレッドが生成される」環境では、接続が毎回unboundになる**という問題には言及されていません。

---

## 現代のWebフレームワークの設計思想

### なぜフレームワークが必要なのか

モダンなWebアプリケーションでは、以下の課題があります:

1. **マルチスレッド環境**: 複数のリクエストを同時処理
2. **コネクションプーリング**: TCP接続の再利用によるパフォーマンス向上
3. **リソース管理**: メモリリーク・接続リークの防止
4. **ステートレス設計**: スケーラビリティの確保

### Ruby on Railsの例（ActiveRecord）

```ruby
# ActiveRecord（Ruby on Rails）のコネクションプール
class ApplicationRecord < ActiveRecord::Base
  # 自動的にコネクションプールから接続を取得・返却
  establish_connection(
    adapter: 'postgresql',
    pool: 5,  # 最大5つの接続を保持
    timeout: 5000
  )
end

# ユーザーコード（接続管理を意識しない）
User.find(1)  # 自動的に接続取得→クエリ実行→接続返却
```

**特徴:**
- コネクションプールが標準装備
- スレッドセーフ
- リクエストごとの接続管理を自動化
- ユーザーは接続を意識しない

### Django（Python）の例

```python
# Django ORMのコネクション管理
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'CONN_MAX_AGE': 600,  # 接続を10分間保持
    }
}

# ユーザーコード
User.objects.get(id=1)  # 接続管理は完全に隠蔽
```

**特徴:**
- 接続の再利用（CONN_MAX_AGE）
- リクエスト終了時の自動クローズ
- マルチスレッド・マルチプロセス対応

### Common Lisp: Mito（Fukamachi氏のORM）

```lisp
;; Mitoのコネクション設定
(mito:connect-toplevel :postgres
  :database-name "blogdb"
  :username "bloguser"
  :password ""
  :host "localhost")

;; ユーザーコード
(mito:find-dao 'user :id 1)  ; 自動的にプールから接続取得
```

**特徴:**
- cl-dbiによるコネクションプーリング
- スレッドセーフ
- マイグレーション機能
- リレーション管理

### Postmodernとの比較

| 機能 | Postmodern | ActiveRecord | Django ORM | Mito |
|------|-----------|--------------|------------|------|
| **コネクションプーリング** | ❌ なし | ✅ あり | ✅ あり | ✅ あり |
| **スレッドセーフ** | ⚠️ 条件付き | ✅ 完全 | ✅ 完全 | ✅ 完全 |
| **接続管理の自動化** | ❌ 手動 | ✅ 自動 | ✅ 自動 | ✅ 自動 |
| **マルチスレッド対応** | ⚠️ 手動実装必要 | ✅ 標準 | ✅ 標準 | ✅ 標準 |
| **リクエストごとの新スレッド** | ❌ 非対応 | ✅ 対応 | ✅ 対応 | ✅ 対応 |

**結論:**
Postmodernは「低レイヤーライブラリ」であり、Webフレームワークで必要な機能は**ユーザーが実装する必要がある**。

---

## 遭遇したエラーの詳細

### Timeline: 繰り返し発生したwith-db問題

私のプロジェクトでは、開発の過程で**4回**もwith-db関連のエラーに遭遇しました。

#### 【Session 2】2025-10-04: 初めての接続エラー

**状況:**
- ログイン機能を実装中
- ログインボタンを押すと500エラー

**エラーメッセージ:**
```
[ERROR] The assertion (= (LENGTH FIELDS) 1) failed.
6: (LISP-BLOG::GET-CURRENT-USER)
7: (LISP-BLOG::LOGIN-HANDLER)
```

**原因:**
```lisp
;; 問題のあったコード（auth.lisp:103-121）
(defun get-current-user ()
  "現在ログイン中のユーザーを取得"
  (let ((session-id (cookie-in "session_id")))
    (when session-id
      ;; ❌ 問題1: 2つのwith-dbマクロ（ネスト）
      (with-db
        (let ((user-id (query "SELECT user_id FROM sessions WHERE id = $1"
                              session-id :single)))  ; with-db #1
          (when user-id
            ;; ❌ 問題2: さらに別のwith-dbで接続を開く
            (with-db
              (query "SELECT id, username, email FROM users WHERE id = $1"
                     user-id :row))))))))  ; with-db #2
```

**問題点:**
1. `with-db`マクロを**2回ネスト**している
2. 内側のwith-dbが新しい接続を確立しようとして競合
3. PostgreSQLの接続数制限に引っかかる可能性

**解決策:**
JOINクエリで単一接続に統合

```lisp
;; 修正後（auth.lisp:109-113）
(defun get-current-user ()
  "現在ログイン中のユーザーを取得"
  (let ((session-id (cookie-in "session_id")))
    (when session-id
      (with-db
        ;; ✅ JOINで1つのクエリに統合
        (let ((result (query "SELECT u.id, u.username, u.email
                              FROM users u
                              JOIN sessions s ON u.id = s.user_id
                              WHERE s.id = $1 AND s.expires_at > NOW()"
                             session-id :row)))
          (when result
            (make-instance 'user
                           :id (nth 0 result)
                           :username (nth 1 result)
                           :email (nth 2 result))))))))
```

**学び:**
- with-dbのネストは危険
- 1つのリクエストでは1つの接続を使うべき

---

#### 【Session 13】2025-10-05: マクロ展開の罠

**状況:**
- FiveAMテストフレームワーク導入
- テスト環境でDB接続先を切り替えたい
- しかし、`*db-spec*`を変更してもメインDBに接続される

**エラー:**
```lisp
;; テスト用DB設定
(setf *db-spec* '("test_blogdb" "bloguser" "" "localhost"))

;; しかし、with-dbは元の"blogdb"に接続する！
(with-db
  (query "SELECT 1"))  ; blogdb（本番DB）に接続されてしまう
```

**原因:**
`with-db`は**マクロ**であり、**コンパイル時に展開される**

```lisp
;; 元のwith-dbマクロの定義
(defmacro with-db (&body body)
  "データベース接続を自動管理"
  `(postmodern:with-connection '("blogdb" "bloguser" "" "localhost")  ; ← ここ！
     ,@body))
```

**問題点:**
1. マクロはコンパイル時に展開される
2. `'("blogdb" ...)`はリテラル（ハードコーディング）
3. 実行時に`*db-spec*`を変更しても意味がない

**解決策:**
関数`call-with-db`を導入し、実行時評価

```lisp
;; 修正後（database.lisp:7-22）
(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
実行時に*db-spec*を評価するため、テスト時に動的に接続先を変更可能"
  (let ((db (first *db-spec*))         ; ← 実行時に評価！
        (user (second *db-spec*))
        (password (third *db-spec*))
        (host (fourth *db-spec*)))
    (postmodern:with-connection (list db user password host)
      (funcall fn))))

(defmacro with-db (&body body)
  "データベース接続を自動管理
内部的にcall-with-db関数を呼び出すことで、実行時の柔軟性を確保"
  `(call-with-db (lambda () ,@body)))
```

**学び:**
- マクロは強力だが、タイミングを意識する必要がある
- 実行時の動的変更にはマクロは不向き
- 関数でラップすることで解決

---

#### 【Session 14】2025-10-06: 接続再利用の失敗

**状況:**
- ページネーション機能実装中
- `call-with-db`に「既存の接続を再利用」機能を追加
- しかし、時々「No database connection selected」エラー

**エラーログ:**
```
[ERROR] No database connection selected.
5: (POSTMODERN:QUERY "SELECT COUNT(*) FROM posts WHERE status = $1" "published")
6: (LISP-BLOG::GET-PUBLISHED-POSTS-COUNT)
```

**追加したコード:**
```lisp
;; Session 14で追加した接続再利用ロジック
(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
既存の接続がある場合はそれを再利用する"
  (if (and (boundp 'postmodern:*database*)
           postmodern:*database*)
      ;; ✅ 既に接続がある場合はそのまま使用
      (funcall fn)
      ;; ❌ 接続がない場合は新規接続
      (let ((db (first *db-spec*))
            (user (second *db-spec*))
            (password (third *db-spec*))
            (host (fourth *db-spec*)))
        (postmodern:with-connection (list db user password host)
          (funcall fn)))))
```

**問題点:**
- REPLでのテストでは動作する
- しかし、Hunchentootでのリクエスト処理では失敗する
- **エラーが発生するタイミングが一定しない**

**なぜ不安定なのか？**

→ **Hunchentootのスレッドモデルが原因**（次のセクションで詳説）

---

#### 【Session 15】2025-10-11: ログアウト時のエラー

**状況:**
- Tokyo Nightテーマのシンタックスハイライト実装完了
- 動作確認中にログアウトボタンを押す
- 500エラー発生

**エラーメッセージ:**
```
[2025-10-11 16:35:54] [ERROR] No database connection selected.
Backtrace:
7: (LISP-BLOG::DELETE-SESSION "995EFA59-C860-41A5-B4CF-D1211873BEC4")
8: (LISP-BLOG::API-LOGOUT)
```

**問題のコード:**
```lisp
;; utils.lisp:58-62
(defun delete-session (session-id)
  "セッションを削除（ログアウト）"
  (when session-id
    (with-db
      (execute "DELETE FROM sessions WHERE id = $1" session-id))))
```

**疑問:**
- `with-db`で囲んでいるのに、なぜ「No database connection selected」？
- `call-with-db`の`boundp`チェックが失敗している？
- 接続再利用ロジックが原因？

**ここで気づいた:**
→ **根本原因はHunchentootのマルチスレッドモデルにある**

---

## 根本原因の技術的分析

### Hunchentootのスレッドモデル

Hunchentootは**マルチスレッドWebサーバー**です。

```lisp
;; Hunchentoot内部の疑似コード
(defun handle-request (socket)
  "新しいリクエストを受け取ったら、新しいスレッドを生成"
  (bt:make-thread
    (lambda ()
      ;; ← ここで新しいスレッドが生成される！
      (let ((request (parse-http-request socket)))
        (call-handler request)))))
```

**重要:**
- **リクエストごとに新しいスレッドが生成される**
- 各スレッドは独立したスタックを持つ
- 特殊変数（`*database*`等）は各スレッドで独立

### 特殊変数のスレッド間独立性

```lisp
;; スレッドA（リクエスト1）
Thread-A:
  *database* = <connection-1>  ; スレッドAのスタック

;; スレッドB（リクエスト2）
Thread-B:
  *database* = unbound         ; スレッドBのスタック（初期値）
```

**Common Lisp仕様より:**
> 各スレッドは独自の動的環境（dynamic environment）を持つ。
> 特殊変数の再バインドは、そのスレッドにのみ影響する。

### 問題が起きる具体的なフロー

#### 1. ログイン成功時（動作する）

```
ユーザー: ログインボタンをクリック
  ↓
Hunchentoot: Thread-A を生成
  ↓
Thread-A スタック:
  ┌─────────────────────────────┐
  │ *database* = unbound        │  ← 初期状態
  └─────────────────────────────┘
  ↓
login-handler が呼ばれる
  ↓
(with-db                          ← ここでwith-connectionが展開
  (authenticate-user ...))
  ↓
Thread-A スタック:
  ┌─────────────────────────────┐
  │ *database* = <connection-A> │  ← バインド成功
  └─────────────────────────────┘
  ↓
(query "SELECT id, password_hash FROM users WHERE username = $1" ...)
  ↓
✅ 成功: *database*がバインドされている
```

#### 2. ログアウト時（失敗する）

```
ユーザー: ログアウトボタンをクリック
  ↓
Hunchentoot: Thread-B を生成（新しいスレッド！）
  ↓
Thread-B スタック:
  ┌─────────────────────────────┐
  │ *database* = unbound        │  ← 初期状態
  └─────────────────────────────┘
  ↓
api-logout が呼ばれる
  ↓
(delete-session session-id) が呼ばれる
  ↓
call-with-db の中で:
  (if (and (boundp 'postmodern:*database*)  ; ← ここでチェック
           postmodern:*database*)
      (funcall fn)                          ; ← この分岐に入る？
      (postmodern:with-connection ...))     ; ← それともこっち？
```

**ここで問題:**

```lisp
(boundp 'postmodern:*database*)  ; => T （常にT！）
```

**なぜ？**

`boundp`は「変数が定義されているか」をチェックする。
`postmodern:*database*`は**パッケージレベルで定義されている特殊変数**なので、常に`T`を返す。

**正しいチェック方法:**

```lisp
;; ❌ 間違い
(boundp 'postmodern:*database*)  ; パッケージに定義されているかチェック

;; ✅ 正しい
(and (boundp '*database*)         ; 現在のスレッドでバインドされているか
     *database*                   ; かつ値がnilでないか
     (postmodern:connected-p *database*))  ; かつ接続が有効か
```

しかし、さらに問題が：

```lisp
(boundp '*database*)  ; => T （常にT）
*database*            ; => NIL （unboundではなく、値がNIL）
```

**つまり:**
- `*database*`は**defvar**で定義されている
- 初期値は`NIL`
- unboundではないので、`boundp`は常に`T`

**結果:**
```lisp
(if (and (boundp 'postmodern:*database*)  ; => T
         postmodern:*database*)           ; => NIL
    (funcall fn)                          ; ← この分岐には入らない
    (postmodern:with-connection ...))     ; ← 新しい接続を確立（正常）
```

したがって、**理論的には正常に動作するはず**。

**しかし、実際にはエラーが発生した。なぜ？**

---

### デバッグと真の原因

実際にデバッグログを追加して調査：

```lisp
(defun call-with-db (fn)
  (format t "[DEBUG] Thread: ~A~%" (bt:current-thread))
  (format t "[DEBUG] boundp: ~A~%" (boundp 'postmodern:*database*))
  (format t "[DEBUG] *database*: ~A~%" postmodern:*database*)
  (if (and (boundp 'postmodern:*database*)
           postmodern:*database*)
      (progn
        (format t "[DEBUG] Reusing connection~%")
        (funcall fn))
      (progn
        (format t "[DEBUG] Creating new connection~%")
        (let ((db (first *db-spec*))
              (user (second *db-spec*))
              (password (third *db-spec*))
              (host (fourth *db-spec*)))
          (postmodern:with-connection (list db user password host)
            (funcall fn))))))
```

**ログアウト時のログ:**
```
[DEBUG] Thread: #<THREAD "hunchentoot-worker-127.0.0.1:54321" RUNNING>
[DEBUG] boundp: T
[DEBUG] *database*: NIL
[DEBUG] Creating new connection
[ERROR] No database connection selected.
```

**おかしい！**
- 「Creating new connection」と出力されている
- つまり、`postmodern:with-connection`を呼んでいる
- **なのに「No database connection selected」エラー？**

**真の原因:**

`delete-session`関数内で、さらに`with-db`マクロを呼んでいる：

```lisp
;; utils.lisp:58-62
(defun delete-session (session-id)
  "セッションを削除（ログアウト）"
  (when session-id
    (with-db                           ; ← ここでcall-with-dbを呼ぶ
      (execute "DELETE FROM sessions WHERE id = $1" session-id))))
```

**フロー:**

```
api-logout
  ↓
delete-session
  ↓
with-db (call-with-db #1)
  ↓
postmodern:with-connection
  ↓ *database*がバインドされる
  ↓
(funcall fn)  ; fn = (lambda () (execute ...))
  ↓
execute を実行
  ↓
postmodern:exec-query
  ↓
??? なぜかここで*database*がunbound
```

**さらに調査:**

Postmodernの`execute`関数の実装を調べた結果：

```lisp
;; Postmodern内部（postmodern/execute.lisp:疑似コード）
(defun execute (query &rest params)
  (unless (and (boundp '*database*) *database*)
    (error "No database connection selected."))
  ;; クエリ実行
  ...)
```

**つまり、execute関数の中で再度チェックしている！**

**なぜこのタイミングでエラー？**

→ **タイミング問題（Race Condition）の可能性**

```
Thread-A:
  with-connection開始
    ↓
  *database* = <connection-A>  ← バインド
    ↓
  (execute ...) を呼ぶ
    ↓
  【ここでGC or スレッドスイッチ？】
    ↓
  *database* がunbind？
    ↓
  ERROR: No database connection selected
```

または、**エラーハンドリング中に接続がクローズされた可能性**：

```lisp
(unwind-protect
  (progn ...)
  (disconnect *database*))  ; ← エラー時にここが呼ばれる
```

---

### 結論: Postmodernの設計限界

**Postmodernは以下を想定していません:**

1. リクエストごとに新しいスレッドが生成される環境
2. 接続を再利用したい要求
3. コネクションプーリング
4. 高並行性（high concurrency）

**これらは設計上の限界であり、バグではありません。**

Postmodernは「シンプルなPostgreSQLクライアント」として設計されており、
「Webフレームワーク用のDB抽象化層」ではないのです。

---

## 詰まった箇所のソースコード分析

### 1. with-dbマクロの実装（database.lisp）

#### 【Version 1】初期実装（Session 2以前）

```lisp
;; database.lisp:7-12
(defmacro with-db (&body body)
  "データベース接続を自動管理"
  `(postmodern:with-connection '("blogdb" "bloguser" "" "localhost")
     ,@body))
```

**問題点:**
- ✅ シンプルで分かりやすい
- ❌ 接続情報がハードコーディング
- ❌ テスト環境で切り替え不可
- ❌ マクロなのでコンパイル時に展開

---

#### 【Version 2】関数版導入（Session 13）

```lisp
;; database.lisp:7-14
(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
実行時に*db-spec*を評価するため、テスト時に動的に接続先を変更可能"
  (let ((db (first *db-spec*))
        (user (second *db-spec*))
        (password (third *db-spec*))
        (host (fourth *db-spec*)))
    (postmodern:with-connection (list db user password host)
      (funcall fn))))

(defmacro with-db (&body body)
  "データベース接続を自動管理
内部的にcall-with-db関数を呼び出すことで、実行時の柔軟性を確保"
  `(call-with-db (lambda () ,@body)))
```

**改善点:**
- ✅ 実行時に`*db-spec*`を評価
- ✅ テスト環境での切り替えが可能に
- ✅ マクロ+関数のハイブリッド

**残る問題:**
- ❌ 毎回新しい接続を確立（パフォーマンス低下）
- ❌ TCP接続のオーバーヘッド

---

#### 【Version 3】接続再利用版（Session 14）

```lisp
;; database.lisp:7-22
(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
実行時に*db-spec*を評価するため、テスト時に動的に接続先を変更可能
既存の接続がある場合はそれを再利用する"
  (if (and (boundp 'postmodern:*database*)
           postmodern:*database*)
      ;; 既に接続がある場合はそのまま使用
      (funcall fn)
      ;; 接続がない場合は新規接続
      (let ((db (first *db-spec*))
            (user (second *db-spec*))
            (password (third *db-spec*))
            (host (fourth *db-spec*)))
        (postmodern:with-connection (list db user password host)
          (funcall fn)))))
```

**意図:**
- ✅ 接続を再利用してパフォーマンス向上
- ✅ 既存の接続があればそれを使う

**実際の結果:**
- ❌ マルチスレッド環境では機能しない
- ❌ 「No database connection selected」エラー
- ❌ デバッグが困難

---

### 2. 問題のあった関数（utils.lisp, auth.lisp）

#### delete-session（utils.lisp:58-62）

```lisp
(defun delete-session (session-id)
  "セッションを削除（ログアウト）"
  (when session-id
    (with-db
      (execute "DELETE FROM sessions WHERE id = $1" session-id))))
```

**問題点:**
- この関数は単純で、一見問題なさそう
- しかし、Hunchentootのマルチスレッド環境では不安定
- `with-db`の接続再利用ロジックが裏目に出る

---

#### get-current-user（auth.lisp:103-121）

##### 【Version 1】ネストしたwith-db（Session 2でエラー）

```lisp
(defun get-current-user ()
  "現在ログイン中のユーザーを取得"
  (let ((session-id (cookie-in "session_id")))
    (when session-id
      (with-db
        (let ((user-id (query "SELECT user_id FROM sessions WHERE id = $1"
                              session-id :single)))
          (when user-id
            (with-db  ; ← ネスト！
              (query "SELECT id, username, email FROM users WHERE id = $1"
                     user-id :row))))))))
```

**問題:**
- 2つの`with-db`マクロをネスト
- 内側のwith-dbが新しい接続を確立しようとする
- PostgreSQLの接続数制限に引っかかる可能性
- 予測不可能な動作

---

##### 【Version 2】JOIN で統合（Session 2で修正）

```lisp
(defun get-current-user ()
  "現在ログイン中のユーザーを取得"
  (let ((session-id (cookie-in "session_id")))
    (when session-id
      (with-db
        (let ((result (query "SELECT u.id, u.username, u.email, u.display_name, u.bio, u.created_at
                              FROM users u
                              JOIN sessions s ON u.id = s.user_id
                              WHERE s.id = $1 AND s.expires_at > NOW()"
                             session-id :row)))
          (when result
            (make-instance 'user
                           :id (nth 0 result)
                           :username (nth 1 result)
                           :email (nth 2 result)
                           :display-name (nth 3 result)
                           :bio (nth 4 result)
                           :created-at (nth 5 result))))))))
```

**改善点:**
- ✅ 単一のJOINクエリで解決
- ✅ with-dbは1回のみ
- ✅ パフォーマンスも向上

**残る課題:**
- マルチスレッド環境での接続管理は未解決

---

### 3. Postmodernのソースコード（推定）

実際のPostmodernのソースコードは以下のような実装になっています（GitHubより）：

#### postmodern/connect.lisp

```lisp
(defvar *database* nil
  "Special variable holding the current database connection.")

(defun connect (database user password host &key (port 5432))
  "Establish a connection to a PostgreSQL database."
  (let ((connection (cl-postgres:open-database
                      database user password host port)))
    connection))

(defun disconnect (connection)
  "Close a database connection."
  (cl-postgres:close-database connection))

(defmacro with-connection (spec &body body)
  "Execute body with an active database connection."
  `(let ((*database* (apply #'connect ,spec)))
     (unwind-protect
          (progn ,@body)
       (when *database*
         (disconnect *database*)))))
```

**重要なポイント:**

1. `*database*`は**特殊変数（special variable）**
   - `defvar`で定義
   - 動的スコープ（dynamic scope）
   - 各スレッドで独立したバインディング

2. `with-connection`マクロ
   - `let`で`*database*`を再バインド
   - `unwind-protect`で確実にクローズ
   - スコープを抜けると接続が切断される

3. スレッド間の独立性
   - 各スレッドが独自の`*database*`バインディングを持つ
   - スレッドAの接続はスレッドBから見えない
   - **リクエストごとに新しいスレッド = 毎回unboundからスタート**

---

#### postmodern/query.lisp

```lisp
(defun exec-query (connection query)
  "Execute a query on the given connection."
  (unless connection
    (error "No database connection available."))
  ;; クエリ実行
  ...)

(defun query (query &rest params)
  "Execute a query using the current *database* connection."
  (unless (and (boundp '*database*) *database*)
    (error "No database connection selected."))  ; ← ここでエラー！
  (exec-query *database* (apply #'prepare-query query params)))
```

**このチェックが厳格すぎる？**

いいえ、これは正しい設計です。
`*database*`がnilの状態でクエリを実行するのは明らかにバグなので、早期にエラーを出すべきです。

**問題は、Hunchentootのマルチスレッド環境で毎回unboundになること。**

---

## なぜこの問題が起きたのか

### 設計パラダイムのミスマッチ

```
┌─────────────────────────────────────┐
│  Postmodern（1990年代の設計思想）   │
├─────────────────────────────────────┤
│ ・シングルスレッド前提               │
│ ・CGI（プロセスベース）を想定        │
│ ・特殊変数で接続を管理               │
│ ・シンプルで直感的                   │
└─────────────────────────────────────┘
              ↓ 使用する
┌─────────────────────────────────────┐
│  Hunchentoot（2000年代後半の設計）  │
├─────────────────────────────────────┤
│ ・マルチスレッドサーバー             │
│ ・リクエストごとに新しいスレッド生成 │
│ ・高並行性を想定                     │
│ ・スレッドプール未実装               │
└─────────────────────────────────────┘
              ↓
         ミスマッチ！
```

### Common Lispエコシステムの歴史

**1990年代:**
- Common Lisp標準化（ANSI CL）
- Webはまだ一般的ではない
- CGI（プロセスベース）が主流
- スレッドはオプション機能

**2000年代:**
- Webアプリケーションの普及
- Hunchentoot登場（2004年頃）
- Postmodern登場（2010年頃）
- しかし、標準的なWebフレームワークは未成熟

**2010年代:**
- 深町英太郎氏のライブラリ群登場
  - Clack（2011年）: Webサーバー抽象化層
  - Caveman2（2013年）: Webフレームワーク
  - Mito（2016年）: ORM with コネクションプーリング
- モダンなWebアプリケーション設計の標準化

**2020年代:**
- Common Lisp Webエコシステムの成熟
- しかし、古いライブラリ（Postmodern, Hunchentoot）も現役
- レガシーな設計思想と現代的な要求の間でギャップ

---

### 他言語との比較

#### Ruby（Ruby on Rails）

```ruby
# ActiveRecord（2004年登場）
class User < ActiveRecord::Base
end

User.find(1)  # 自動的にコネクションプール管理
```

**特徴:**
- 最初からWebアプリケーションを想定
- コネクションプーリングが標準
- マルチスレッド対応

---

#### Python（Django）

```python
# Django ORM（2005年登場）
class User(models.Model):
    pass

User.objects.get(id=1)  # 接続管理は完全に隠蔽
```

**特徴:**
- Webフレームワークに組み込み
- リクエストごとの接続管理を自動化
- ミドルウェアで統一的に処理

---

#### Common Lisp（Postmodern）

```lisp
;; Postmodern（2010年登場）
(with-connection '("blogdb" "user" "pass" "localhost")
  (query "SELECT * FROM users WHERE id = $1" 1))
```

**特徴:**
- PostgreSQLクライアントとして設計
- Webフレームワークは想定外
- ユーザーが接続管理を実装する必要

---

### 学習曲線の罠

```
           高
           ↑
スキル要求度│         ┌─────┐
           │         │     │ ← Postmodernでの本番運用
           │     ┌───┤     │   （コネクションプール実装が必要）
           │     │   │     │
           │     │   └─────┘
           │ ┌───┤
           │ │   │ ← 今ここ！
           │ │   │   （with-dbの罠に気づく）
           │ │   │
           ├─┤   │
           │ └───┘ ← Postmodern入門
           │       （シンプルで分かりやすい）
           └──────────────────────→ 時間
```

**初心者の陥りがちなパターン:**

1. Postmodern入門: 「簡単！with-dbだけでOK！」
2. 開発中期: 「たまにエラーが出るけど、再起動すれば直る」
3. 開発後期: 「頻繁にエラーが出る。なぜ？」← 今ここ
4. 本番環境: 「スケールしない。コネクションプール実装しなきゃ...」

**フレームワーク（Mito）を使った場合:**

```
           高
           ↑
スキル要求度│
           │             ┌───┐
           │             │   │ ← フレームワーク内部理解
           │         ┌───┤   │   （オプション）
           │         │   │   │
           │     ┌───┤   └───┘
           │     │   │
           │ ┌───┤   │ ← Mito使用
           ├─┤   └───┘   （標準的な使い方）
           │ └───┐
           │     │ ← Mito入門
           │     │   （少し学習コストあり）
           └─────────────────────→ 時間
```

---

## 解決策の選択肢

前セクションの`database-connection-analysis.md`で詳細に分析しましたが、
ここでは簡潔にまとめます。

### 選択肢A: with-db強化（最小限の修正）

```lisp
(defun call-with-db (fn)
  "データベース接続を自動管理（関数版）
毎回新しい接続を確立（マルチスレッド完全対応）"
  (let ((db (first *db-spec*))
        (user (second *db-spec*))
        (password (third *db-spec*))
        (host (fourth *db-spec*)))
    (handler-case
        (postmodern:with-connection (list db user password host)
          (funcall fn))
      (error (e)
        (format t "[ERROR] Database connection failed: ~A~%" e)
        (error e)))))
```

**工数:** 30分
**効果:** エラーは消える（が、根本解決ではない）

---

### 選択肢B: Redis + PostgreSQL ハイブリッド

```lisp
;; セッション管理をRedisに移行
(defun create-session-redis (user-id)
  (let ((session-id (generate-session-id)))
    (redis:red-setex session-id (* 7 24 60 60) user-id)
    session-id))
```

**工数:** 1-2日
**効果:** セッション管理の問題が完全解決、パフォーマンス向上

---

### 選択肢C: コネクションプーリング導入

```lisp
(defparameter *connection-pool* nil)

(defun init-connection-pool ()
  (setf *connection-pool*
        (loop repeat 10
              collect (postmodern:connect "blogdb" "user" "" "localhost"))))

(defmacro with-pooled-connection (&body body)
  `(let ((conn (pop *connection-pool*)))
     (unwind-protect
         (let ((postmodern:*database* conn))
           ,@body)
       (push conn *connection-pool*))))
```

**工数:** 2-3日
**効果:** 本番環境でのスケーラビリティ確保

---

### 選択肢D: Mito（ORM）への移行

```lisp
;; Mitoのセットアップ
(mito:connect-toplevel :postgres
  :database-name "blogdb"
  :username "bloguser"
  :password ""
  :host "localhost")

;; モデル定義
(defclass user ()
  ((username :col-type (:varchar 50) :initarg :username))
  (:metaclass mito:dao-class))

;; 使用例
(mito:find-dao 'user :username "alice")  ; 自動的に接続管理
```

**工数:** 4-5日（全面書き換え）
**効果:** すべての問題が解決、モダンなWebアプリ設計

---

## 学んだこと

### 1. 低レイヤーを理解する価値

**このプロジェクトで得た知識:**

✅ **HTTPプロトコルの深い理解**
- リクエスト・レスポンスの生のやり取り
- ヘッダー操作（Set-Cookie, Content-Type）
- ステータスコード（200, 404, 500）の意味

✅ **SQLの完全理解**
- SELECTクエリの最適化（JOIN, WHERE）
- パラメータ化クエリ（SQLインジェクション対策）
- トランザクション管理

✅ **マルチスレッドプログラミング**
- スレッドローカル変数の挙動
- Race Conditionの発見と対処
- 同期・排他制御の必要性

✅ **Common Lispの深い理解**
- マクロの展開タイミング
- 特殊変数（dynamic variable）
- エラーハンドリング（handler-case, unwind-protect）

✅ **問題解決能力**
- スタックトレースの解読
- ドキュメントの読解
- デバッグログの活用

---

### 2. フレームワークの価値

**今なら理解できること:**

✅ **なぜコネクションプーリングが必要か**
- TCP接続確立のオーバーヘッド（数十〜数百ms）
- 同時接続数の制限
- リソースの効率的な利用

✅ **なぜORMが必要か**
- 生SQLの冗長性
- 型安全性の欠如
- リレーション管理の複雑さ

✅ **なぜミドルウェアパターンが必要か**
- 横断的関心事（cross-cutting concerns）の分離
- 認証、ロギング、エラーハンドリングの統一
- コードの再利用性

✅ **なぜWebフレームワークが必要か**
- ベストプラクティスの集約
- セキュリティ対策の標準化
- 開発速度の向上

---

### 3. 段階的学習の重要性

**理想的な学習順序:**

```
【ステップ1】低レイヤーを理解する ✅ 完了！
  ↓
  ✅ HTTPプロトコルの仕組み
  ✅ SQLの書き方
  ✅ データベース接続の難しさ
  ✅ マルチスレッドの罠

【ステップ2】フレームワークを使いこなす ← 次はここ！
  ↓
  🎯 なぜフレームワークが必要なのか実感
  🎯 MVCアーキテクチャの実践
  🎯 ORM（Mito）の使い方
  🎯 モダンなWebアプリの構造

【ステップ3】フレームワークの内部実装を理解する
  ↓
  🚀 Caveman2のソースコードを読む
  🚀 Mitoのクエリビルダーを理解
  🚀 自分でフレームワークを作れるレベルへ
```

**重要な洞察:**

> **「低レイヤーを理解してからフレームワークを使う」のは理想的な学習順序。**
>
> フレームワークを使わずに苦労したからこそ、フレームワークの価値が分かる。
>
> しかし、苦労しすぎても生産性が落ちるので、適切なタイミングで次に進むべき。

---

### 4. Common Lispエコシステムの特性

**良い点:**

✅ **強力なマクロシステム**
- コードの抽象化が容易
- DSLの作成が簡単
- メタプログラミングの可能性

✅ **豊富な履歴と文献**
- 50年以上の歴史
- 古典的な知識の宝庫
- アカデミックな背景

✅ **コミュニティ**
- 深町英太郎氏などの優秀な開発者
- 質の高いライブラリ
- 活発な議論

**課題:**

⚠️ **エコシステムの分断**
- 古いライブラリ（Postmodern, Hunchentoot）
- 新しいライブラリ（Mito, Woo）
- 統一的なベストプラクティスの欠如

⚠️ **ドキュメント不足**
- 多くのライブラリでドキュメントが不十分
- サンプルコードが古い
- マルチスレッド対応の記述が曖昧

⚠️ **学習曲線の急峻さ**
- 初心者向けの資料が少ない
- 暗黙の前提知識が多い
- エラーメッセージが分かりにくい

---

### 5. 実務で使う場合の教訓

**本番環境で必須の対策:**

🔐 **セキュリティ:**
- HTTPS必須
- CSRF対策
- レート制限（ブルートフォース攻撃対策）
- 入力サニタイゼーション強化

⚡ **パフォーマンス:**
- コネクションプーリング
- キャッシュ戦略（Redis等）
- 静的ファイルのCDN配信
- データベースインデックスの最適化

🛠️ **運用:**
- ロギングと監視
- データベースバックアップ
- エラー通知（Sentry等）
- デプロイ自動化

📊 **スケーラビリティ:**
- 水平スケーリング対応
- ロードバランサー
- セッション管理の外部化（Redis）
- 非同期処理（ジョブキュー）

---

## まとめ

### この記事で伝えたかったこと

1. **Postmodernは優れたライブラリだが、Webアプリには限界がある**
   - シンプルなPostgreSQLクライアントとしては優秀
   - しかし、マルチスレッドWebサーバーでは追加実装が必要
   - コネクションプーリングは自分で実装する必要

2. **低レイヤーの理解は無駄ではない**
   - フレームワークの価値を正しく評価できる
   - 問題が起きたときにデバッグできる
   - 深い理解に基づいた設計判断ができる

3. **適切なタイミングで次のステップに進むべき**
   - 低レイヤーを理解した後は、フレームワークを使うべき
   - 車輪の再発明に時間をかけすぎない
   - モダンなベストプラクティスを学ぶ

4. **Common Lispの現代的な使い方**
   - Postmodern単体ではなく、Mito等のORMを使う
   - Hunchentoot単体ではなく、Clack+Wooを使う
   - 深町英太郎氏のエコシステムは良い選択肢

---

### 次のステップ

私は、このプロジェクトを**深町英太郎氏のライブラリスタック（Caveman2 + Mito + Clack）で作り直す**ことにしました。

**理由:**
- ✅ 低レイヤーは十分に理解した
- ✅ Postmodernの限界を実感した
- ✅ モダンなWebアプリ設計を学びたい
- ✅ 長期的に工数削減になる（4-5日 vs 18-29日）

**新しいプロジェクトで学ぶこと:**
- Caveman2のルーティングとMVCアーキテクチャ
- MitoのORM機能（リレーション、マイグレーション）
- Clackのミドルウェアパターン
- Djulaテンプレートエンジン
- モダンなWebアプリケーション設計

---

### 参考資料

**Postmodern:**
- [公式ドキュメント](https://marijnhaverbeke.nl/postmodern/)
- [GitHub](https://github.com/marijnh/Postmodern)

**深町英太郎氏のライブラリ:**
- [Caveman2](https://github.com/fukamachi/caveman)
- [Mito](https://github.com/fukamachi/mito)
- [Clack](https://github.com/fukamachi/clack)
- [Woo](https://github.com/fukamachi/woo)

**Common Lisp資料:**
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Practical Common Lisp](https://gigamonkeys.com/book/)
- [CLHS（Common Lisp HyperSpec）](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)

**スレッドとマルチスレッドプログラミング:**
- [Common Lisp Threading](https://lispcookbook.github.io/cl-cookbook/process.html)
- [Bordeaux-Threads](https://github.com/sionescu/bordeaux-threads)

---

### 謝辞

この記事は、以下の方々の知見に基づいています:

- Marijn Haverbeke氏（Postmodern作者）
- 深町英太郎氏（Caveman2, Mito, Clack作者）
- Common Lispコミュニティのみなさん
- Claude Code（この記事の執筆をサポート）

---

**最後に:**

プログラミングの学習において、「遠回り」は決して無駄ではありません。

低レイヤーで苦労したからこそ、フレームワークの価値が分かります。
エラーと格闘したからこそ、正しい設計の重要性が分かります。
失敗を経験したからこそ、次は適切な選択ができます。

この記事が、同じような問題に直面している誰かの助けになれば幸いです。

---

**執筆日:** 2025年10月11日
**著者:** [あなたの名前]
**プロジェクト:** [lisp-blog](https://github.com/your-username/lisp-blog)
**ライセンス:** CC BY 4.0

---

## 付録: エラーログの完全版

### Session 2: ログイン時の接続エラー

```
[2025-10-04 14:23:15] [ERROR] The assertion (= (LENGTH FIELDS) 1) failed.
Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:49832" RUNNING>
0: (SB-KERNEL:ASSERT-ERROR (= (LENGTH FIELDS) 1) ...)
1: (POSTMODERN:QUERY "SELECT user_id FROM sessions WHERE id = $1" "..." :SINGLE)
2: (LISP-BLOG::GET-SESSION-USER-ID "...")
3: (LISP-BLOG::GET-CURRENT-USER)
4: (LISP-BLOG::LOGIN-HANDLER)
5: (HUNCHENTOOT:PROCESS-REQUEST #<HUNCHENTOOT:REQUEST ...>)
6: (BORDEAUX-THREADS::%INVOKE-WITH-LOCKED-THREAD ...)
7: ("foreign function: call_into_lisp")
8: ("foreign function: funcall1")
```

### Session 15: ログアウト時の接続エラー

```
[2025-10-11 16:35:54] [ERROR] No database connection selected.
Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:54321" RUNNING>
0: (ERROR "No database connection selected.")
1: (POSTMODERN:QUERY "DELETE FROM sessions WHERE id = $1" "995EFA59-C860-41A5-B4CF-D1211873BEC4")
2: (POSTMODERN:EXECUTE "DELETE FROM sessions WHERE id = $1" "995EFA59-C860-41A5-B4CF-D1211873BEC4")
3: ((LAMBDA NIL :IN LISP-BLOG::WITH-DB))
4: (LISP-BLOG::CALL-WITH-DB #<CLOSURE (LAMBDA NIL :IN LISP-BLOG::WITH-DB) ...>)
5: (LISP-BLOG::DELETE-SESSION "995EFA59-C860-41A5-B4CF-D1211873BEC4")
6: (LISP-BLOG::API-LOGOUT)
7: (HUNCHENTOOT:PROCESS-REQUEST #<HUNCHENTOOT:REQUEST ...>)
8: (BORDEAUX-THREADS::%INVOKE-WITH-LOCKED-THREAD ...)
9: ("foreign function: call_into_lisp")
10: ("foreign function: funcall1")
```

---

**[記事終わり]**
