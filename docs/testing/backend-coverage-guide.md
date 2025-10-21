# バックエンドカバレッジガイド（sb-cover）

## 概要

このドキュメントでは、SBCL の sb-cover を使ったバックエンド（Common Lisp）コードのカバレッジ測定方法と、レポートの見方を解説します。

**対象**: Common Lisp バックエンドコード（src/ ディレクトリ）

**ツール**: SBCL sb-cover（SBCL に標準搭載）

**実行時間**: 約5-10分（通常テストの150-200倍）

---

## 1. 実行方法

### 基本的な実行

このプロジェクトでは専用スクリプトを用意しています:

```bash
# 標準モード（推奨）
./run-tests-coverage.sh

# 詳細モード（全出力を表示）
./run-tests-coverage.sh -v
```

### スクリプトが行う処理

1. **古いレポートの削除**: `coverage-report/` ディレクトリをクリーンアップ
2. **sb-cover 読み込み**: `(require :sb-cover)`
3. **カバレッジ有効化**: `(declaim (optimize sb-cover:store-coverage-data))`
4. **システム再コンパイル**: `(asdf:load-system :lisp-blog :force t)`
5. **テスト実行**: 全307テストケースを実行
6. **レポート生成**: `(sb-cover:report "coverage-report/")`
7. **完了メッセージ**: レポートの場所を表示

---

## 2. レポートの構造

### 生成されるファイル

```
coverage-report/
├── cover-index.html          ← メインのインデックス（★ここから開始）
├── ff4a4c7b...d3d8.html     ← config.lisp のカバレッジ
├── 901086e8...dd183.html    ← db.lisp のカバレッジ
├── 2b9e5b45...662828.html   ← service/auth.lisp のカバレッジ
├── 1fc6a48f...db206b5.html  ← service/post.lisp のカバレッジ
└── ... (計28ファイル)
```

**注意**: ファイル名はソースファイルのパスをMD5ハッシュ化したものです。`cover-index.html` から辿るのが確実です。

### レポートを開く

```bash
# macOS
open coverage-report/cover-index.html

# Linux
xdg-open coverage-report/cover-index.html

# Windows
start coverage-report/cover-index.html
```

または、ブラウザのアドレスバーに直接入力:
```
file:///Users/key-person/projects/lisp-blog/coverage-report/cover-index.html
```

---

## 3. cover-index.html の見方

### 画面構成

```
┌─────────────────────────────────────────────────────────────┐
│  Source file        │ Covered │ Total │  %  │ (Branch)      │
├─────────────────────────────────────────────────────────────┤
│  src/service/       │         │       │     │               │
│  ├─ auth.lisp       │  120    │ 123   │97.6%│ 17/18 (94.4%)│ ← 高カバレッジ
│  ├─ post.lisp       │  151    │ 154   │98.1%│ 16/18 (88.9%)│ ← 高カバレッジ
│  src/util/          │         │       │     │               │
│  ├─ crypto.lisp     │   38    │  41   │92.7%│ 0/0 (-)      │ ← 高カバレッジ
│  ├─ json.lisp       │   32    │  38   │84.2%│ 0/0 (-)      │ ← 良好
│  src/handler/       │         │       │     │               │
│  ├─ auth.lisp       │   34    │ 377   │ 9.0%│ 0/4 (0%)     │ ← 低カバレッジ
│  ├─ post.lisp       │   57    │ 695   │ 8.2%│ 0/20 (0%)    │ ← 低カバレッジ
└─────────────────────────────────────────────────────────────┘
```

### カラムの意味

| カラム | 意味 |
|--------|------|
| **Source file** | ソースファイルのパス |
| **Covered** | テストで実行された式（expression）の数 |
| **Total** | 全式の数 |
| **%** | カバレッジ率（Covered / Total × 100） |
| **Covered (Branch)** | 実行された分岐の数 |
| **Total (Branch)** | 全分岐の数 |
| **% (Branch)** | 分岐カバレッジ率 |

### Expression（式）とは？

Common Lisp では、すべてのコードが「式（expression）」です:

```lisp
;; 例: 3つの式
(defun add (a b)           ; 式1: defun全体
  (+ a b))                 ; 式2: +の呼び出し

(add 1 2)                  ; 式3: add関数の呼び出し
```

sb-cover は各式が実行されたかどうかを追跡します。

---

## 4. 個別ファイルのレポートの見方

### ファイルリンクをクリック

`cover-index.html` でファイル名（例: `auth.lisp`）をクリックすると、そのファイルの詳細レポートが開きます。

### 色分けの意味

sb-cover は実行回数に応じて背景色を変えます:

| 背景色 | 意味 | 実行回数 |
|--------|------|----------|
| 🟢 **緑** (state-1, state-5) | 実行された | 1回以上 |
| 🔴 **赤** (state-2, state-10) | 実行されなかった | 0回 |
| 🟡 **黄色** (state-6, state-9) | 部分的に実行 | 分岐の一部のみ |
| ⚪ **灰色** (state-0) | カバレッジ対象外 | コメント、空行など |
| 🔘 **薄灰** (state-15) | 最適化で削除 | コンパイラが除去 |

### 実例

```lisp
(defun authenticate-user (username password)          ; 🟢 実行された
  "ユーザー認証を行う"                                ; ⚪ カバレッジ対象外
  (let ((user (find-user-by-username username)))      ; 🟢 実行された
    (if user                                           ; 🟢 実行された
        (if (verify-password password (user-password user))  ; 🟢 実行された
            (list :success user)                       ; 🟢 実行された
            (list :error "Invalid password"))          ; 🟢 実行された
        (list :error "User not found"))))              ; 🔴 実行されなかった
```

**解釈**:
- ユーザーが見つからないケース（最後の行）がテストされていない
- → テストケースを追加すべき

---

## 5. カバレッジの解釈

### 高カバレッジ（80%以上）

```
src/service/auth.lisp: 97.6% (120/123)
src/service/post.lisp: 98.1% (151/154)
```

**意味**: ビジネスロジックがしっかりテストされている

**理由**:
- Service 層は単体テストで直接呼び出しやすい
- 複雑な分岐も網羅的にテストされている

### 低カバレッジ（50%未満）

```
src/handler/auth.lisp: 9.0% (34/377)
src/handler/post.lisp: 8.2% (57/695)
```

**意味**: Handler 層のカバレッジが不足している

**理由**:
- HTTP リクエスト/レスポンス処理が複雑
- 統合テストでは細かい分岐まで網羅しにくい
- テスト実行時に Web サーバーが起動していない

**改善方法**:
1. Handler 層の単体テスト追加（モックリクエスト使用）
2. エラーハンドリングのテスト追加
3. 各 HTTP メソッド（GET/POST/PUT/DELETE）のテスト追加

### 0% カバレッジ

```
src/web.lisp: 0.0% (0/231)
src/main.lisp: 0.0% (0/56)
```

**意味**: Web サーバー起動コードがテストされていない

**理由**:
- これらは Web サーバーの起動・停止を担当
- テストスイートは起動済みサーバーを使用しない
- REPL で手動起動することを想定

**対応**:
- Web サーバー起動のテストは優先度低
- 統合テストで間接的にテストされている

---

## 6. よくある質問

### Q1. なぜこんなに遅いのですか？

**A**: sb-cover はすべての式に対してインストルメンテーション（計測コード）を挿入するため、実行速度が大幅に低下します。

**具体例**:
```lisp
;; 元のコード
(+ a b)

;; sb-cover が変換後のイメージ
(progn
  (sb-cover:record-execution 'expression-123)
  (+ a b))
```

307テストケース × 数千の式 = 膨大なオーバーヘッド

### Q2. CI/CD で実行すべきですか？

**A**: 推奨しません。以下の理由により:

- 実行時間が5-10分と長い（CI の実行時間を圧迫）
- 通常のテスト（2-3秒）で十分に品質を保証できている
- カバレッジレポートは必要時にローカルで生成すれば十分

**推奨する使い方**:
- リリース前の品質確認として手動実行
- 新機能追加時にカバレッジを確認
- 週次や月次で定期的にチェック

### Q3. どのくらいのカバレッジを目指すべきですか？

**A**: レイヤーごとに異なります:

| レイヤー | 目標カバレッジ | 現状 |
|----------|---------------|------|
| Service 層 | 90%以上 | 97-98% ✅ |
| Util 層 | 80%以上 | 70-92% ⚠️ |
| Model 層 | 70%以上 | 73% ✅ |
| Handler 層 | 50%以上 | 8-9% ❌ |
| Web/Main 層 | （測定不要） | 0% - |

**重要なのは数字ではなく**:
- ビジネスロジックが網羅的にテストされているか
- エラーハンドリングが正しく動作するか
- エッジケースが考慮されているか

### Q4. Expression カバレッジと Branch カバレッジの違いは？

**A**:

**Expression カバレッジ**: 各式が1回でも実行されたか

```lisp
(if condition    ; この式が実行されたか
    (then-part)  ; この式が実行されたか
    (else-part)) ; この式が実行されたか
```

**Branch カバレッジ**: 分岐のすべてのパスが実行されたか

```lisp
(if condition
    (then-part)  ; ← true の分岐
    (else-part)) ; ← false の分岐
; 両方の分岐が実行されれば 100%
```

Branch カバレッジの方が厳しい基準です。

---

## 7. トラブルシューティング

### 問題: レポートが生成されない

**原因**: テストが失敗している

**解決方法**:
```bash
# 詳細モードで実行してエラーを確認
./run-tests-coverage.sh -v
```

### 問題: ファイルが見つからない

**原因**: ハッシュ化されたファイル名が分からない

**解決方法**: 必ず `cover-index.html` から辿る

### 問題: カバレッジが 0% のまま

**原因**: カバレッジ有効化前にコンパイルされた

**解決方法**:
```bash
# キャッシュをクリア
./clean-cache.sh -y

# 再実行
./run-tests-coverage.sh
```

### 問題: メモリ不足エラー

**原因**: sb-cover の計測データが大量に蓄積

**解決方法**:
```bash
# ヒープサイズを増やして実行
sbcl --dynamic-space-size 4096 --load run-tests-coverage-script.lisp
```

（現在のスクリプトでは自動対応済み）

---

## 8. 参考資料

### SBCL 公式ドキュメント

- [SBCL Manual - sb-cover](http://www.sbcl.org/manual/#sb_002dcover)

### プロジェクト内の関連ファイル

- `run-tests-coverage.sh`: カバレッジテスト実行スクリプト
- `run-tests.sh`: 通常のテスト実行スクリプト
- `.gitignore`: カバレッジレポートの除外設定

### 関連コマンド

```bash
# 通常のテスト実行（カバレッジなし）
./run-tests.sh

# カバレッジレポートの削除
rm -rf coverage-report/

# 特定のテストスイートのみ実行（REPLで）
(ql:quickload :lisp-blog/tests)
(in-package :lisp-blog-test)
(run! 'auth-service-tests)
```

---

## まとめ

- **実行**: `./run-tests-coverage.sh`
- **レポート**: `coverage-report/cover-index.html`
- **目標**: Service 層 90%以上、Handler 層の改善
- **頻度**: リリース前や新機能追加時に手動実行
- **注意**: 実行時間が長い（5-10分）ため CI には不向き

カバレッジは品質の指標の一つですが、数値だけに囚われず、重要なビジネスロジックが正しくテストされていることを優先しましょう。
