# 多言語対応（i18n）実装計画

**作成日**: 2025-10-06
**対象言語**: 日本語（ja）、英語（en）
**ブランチ**: feat/i18n-internationalization

---

## 📋 実装アーキテクチャ

### ファイル構造

```
lisp-blog/
├── src/
│   ├── i18n.lisp          (新規) - i18nコアシステム
│   └── locales/           (新規) - 翻訳ファイルディレクトリ
│       ├── ja.lisp        - 日本語翻訳
│       └── en.lisp        - 英語翻訳
├── static/js/
│   └── i18n.js           (新規) - フロントエンドi18nヘルパー
└── lisp-blog.asd         (更新) - システム定義に追加
```

### 設計方針: シンプルなハッシュテーブルベース

**採用理由**:
- ✅ 外部ライブラリ不要（依存関係を増やさない）
- ✅ Common Lispの標準機能のみ使用
- ✅ 学習プロジェクトに最適
- ✅ 拡張性が高い（後から言語追加が容易）
- ✅ パフォーマンスが良い（ハッシュテーブル検索はO(1)）

**非採用案**:
- ❌ cl-locale: 外部依存が増える、オーバースペック
- ❌ gettext: C依存、Common Lispらしくない
- ❌ Vue I18n: フロントエンド用ライブラリ、重い

---

## 🏗️ バックエンド設計（Common Lisp）

### src/i18n.lisp - コアシステム

```lisp
;;;; i18n.lisp - Internationalization support
;;;; Simple hash-table based translation system

(in-package :lisp-blog)

;;; グローバル変数
(defparameter *current-locale* :ja
  "現在のロケール（デフォルト: 日本語）")

(defparameter *translations* (make-hash-table :test 'equal)
  "全翻訳を格納するハッシュテーブル
   キー形式: 'locale.translation-key' (例: 'ja.login.title')")

(defparameter *supported-locales* '(:ja :en)
  "サポートする言語のリスト")

;;; 翻訳関数
(defun t! (key &optional (locale *current-locale*))
  "翻訳を取得する
   引数:
     key - 翻訳キー (例: 'login.title')
     locale - ロケール（省略時は *current-locale* を使用）
   戻り値:
     翻訳されたテキスト（見つからない場合はキーをそのまま返す）"
  (let ((full-key (format nil "~A.~A" locale key)))
    (gethash full-key *translations* key)))

(defun set-translation (locale key value)
  "翻訳を設定する
   引数:
     locale - ロケール (:ja, :en等)
     key - 翻訳キー
     value - 翻訳テキスト"
  (let ((full-key (format nil "~A.~A" locale key)))
    (setf (gethash full-key *translations*) value)))

(defun load-translations ()
  "全ての翻訳ファイルをロード"
  (dolist (locale *supported-locales*)
    (let ((file (merge-pathnames
                  (format nil "src/locales/~A.lisp" locale)
                  *project-root*)))
      (when (probe-file file)
        (load file)
        (format t "Loaded translations: ~A~%" locale)))))

(defun get-locale-from-cookie ()
  "クッキーからロケールを取得"
  (let ((locale-str (cookie-in "locale")))
    (when locale-str
      (intern (string-upcase locale-str) :keyword))))

(defun set-current-locale-from-request ()
  "リクエストから現在のロケールを設定
   優先順位: クッキー > Accept-Language > デフォルト"
  (let ((locale (or (get-locale-from-cookie)
                    *current-locale*)))
    (when (member locale *supported-locales*)
      (setf *current-locale* locale))))
```

### src/locales/ja.lisp - 日本語翻訳

```lisp
;;;; ja.lisp - Japanese translations

(in-package :lisp-blog)

;;; 共通
(set-translation :ja "common.login" "ログイン")
(set-translation :ja "common.logout" "ログアウト")
(set-translation :ja "common.signup" "新規登録")
(set-translation :ja "common.cancel" "キャンセル")
(set-translation :ja "common.save" "保存")
(set-translation :ja "common.delete" "削除")
(set-translation :ja "common.edit" "編集")

;;; ログインページ
(set-translation :ja "login.title" "ログイン - Common Lisp Blog")
(set-translation :ja "login.brand" "MyBlog")
(set-translation :ja "login.subtitle" "ログインしてブログを書こう")
(set-translation :ja "login.username" "ユーザー名")
(set-translation :ja "login.password" "パスワード")
(set-translation :ja "login.button" "ログイン")
(set-translation :ja "login.no-account" "アカウントをお持ちでない方は")
(set-translation :ja "login.error.invalid" "ユーザー名またはパスワードが正しくありません")

;;; サインアップページ
(set-translation :ja "signup.title" "新規登録 - Common Lisp Blog")
(set-translation :ja "signup.subtitle" "アカウントを作成")
(set-translation :ja "signup.email" "メールアドレス")
(set-translation :ja "signup.display-name" "表示名 (任意)")
(set-translation :ja "signup.button" "アカウント作成")
(set-translation :ja "signup.has-account" "すでにアカウントをお持ちの方は")
(set-translation :ja "signup.success" "アカウントが作成されました！")

;;; メインページ
(set-translation :ja "main.title" "Common Lisp Blog - Modern Admin")
(set-translation :ja "main.new-post" "新規作成")
(set-translation :ja "main.back-to-list" "一覧に戻る")
(set-translation :ja "main.posts-list" "記事一覧")
(set-translation :ja "main.new-article" "新規作成")

;;; 投稿
(set-translation :ja "post.title-placeholder" "魅力的なタイトルを入力...")
(set-translation :ja "post.content-placeholder" "ここから書き始めましょう...")
(set-translation :ja "post.status.draft" "下書き")
(set-translation :ja "post.status.published" "公開済み")
(set-translation :ja "post.publish" "公開する")
(set-translation :ja "post.save-draft" "下書き保存")
(set-translation :ja "post.unpublish" "下書きに戻す")

;;; エラーメッセージ
(set-translation :ja "error.not-logged-in" "ログインが必要です")
(set-translation :ja "error.invalid-credentials" "ユーザー名またはパスワードが正しくありません")
(set-translation :ja "error.server-error" "サーバーエラーが発生しました")
```

### src/locales/en.lisp - 英語翻訳

```lisp
;;;; en.lisp - English translations

(in-package :lisp-blog)

;;; Common
(set-translation :en "common.login" "Login")
(set-translation :en "common.logout" "Logout")
(set-translation :en "common.signup" "Sign Up")
(set-translation :en "common.cancel" "Cancel")
(set-translation :en "common.save" "Save")
(set-translation :en "common.delete" "Delete")
(set-translation :en "common.edit" "Edit")

;;; Login page
(set-translation :en "login.title" "Login - Common Lisp Blog")
(set-translation :en "login.brand" "MyBlog")
(set-translation :en "login.subtitle" "Login to write your blog")
(set-translation :en "login.username" "Username")
(set-translation :en "login.password" "Password")
(set-translation :en "login.button" "Login")
(set-translation :en "login.no-account" "Don't have an account?")
(set-translation :en "login.error.invalid" "Invalid username or password")

;;; Signup page
(set-translation :en "signup.title" "Sign Up - Common Lisp Blog")
(set-translation :en "signup.subtitle" "Create an account")
(set-translation :en "signup.email" "Email")
(set-translation :en "signup.display-name" "Display Name (optional)")
(set-translation :en "signup.button" "Create Account")
(set-translation :en "signup.has-account" "Already have an account?")
(set-translation :en "signup.success" "Account created successfully!")

;;; Main page
(set-translation :en "main.title" "Common Lisp Blog - Modern Admin")
(set-translation :en "main.new-post" "New Post")
(set-translation :en "main.back-to-list" "Back to List")
(set-translation :en "main.posts-list" "Posts")
(set-translation :en "main.new-article" "New Article")

;;; Post
(set-translation :en "post.title-placeholder" "Enter a catchy title...")
(set-translation :en "post.content-placeholder" "Start writing here...")
(set-translation :en "post.status.draft" "Draft")
(set-translation :en "post.status.published" "Published")
(set-translation :en "post.publish" "Publish")
(set-translation :en "post.save-draft" "Save Draft")
(set-translation :en "post.unpublish" "Unpublish")

;;; Error messages
(set-translation :en "error.not-logged-in" "Login required")
(set-translation :en "error.invalid-credentials" "Invalid username or password")
(set-translation :en "error.server-error" "Server error occurred")
```

---

## 🎨 フロントエンド設計（JavaScript）

### static/js/i18n.js - シンプルなi18nヘルパー

```javascript
/**
 * シンプルなi18nヘルパー
 * Vue I18nを使わず、軽量な独自実装
 */

const i18n = {
  // 現在のロケール（クッキーから取得）
  locale: getCookie('locale') || 'ja',

  // 翻訳メッセージ
  messages: {
    ja: {
      // ログイン
      'login.error.invalid': 'ユーザー名またはパスワードが正しくありません',
      'login.error.server': 'サーバーエラーが発生しました',

      // サインアップ
      'signup.success': 'アカウントが作成されました！',
      'signup.error.exists': 'ユーザー名またはメールアドレスが既に使用されています',

      // 投稿
      'post.created': '投稿が作成されました！',
      'post.updated': '投稿が更新されました！',
      'post.deleted': '投稿が削除されました',
      'post.published': '下書きが公開されました！',
      'post.unpublished': '投稿が下書きに戻されました！',

      // バリデーション
      'validation.required': 'タイトルと内容を入力してください',
      'validation.confirm-delete': '本当に削除しますか？',

      // その他
      'auth.logged-out': 'ログアウトしました'
    },

    en: {
      // Login
      'login.error.invalid': 'Invalid username or password',
      'login.error.server': 'Server error occurred',

      // Signup
      'signup.success': 'Account created successfully!',
      'signup.error.exists': 'Username or email already exists',

      // Post
      'post.created': 'Post created!',
      'post.updated': 'Post updated!',
      'post.deleted': 'Post deleted',
      'post.published': 'Draft published!',
      'post.unpublished': 'Post unpublished!',

      // Validation
      'validation.required': 'Please enter title and content',
      'validation.confirm-delete': 'Are you sure you want to delete?',

      // Other
      'auth.logged-out': 'Logged out'
    }
  },

  /**
   * 翻訳を取得
   * @param {string} key - 翻訳キー
   * @returns {string} 翻訳されたテキスト
   */
  t(key) {
    return this.messages[this.locale][key] || key;
  },

  /**
   * ロケールを切り替え
   * @param {string} locale - 'ja' or 'en'
   */
  switchLocale(locale) {
    this.locale = locale;
    setCookie('locale', locale, 365); // 1年間有効
    location.reload(); // ページをリロードして反映
  }
};

/**
 * クッキーを取得
 */
function getCookie(name) {
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) return parts.pop().split(';').shift();
}

/**
 * クッキーを設定
 */
function setCookie(name, value, days) {
  const expires = new Date(Date.now() + days * 864e5).toUTCString();
  document.cookie = `${name}=${encodeURIComponent(value)}; expires=${expires}; path=/`;
}

// グローバルに公開
window.i18n = i18n;
```

---

## 🔄 ハンドラー更新例

### handlers.lisp の変更前後

**変更前**:
```lisp
(define-easy-handler (login-page :uri "/login") ()
  (setf (content-type*) "text/html")
  (with-html-string
    (:title "ログイン - Common Lisp Blog")
    (:h1 "MyBlog")
    (:p "ログインしてブログを書こう")))
```

**変更後**:
```lisp
(define-easy-handler (login-page :uri "/login") ()
  (set-current-locale-from-request) ; ロケール設定
  (setf (content-type*) "text/html")
  (with-html-string
    (:title (t! "login.title"))
    (:h1 (t! "login.brand"))
    (:p (t! "login.subtitle"))))
```

---

## 🎯 実装ステップ

### フェーズ1: バックエンドi18nシステム構築
- [ ] `src/i18n.lisp` 作成
  - [ ] ハッシュテーブルベースの翻訳システム
  - [ ] `t!` 関数実装
  - [ ] クッキーからロケール検出
  - [ ] デフォルトロケール設定
- [ ] `lisp-blog.asd` にファイル追加

### フェーズ2: 翻訳ファイル作成
- [ ] `src/locales/` ディレクトリ作成
- [ ] `src/locales/ja.lisp` 作成
  - [ ] 共通翻訳
  - [ ] ログインページ翻訳
  - [ ] サインアップページ翻訳
  - [ ] メインページ翻訳
  - [ ] 投稿関連翻訳
  - [ ] エラーメッセージ翻訳
- [ ] `src/locales/en.lisp` 作成（同上）

### フェーズ3: ハンドラー更新
- [ ] `handlers.lisp` 修正
  - [ ] ログインページ
  - [ ] サインアップページ
  - [ ] メインページ
  - [ ] 全てのハードコード文字列を `t!` に置き換え

### フェーズ4: フロントエンドi18n
- [ ] `static/js/i18n.js` 作成
- [ ] `static/js/login.js` 更新
  - [ ] エラーメッセージを `i18n.t()` に置き換え
- [ ] `static/js/app.js` 更新
  - [ ] アラート、メッセージを `i18n.t()` に置き換え

### フェーズ5: 言語切り替えUI
- [ ] ヘッダーに言語切り替えボタン追加
  - [ ] 「日本語 / English」トグル
  - [ ] クッキーに言語設定を保存
  - [ ] ページリロードで反映
- [ ] 現在の言語を視覚的に表示

### フェーズ6: テストと検証
- [ ] 全ページを日本語で確認
- [ ] 全ページを英語で確認
- [ ] 言語切り替え動作確認
- [ ] クッキー永続化確認
- [ ] エラーメッセージ確認

---

## 📊 翻訳対象テキストの抽出リスト

### サーバーサイド（handlers.lisp）
- ページタイトル（`<title>`タグ）
- ブランド名、サブタイトル
- ボタンテキスト（Login, Sign Up, Logout等）
- フォームラベル（Username, Password等）
- リンクテキスト

### クライアントサイド（JavaScript）
- アラートメッセージ
- エラーメッセージ
- 成功メッセージ
- 確認ダイアログ

---

## 🚀 期待される成果

### 機能面
- ✅ 日本語・英語の完全サポート
- ✅ ユーザーが自由に言語切り替え可能
- ✅ 言語設定の永続化（クッキー）
- ✅ 新機能追加時の多言語対応が容易

### 技術面
- ✅ 外部依存なしのシンプルな実装
- ✅ Common Lispらしいコード
- ✅ パフォーマンスへの影響最小
- ✅ 拡張性の高いアーキテクチャ

### 学習面
- ✅ 国際化（i18n）の実践経験
- ✅ ハッシュテーブルの活用
- ✅ クッキー処理の理解
- ✅ 多言語Webアプリケーション開発スキル

---

## 🔍 将来の拡張案

### 第3言語の追加（例: 中国語）
1. `src/locales/zh.lisp` 作成
2. `*supported-locales*` に `:zh` 追加
3. 翻訳を設定

### より高度な機能
- 複数形対応（英語: 1 post / 2 posts）
- 日付フォーマットのロケール対応
- 数値フォーマットのロケール対応
- Accept-Language ヘッダーの自動検出

---

**実装担当**: Claude Code
**レビュー**: ユーザー
**完了予定**: 2025-10-06
