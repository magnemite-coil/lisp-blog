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
(set-translation :ja "common.sign-up" "Sign Up")

;;; ログインページ
(set-translation :ja "login.page-title" "ログイン - Common Lisp Blog")
(set-translation :ja "login.brand" "MyBlog")
(set-translation :ja "login.subtitle" "ログインしてブログを書こう")
(set-translation :ja "login.username" "ユーザー名")
(set-translation :ja "login.password" "パスワード")
(set-translation :ja "login.button" "ログイン")
(set-translation :ja "login.no-account" "アカウントをお持ちでない方は")
(set-translation :ja "login.logo-text" "Common Lisp Blog")
(set-translation :ja "login.error.invalid" "ユーザー名またはパスワードが正しくありません")

;;; サインアップページ
(set-translation :ja "signup.page-title" "新規登録 - Common Lisp Blog")
(set-translation :ja "signup.brand" "MyBlog")
(set-translation :ja "signup.subtitle" "アカウントを作成")
(set-translation :ja "signup.username" "ユーザー名 (3-50文字)")
(set-translation :ja "signup.email" "メールアドレス")
(set-translation :ja "signup.password" "パスワード (8文字以上)")
(set-translation :ja "signup.display-name" "表示名 (任意)")
(set-translation :ja "signup.button" "アカウント作成")
(set-translation :ja "signup.has-account" "すでにアカウントをお持ちの方は")
(set-translation :ja "signup.success" "アカウントが作成されました！")
(set-translation :ja "signup.login-btn-header" "ログイン")

;;; メインページ
(set-translation :ja "main.page-title" "Common Lisp Blog - Modern Admin")
(set-translation :ja "main.breadcrumb.posts" "記事")
(set-translation :ja "main.breadcrumb.new" "新規作成")
(set-translation :ja "main.breadcrumb.list" "記事一覧")
(set-translation :ja "main.new-post" "新規作成")
(set-translation :ja "main.back-to-list" "一覧に戻る")
(set-translation :ja "main.logout" "Logout")

;;; 投稿エディタ
(set-translation :ja "editor.title-placeholder" "魅力的なタイトルを入力...")
(set-translation :ja "editor.content-label" "本文")
(set-translation :ja "editor.content-placeholder" "ここから書き始めましょう...

Markdownで自由に執筆できます。

# 見出し
## サブ見出し

**太字** や *斜体* も使えます。")

;;; 公開設定
(set-translation :ja "status.title" "公開設定")
(set-translation :ja "status.draft" "下書き")
(set-translation :ja "status.draft.desc" "非公開で保存")
(set-translation :ja "status.published" "公開")
(set-translation :ja "status.published.desc" "すぐに公開する")
(set-translation :ja "status.save-draft" "下書き保存")
(set-translation :ja "status.publish" "公開する")

;;; 統計
(set-translation :ja "stats.title" "統計")
(set-translation :ja "stats.word-count" "文字数")
(set-translation :ja "stats.read-time" "分で読める")

;;; 投稿一覧
(set-translation :ja "post.status.draft" "下書き")
(set-translation :ja "post.status.published" "公開済み")
(set-translation :ja "post.edit" "編集")
(set-translation :ja "post.publish-action" "公開")
(set-translation :ja "post.unpublish" "下書きに戻す")
(set-translation :ja "post.delete" "削除")

;;; エラーメッセージ
(set-translation :ja "error.not-logged-in" "ログインが必要です")
(set-translation :ja "error.invalid-credentials" "ユーザー名またはパスワードが正しくありません")
(set-translation :ja "error.server-error" "サーバーエラーが発生しました")

;;; API メッセージ - 認証
(set-translation :ja "api.auth.account-created" "アカウントが正常に作成されました")
(set-translation :ja "api.auth.login-success" "ログインに成功しました")
(set-translation :ja "api.auth.invalid-credentials" "ユーザー名またはパスワードが正しくありません")
(set-translation :ja "api.auth.logout-success" "ログアウトしました")
(set-translation :ja "api.auth.login-required" "ログインが必要です")

;;; API メッセージ - 投稿
(set-translation :ja "api.post.created" "投稿が正常に作成されました")
(set-translation :ja "api.post.updated" "投稿が正常に更新されました")
(set-translation :ja "api.post.deleted" "投稿が削除されました")
(set-translation :ja "api.post.published" "下書きが公開されました")
(set-translation :ja "api.post.unpublished" "投稿が下書きに戻されました")
(set-translation :ja "api.post.permission-denied" "この投稿を編集する権限がありません")
(set-translation :ja "api.post.not-found" "投稿が見つかりません")
(set-translation :ja "api.post.draft-not-found" "下書きが見つからないか、権限がありません")
(set-translation :ja "api.post.missing-id" "投稿IDが必要です")
(set-translation :ja "api.post.invalid-id" "無効な投稿IDです")

;;; バリデーションエラー
(set-translation :ja "validation.username.format" "ユーザー名は3-50文字の英数字、アンダースコア、ハイフンである必要があります")
(set-translation :ja "validation.username.taken" "このユーザー名は既に使用されています")
(set-translation :ja "validation.email.format" "有効なメールアドレス形式ではありません")
(set-translation :ja "validation.email.taken" "このメールアドレスは既に使用されています")
(set-translation :ja "validation.password.length" "パスワードは8-100文字である必要があります")
