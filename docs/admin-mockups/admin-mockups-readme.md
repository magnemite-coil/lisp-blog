# ブログ管理画面 モックアップ

このディレクトリには、ブログシステムの管理者画面（CMS）のデザインモックアップが含まれています。

## 📋 概要

- **目的**: 管理画面のUX検討、デザインパターンの比較、実装前の機能確認
- **形式**: Pure HTML/CSS（JavaScriptは最小限）
- **想定用途**: 最終的にはVue.jsやReactで動的に実装するが、ここではモックとして静的HTMLで出力

## 🎨 モックアップ一覧

全5種類の異なるレイアウトパターンを用意しています。

| # | ファイル名 | レイアウト名 | 特徴 | 適用シーン |
|---|-----------|------------|------|-----------|
| 1 | `admin-classic-sidebar.html` | Classic Sidebar | サイドバーナビゲーション型の定番レイアウト | 多機能なCMS、管理項目が多い場合 |
| 2 | `admin-topbar.html` | Top Bar Navigation | トップバーでタブ切り替えする現代的なUI | シンプルな管理画面、モバイル対応重視 |
| 3 | `admin-focus-mode.html` | Focus Mode | 執筆に集中できるミニマルデザイン | 長文記事の執筆、気が散らない環境が必要な場合 |
| 4 | `admin-split-view.html` | Split View | エディタとプレビューを同時表示 | リアルタイムプレビューが必要、Markdown編集 |
| 5 | `admin-card-layout.html` | Card Layout | カード型の視覚的なレイアウト | モダンなUI、視覚的な分かりやすさ重視 |

## 🔍 各レイアウトの詳細

### 1. Classic Sidebar Layout
**配色**: ダークブルー系（`#1a1d2e`）  
**特徴**:
- 左サイドバーに固定ナビゲーション
- 右サイドに設定パネル
- 中央にメインエディタ
- 3カラムレイアウト

**メリット**:
- 多くの機能に素早くアクセス可能
- 一般的なレイアウトで学習コストが低い
- 設定項目を常時表示できる

**デメリット**:
- 画面が狭く感じる場合がある
- モバイル対応が複雑

**推奨用途**: WordPress風の多機能CMS

---

### 2. Top Bar Navigation
**配色**: ダークグレー系（`#0d1117`）  
**特徴**:
- トップバーでタブ切り替え
- フラットな2カラムレイアウト
- GitHubライクなUI

**メリット**:
- 画面を広く使える
- モダンでスッキリした見た目
- レスポンシブ対応しやすい

**デメリット**:
- ナビゲーション項目が多いと収まらない
- タブ切り替えが必要

**推奨用途**: 中規模のCMS、開発者向けツール

---

### 3. Focus Mode
**配色**: ピュアブラック系（`#0f0f0f`）  
**特徴**:
- 極限までシンプル化
- フローティングUIで邪魔にならない
- 執筆に集中できる設計

**メリット**:
- 気が散らない
- 没入感が高い
- 美しく洗練された見た目

**デメリット**:
- 設定変更がやや手間
- 初心者には分かりにくい可能性

**推奨用途**: ライター向け、長文コンテンツ作成

---

### 4. Split View
**配色**: パープル系（`#1e1e2e`）  
**特徴**:
- 左にエディタ、右にリアルタイムプレビュー
- Markdownエディタに最適
- デバイスプレビュー切り替え

**メリット**:
- 書いた内容をすぐ確認できる
- Markdownの学習に最適
- レスポンシブ確認が容易

**デメリット**:
- 画面が分割されるため1つあたりが狭い
- 処理が重くなる可能性

**推奨用途**: 技術ブログ、Markdown重視のCMS

---

### 5. Card Layout
**配色**: ディープブラック系（`#0a0a0f`）  
**特徴**:
- カード型のモジュラーUI
- グラデーションアクセント
- 視覚的に分かりやすい

**メリット**:
- 直感的で分かりやすい
- モダンで魅力的なデザイン
- 各機能が独立している

**デメリット**:
- 縦に長くなりがち
- スクロールが多くなる

**推奨用途**: 初心者向け、ビジュアル重視のCMS

## ✨ 共通機能

すべてのモックアップは以下の機能を実装しています：

### 必須機能
- ✅ **タイトル入力**: 記事タイトルの入力欄
- ✅ **本文エディタ**: Markdown形式での執筆
- ✅ **フォーマットツールバー**: 太字、見出し、リンク、画像、コードブロックなど
- ✅ **下書き保存**: 非公開での保存機能
- ✅ **公開設定**: 即時公開、下書き、予約投稿の選択
- ✅ **公開日時指定**: datetime-localによる日時選択
- ✅ **カテゴリー選択**: プルダウンでカテゴリーを選択
- ✅ **タグ管理**: 複数タグの追加・削除
- ✅ **アイキャッチ画像**: 画像アップロードエリア

### 追加機能
- 📊 文字数カウント（一部）
- 🎨 シンタックスハイライト対応
- 📱 レスポンシブ対応の基本実装
- 🖼️ 画像・動画の挿入機能
- 💾 自動保存の表示（一部）

## 🖥️ プレビュー方法

### ローカル環境で確認

#### 方法1: シンプルHTTPサーバー
```bash
npx serve docs/admin-mockups
```
または
```bash
cd docs/admin-mockups
python -m http.server 8000
```

#### 方法2: 直接ブラウザで開く
各HTMLファイルをダブルクリックして直接ブラウザで開く

#### 方法3: VS Code Live Server
VS Codeの拡張機能「Live Server」を使用

### オンラインプレビュー
GitHub Pagesにデプロイした場合：
```
https://[username].github.io/[repository]/admin-mockups/
```

## 🎯 デザインシステム

### カラーパレット

各モックアップは統一されたダークモード配色を採用：

```css
/* 基本色 */
--bg-primary: #暗い背景;
--bg-secondary: #やや明るい背景;
--text-primary: #明るいテキスト;
--text-secondary: #やや暗いテキスト;

/* アクセントカラー */
--accent-primary: #メインアクセント;
--success: #成功色;
--warning: #警告色;
--danger: #エラー色;
```

### タイポグラフィ

- **システムフォント**: -apple-system, BlinkMacSystemFont, 'Segoe UI'
- **コード用フォント**: Menlo, Monaco, 'Courier New', monospace
- **見出し**: 大きめ（2.5rem - 3.5rem）、太字
- **本文**: 読みやすいサイズ（0.9rem - 1rem）

### スペーシング

- カード間の余白: 20-30px
- 内部パディング: 15-30px
- フォーム要素の余白: 10-15px

## 🔧 技術仕様

### 使用技術
- **HTML5**: セマンティックなマークアップ
- **CSS3**: CSS Grid, Flexbox, CSS変数
- **JavaScript**: 最小限（モック用の簡単なインタラクションのみ）

### レスポンシブ対応
```css
/* 基本的なブレイクポイント */
- Desktop: 1024px以上
- Tablet: 768px - 1023px
- Mobile: 767px以下
```

### コンポーネント構造

#### ボタン
```html
<button class="btn btn-primary">公開</button>
<button class="btn btn-secondary">下書き保存</button>
```

#### フォーム要素
```html
<input type="text" class="form-input">
<select class="form-select">...</select>
<textarea class="form-textarea">...</textarea>
```

#### カード
```html
<div class="card">
  <div class="card-header">...</div>
  <div class="card-body">...</div>
</div>
```

## 📝 実装への反映

### フェーズ1: コンポーネント設計
モックアップから以下のコンポーネントを抽出：

```
components/
├── Editor/
│   ├── TitleInput.vue
│   ├── ContentEditor.vue
│   └── Toolbar.vue
├── Sidebar/
│   ├── PublishSettings.vue
│   ├── CategorySelector.vue
│   └── TagInput.vue
└── Common/
    ├── Button.vue
    ├── Card.vue
    └── FormInput.vue
```

### フェーズ2: 状態管理
```javascript
// Vuex/Pinia ストア構造例
{
  post: {
    title: '',
    content: '',
    status: 'draft', // draft | published | scheduled
    publishedAt: null,
    category: null,
    tags: [],
    featuredImage: null
  }
}
```

### フェーズ3: API統合
```javascript
// API エンドポイント例
POST   /api/posts           // 記事作成
PUT    /api/posts/:id       // 記事更新
GET    /api/posts/:id       // 記事取得
DELETE /api/posts/:id       // 記事削除
POST   /api/media           // メディアアップロード
```

## 🚀 実装時の考慮事項

### 必須実装
- [ ] リアルタイム自動保存機能
- [ ] Markdownプレビュー（Marked.js、markdown-itなど）
- [ ] シンタックスハイライト（Prism.js、highlight.jsなど）
- [ ] 画像アップロード（ドラッグ&ドロップ対応）
- [ ] バリデーション（タイトル必須など）
- [ ] 保存前の確認ダイアログ

### 推奨実装
- [ ] ショートカットキー（Ctrl+S で保存など）
- [ ] 編集履歴（Undo/Redo）
- [ ] 下書き一覧からの復元
- [ ] SEOプレビュー
- [ ] 文字数・読了時間の自動計算
- [ ] タグのオートコンプリート

### セキュリティ
- [ ] CSRF対策
- [ ] XSS対策（サニタイゼーション）
- [ ] 認証・認可
- [ ] ファイルアップロードの制限

## ⚠️ 注意事項

### モックアップの制限
- ❌ 実際のデータ保存機能はありません
- ❌ Markdownの実際のパースは行われていません
- ❌ 画像アップロードは動作しません
- ❌ プレビュー機能は静的な表示のみ

### 実装時の推奨ライブラリ

#### Markdownエディタ
- **Toast UI Editor** - 高機能なWYSIWYGエディタ
- **SimpleMDE** - シンプルなMarkdownエディタ
- **CodeMirror** - カスタマイズ性の高いエディタ

#### シンタックスハイライト
- **Prism.js** - 軽量で多言語対応
- **highlight.js** - 自動言語検出

#### 画像アップロード
- **Dropzone.js** - ドラッグ&ドロップ対応
- **Uppy** - モダンなファイルアップローダー

#### 日時選択
- **Flatpickr** - 軽量な日時ピッカー
- **Vue Datepicker** - Vue専用

## 🎨 カスタマイズ方法

### カラーテーマの変更

CSS変数を変更するだけで簡単にカスタマイズ可能：

```css
:root {
  /* 例: ブルー系に変更 */
  --accent-primary: #3b82f6;
  --accent-hover: #2563eb;
}
```

### レイアウトの調整

Grid/Flexboxのパラメータを調整：

```css
/* サイドバーの幅を変更 */
.sidebar {
  width: 400px; /* デフォルト: 300px */
}

/* グリッドレイアウトの比率を変更 */
.grid {
  grid-template-columns: 2fr 1fr; /* デフォルト: 1fr 350px */
}
```

## 📚 参考リンク

- [Notion](https://www.notion.so/) - モダンなエディタUI
- [Medium](https://medium.com/) - シンプルな執筆体験
- [Ghost](https://ghost.org/) - 美しいCMSデザイン
- [WordPress Gutenberg](https://wordpress.org/gutenberg/) - ブロックエディタ

## 🔄 バージョン履歴

- **v1.0** (2025-10-04) - 初回リリース、5つのレイアウトパターン

---

**作成日**: 2025年10月4日  
**最終更新**: 2025年10月4日  
**作成者**: Claude (Anthropic)  
**ライセンス**: プロジェクト内での使用に限定
