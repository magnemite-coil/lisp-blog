# ブログ管理画面 モックアップ（追加版）

このディレクトリには、ブログシステムの管理者画面（CMS）の追加デザインモックアップが含まれています。

## 📋 概要

- **目的**: 多様なUIパターンの検討、国際化対応の確認、実装前の機能検証
- **形式**: Pure HTML/CSS（JavaScriptは最小限）
- **特徴**: 全てのモックアップに英語UI + 日本語/英語の二言語表示を実装
- **想定用途**: 最終的にはVue.jsやReactで動的に実装するが、ここではモックとして静的HTMLで出力

## 🎨 追加モックアップ一覧

全5種類の新しいレイアウトパターンを用意しています。

| # | ファイル名 | レイアウト名 | 特徴 | 適用シーン |
|---|-----------|------------|------|-----------|
| 6 | `admin-kanban-board.html` | Kanban Board Style | カンバンボードで記事管理 | プロジェクト管理風、ワークフロー重視 |
| 7 | `admin-zen-mode.html` | Minimalist Zen Mode | 究極のミニマルデザイン | 執筆に完全集中、気が散る要素ゼロ |
| 8 | `admin-multi-column.html` | Multi-Column Dashboard | 3カラムの情報密度高いUI | パワーユーザー向け、効率重視 |
| 9 | `admin-mobile-first.html` | Mobile-First Design | スマホ最適化レイアウト | モバイル執筆、外出先での更新 |
| 10 | `admin-timeline-editor.html` | Timeline Editor | ストーリーテリング型エディタ | 長編記事、ブロック単位の編集 |

## 🔍 各レイアウトの詳細

### 6. Kanban Board Style
**UI言語**: 英語  
**コンテンツ**: 日本語・英語バイリンガル  
**配色**: ダークパープル系（`#1c1c28`）

**特徴**:
- Trello/Notionライクなカンバンボード
- ドラフト → レビュー → 予約 → 公開のワークフロー
- カード形式で記事を一覧表示
- モーダルウィンドウでフルエディタ展開

**メリット**:
- ワークフローが視覚的に分かりやすい
- 複数の記事の状態を一目で把握
- チーム作業に最適

**デメリット**:
- 1記事の執筆には不向き
- 横スクロールが必要

**推奨用途**: 編集チーム向け、複数記事の進捗管理

**実装例**:
```html
<!-- カンバンカラム構造 -->
<div class="kanban-container">
  <div class="kanban-column">Draft</div>
  <div class="kanban-column">In Review</div>
  <div class="kanban-column">Scheduled</div>
  <div class="kanban-column">Published</div>
</div>
```

---

### 7. Minimalist Zen Mode
**UI言語**: 英語  
**コンテンツ**: 日本語・英語バイリンガル  
**配色**: ピュアブラック（`#0f0f0f`）

**特徴**:
- 極限まで削ぎ落としたUI
- フローティングメニュー（マウスホバーで表示）
- スライドインの設定パネル
- Georgiaセリフフォントで執筆体験向上

**メリット**:
- 執筆に完全集中できる
- 美しく洗練された見た目
- 没入感が非常に高い

**デメリット**:
- 設定変更にステップが必要
- 初めて使う人には分かりにくい

**推奨用途**: プロライター、小説家、長文コンテンツ作成

**CSS特徴**:
```css
/* フローティングUI */
.menu-bar {
  opacity: 0.3;
  transition: opacity 0.3s;
}
.menu-bar:hover {
  opacity: 1;
}
```

---

### 8. Multi-Column Dashboard
**UI言語**: 英語  
**コンテンツ**: 日本語・英語バイリンガル  
**配色**: ダークグレー系（`#111827`）

**特徴**:
- 左：ナビゲーション、中央：エディタ、右：プロパティの3カラム
- 情報密度が高い
- 全ての機能に素早くアクセス可能

**メリット**:
- 画面遷移なしで全機能使用可能
- パワーユーザーに最適
- 大画面で真価を発揮

**デメリット**:
- 小さい画面では使いにくい
- 初心者には圧倒される可能性

**推奨用途**: デスクトップ作業、プロフェッショナルユーザー

**グリッドレイアウト**:
```css
.dashboard {
  display: grid;
  grid-template-columns: 300px 1fr 350px;
}
```

---

### 9. Mobile-First Design
**UI言語**: 英語  
**コンテンツ**: 日本語・英語バイリンガル  
**配色**: ネイビー系（`#0f172a`）

**特徴**:
- スマートフォン画面最適化
- カード型のセクション分割
- タッチ操作に最適化されたボタンサイズ
- 固定ボトムバーでアクション実行

**メリット**:
- 外出先で記事更新可能
- 片手操作しやすい
- レスポンシブ対応の基準になる

**デメリット**:
- デスクトップでは空白が多い
- 長文執筆には不向き

**推奨用途**: モバイル執筆、簡単な更新作業

**モバイル最適化**:
```css
/* タッチターゲット最適化 */
.quick-btn {
  padding: 10px 16px; /* 44px以上推奨 */
}

/* 固定ボトムバー */
.bottom-action-bar {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
}
```

---

### 10. Timeline Editor
**UI言語**: 英語  
**コンテンツ**: 日本語・英語バイリンガル  
**配色**: ダークグレー系（`#18181b`）

**特徴**:
- 左サイドバーにブロック一覧をタイムライン表示
- ブロック単位での編集（テキスト、コード、引用、画像など）
- ストーリーテリングに最適
- Medium/Notionライクなブロックエディタ

**メリット**:
- 長編記事の構造が分かりやすい
- ブロックの並び替えが直感的
- 各ブロックに個別設定可能

**デメリット**:
- 短い記事にはオーバースペック
- 学習コストがやや高い

**推奨用途**: 長編記事、チュートリアル、ストーリー形式のコンテンツ

**ブロック構造**:
```javascript
// ブロックのデータ構造例
{
  id: 1,
  type: 'code',
  content: '...',
  metadata: {
    language: 'javascript',
    theme: 'dracula'
  }
}
```

## ✨ 共通機能（全モックアップ）

### 国際化対応
- ✅ **言語切り替え**: EN ⇄ 日本語
- ✅ **UI言語**: すべて英語表記
- ✅ **コンテンツ**: 日本語・英語の両方を例示
- ✅ **引用**: 日英両方の引用文
- ✅ **コードコメント**: 日英バイリンガル

### 基本機能
- ✅ **Markdownエディタ**: 全モックアップ対応
- ✅ **シンタックスハイライト**: コードブロック例示
- ✅ **画像/動画アップロード**: アップロードエリア実装
- ✅ **下書き保存**: ステータス管理
- ✅ **公開日時指定**: datetime-local入力
- ✅ **カテゴリー・タグ**: 選択・追加機能

### ダークモードデザイン
すべてのモックアップが統一されたダークモードデザイン：
- 目に優しい暗色配色
- 過度なコントラストを避けた配色
- モダンなグラデーションアクセント（一部）

## 🖥️ プレビュー方法

### ローカル環境で確認

#### シンプルHTTPサーバー
```bash
npx serve docs/admin-mockups
```

#### Python
```bash
cd docs/admin-mockups
python -m http.server 8000
```

#### VS Code Live Server
右クリック → "Open with Live Server"

### オンラインプレビュー
```
https://[username].github.io/[repository]/admin-mockups/
```

## 🌍 国際化実装の詳細

### 言語切り替えUI
すべてのモックアップに実装：

```html
<div class="lang-switcher">
  <button class="lang-btn active">EN</button>
  <button class="lang-btn">日本語</button>
</div>
```

### バイリンガルコンテンツの例

#### 見出し
```markdown
## Introduction / はじめに
## Benefits / メリット
```

#### コードコメント
```javascript
// Traditional Promise chain / 従来のPromiseチェーン
fetchUserData(userId)
  .then(user => fetchUserPosts(user.id))
  
// Modern async/await / モダンなasync/await
async function getUserPosts(userId) { ... }
```

#### 引用
```markdown
> "Quote in English"
> 「日本語の引用」
```

## 🎯 デザインシステム

### カラーパレット一覧

| モックアップ | 背景色 | アクセント色 | 特徴 |
|------------|--------|------------|------|
| Kanban Board | `#1c1c28` | `#4a9eff` | クールなブルー |
| Zen Mode | `#09090b` | `#a855f7` | ミステリアスなパープル |
| Multi-Column | `#111827` | `#3b82f6` | クラシックブルー |
| Mobile-First | `#0f172a` | `#06b6d4` | フレッシュシアン |
| Timeline | `#18181b` | `#14b8a6` | トロピカルティール |

### タイポグラフィ階層

```css
/* 見出し */
h1: 2.5rem - 3.5rem
h2: 2rem - 2.8rem
h3: 1.4rem - 1.8rem

/* 本文 */
body: 0.9rem - 1rem
small: 0.75rem - 0.85rem

/* コード */
code: Menlo, Monaco, 'Courier New', monospace
```

## 🔧 技術仕様

### HTML5セマンティックマークアップ
```html
<header>, <nav>, <main>, <aside>, <section>, <article>
```

### CSS3モダン機能
- CSS Grid Layout
- Flexbox
- CSS変数（カスタムプロパティ）
- トランジション・アニメーション
- Backdrop Filter（Zen Mode）

### JavaScript（最小限）
- 設定パネルのトグル
- 言語切り替えのデモ（実装なし）
- モーダル表示/非表示

## 📝 実装への反映

### フェーズ1: 国際化セットアップ

#### Vue I18n
```javascript
// i18n.js
import { createI18n } from 'vue-i18n'

const messages = {
  en: {
    editor: {
      title: 'New Article',
      save: 'Save Draft',
      publish: 'Publish'
    }
  },
  ja: {
    editor: {
      title: '新規記事',
      save: '下書き保存',
      publish: '公開'
    }
  }
}

export default createI18n({
  locale: 'en',
  messages
})
```

#### React i18next
```javascript
import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

i18n
  .use(initReactI18next)
  .init({
    resources: {
      en: { translation: { ... } },
      ja: { translation: { ... } }
    },
    lng: 'en',
    fallbackLng: 'en'
  });
```

### フェーズ2: レイアウト実装

各モックアップのレイアウトをコンポーネント化：

```
components/
├── layouts/
│   ├── KanbanLayout.vue
│   ├── ZenLayout.vue
│   ├── MultiColumnLayout.vue
│   ├── MobileLayout.vue
│   └── TimelineLayout.vue
├── editor/
│   ├── MarkdownEditor.vue
│   ├── BlockEditor.vue
│   └── Toolbar.vue
└── ui/
    ├── LanguageSwitcher.vue
    ├── StatusBadge.vue
    └── MediaUpload.vue
```

### フェーズ3: 状態管理

```javascript
// Vuex/Pinia store
{
  locale: 'en', // 'en' | 'ja'
  layout: 'kanban', // レイアウトタイプ
  currentPost: {
    id: null,
    title: { en: '', ja: '' },
    content: { en: '', ja: '' },
    status: 'draft',
    blocks: [] // Timeline用
  }
}
```

## 🚀 実装時の考慮事項

### 必須実装

#### 国際化
- [ ] i18nライブラリの導入
- [ ] 言語切り替え機能
- [ ] 日時フォーマットのロケール対応
- [ ] RTL対応（将来的に）

#### レイアウト切り替え
- [ ] ユーザー設定で好みのレイアウトを保存
- [ ] レスポンシブでレイアウト自動切り替え
- [ ] ショートカットキーでレイアウト変更

#### モバイル対応
- [ ] タッチイベント対応
- [ ] スワイプジェスチャー
- [ ] バーチャルキーボード対応
- [ ] オフライン編集機能

### 推奨実装

#### Kanban Board
- [ ] ドラッグ&ドロップでカード移動
- [ ] フィルタリング・検索機能
- [ ] バルク操作（複数選択）

#### Zen Mode
- [ ] 全画面モード
- [ ] フォーカスタイマー（Pomodoro）
- [ ] 読み上げ機能

#### Timeline Editor
- [ ] ブロックの並び替え
- [ ] ブロックテンプレート
- [ ] バージョン履歴

## ⚠️ 注意事項

### モックアップの制限
- ❌ 言語切り替えボタンは見た目のみ（実際には切り替わらない）
- ❌ ドラッグ&ドロップは動作しない
- ❌ データの永続化なし
- ❌ 実際のMarkdownパース・プレビューなし

### ブラウザ互換性
- モダンブラウザ推奨（Chrome, Firefox, Safari, Edge最新版）
- CSS Grid, Flexbox必須
- JavaScript ES6+使用

## 📚 参考デザイン

### Kanban Board Style
- [Trello](https://trello.com/) - カンバンボードの定番
- [Notion](https://www.notion.so/) - データベースビュー

### Zen Mode
- [iA Writer](https://ia.net/writer) - 究極のミニマルエディタ
- [Ulysses](https://ulysses.app/) - ディストラクションフリー

### Multi-Column
- [VS Code](https://code.visualstudio.com/) - マルチペイン
- [Sublime Text](https://www.sublimetext.com/) - カラム表示

### Mobile-First
- [Medium Mobile](https://medium.com/) - モバイル執筆
- [Bear](https://bear.app/) - iOS執筆アプリ

### Timeline Editor
- [Notion](https://www.notion.so/) - ブロックエディタ
- [Medium](https://medium.com/) - セクション編集

## 🎨 カスタマイズガイド

### カラーテーマの変更

```css
:root {
  /* お好みの色に変更 */
  --accent: #your-color;
  --bg-primary: #your-bg;
}
```

### レイアウト調整

```css
/* Kanban カラム幅 */
.kanban-column {
  flex: 0 0 400px; /* お好みの幅に */
}

/* Multi-Column 比率 */
.dashboard {
  grid-template-columns: 280px 1fr 320px;
}
```

## 🔄 バージョン履歴

- **v2.0** (2025-10-04) - 追加5レイアウト、国際化対応
- **v1.0** (2025-10-04) - 初回リリース、基本5レイアウト

---

**作成日**: 2025年10月4日  
**最終更新**: 2025年10月4日  
**作成者**: Claude (Anthropic)  
**言語対応**: 英語UI、日本語・英語バイリンガルコンテンツ  
**ライセンス**: プロジェクト内での使用に限定
