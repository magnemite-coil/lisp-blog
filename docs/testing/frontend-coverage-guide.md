# フロントエンドカバレッジガイド（Vitest + @vitest/coverage-v8）

## 概要

このドキュメントでは、Vitest と @vitest/coverage-v8 を使ったフロントエンド（React + TypeScript）コードのカバレッジ測定方法と、レポートの見方を解説します。

**対象**: React + TypeScript フロントエンドコード（frontend/src/ ディレクトリ）

**ツール**: Vitest + @vitest/coverage-v8

**実行時間**: 約2.8秒（通常テストとほぼ同じ）

---

## 1. 実行方法

### 基本的な実行

```bash
cd frontend

# カバレッジレポート付きテスト実行
npm run test:coverage

# カバレッジ + UI モード
npm run test:coverage:ui
```

### 通常テストとの違い

```bash
# 通常のテスト（カバレッジなし）
npm test              # Watch モード
npm run test:run      # 1回のみ実行

# カバレッジ付きテスト
npm run test:coverage # カバレッジレポート生成
```

---

## 2. レポートの構造

### 生成されるファイル

```
frontend/coverage/
├── index.html                    ← メインレポート（★ここから開始）
├── coverage-summary.json         ← JSON サマリー（プログラムで読める）
├── base.css, prettify.css        ← スタイルシート
├── prettify.js, sorter.js        ← JavaScript
└── src/                          ← ソースコード別の詳細レポート
    ├── api/
    │   ├── index.html
    │   ├── auth.ts.html
    │   └── ...
    ├── components/
    ├── hooks/
    ├── pages/
    └── ...
```

### レポートを開く

```bash
# macOS
open coverage/index.html

# Linux
xdg-open coverage/index.html

# Windows
start coverage/index.html
```

---

## 3. index.html の見方

### 画面構成

```
┌────────────────────────────────────────────────────────────────┐
│  All files                        33.27% Statements            │
│                                   86.4%  Branches              │
│                                   48.93% Functions             │
│                                   33.27% Lines                 │
├────────────────────────────────────────────────────────────────┤
│  File               │ %Stmts│ %Branch│ %Funcs│ %Lines│ Uncov. │
├────────────────────────────────────────────────────────────────┤
│  src/               │       │        │       │       │        │
│  └─ App.tsx         │   0   │   0    │   0   │   0   │ 1-50   │
│  src/components/    │       │        │       │       │        │
│  └─ Header.tsx      │  100  │  100   │  100  │  100  │        │ ← 完璧
│  src/hooks/         │       │        │       │       │        │
│  └─ useAuth.tsx     │ 96.51 │ 86.36  │  100  │ 96.51 │ 80,... │ ← 高カバレッジ
│  src/pages/         │       │        │       │       │        │
│  ├─ LoginPage.tsx   │ 99.23 │  100   │  100  │ 99.23 │ 59     │ ← ほぼ完璧
│  ├─ RegisterPage... │ 98.79 │ 96.55  │  100  │ 98.79 │ 32-33  │ ← ほぼ完璧
│  └─ Dashboard...    │   0   │   0    │   0   │   0   │ 1-238  │ ← 未テスト
└────────────────────────────────────────────────────────────────┘
```

### 4つの指標の意味

| 指標 | 英語名 | 意味 | 重要度 |
|------|--------|------|--------|
| **% Stmts** | Statements | 実行された文の割合 | ⭐⭐⭐ |
| **% Branch** | Branches | 実行された分岐の割合 | ⭐⭐⭐⭐⭐ |
| **% Funcs** | Functions | 呼び出された関数の割合 | ⭐⭐ |
| **% Lines** | Lines | 実行された行の割合 | ⭐⭐⭐ |

**Branch カバレッジが最も重要**: すべての分岐（if/else, switch など）がテストされているかを示します。

---

## 4. 個別ファイルのレポートの見方

### ファイルリンクをクリック

`index.html` でファイル名（例: `LoginPage.tsx`）をクリックすると、そのファイルの詳細レポートが開きます。

### 色分けの意味

| 背景色 | 意味 | 実行回数 |
|--------|------|----------|
| 🟢 **緑** | 実行された | 1回以上 |
| 🟡 **黄色** | 部分的に実行 | 分岐の一部のみ |
| 🔴 **赤** | 実行されなかった | 0回 |
| ⚪ **白** | カバレッジ対象外 | インポート、型定義など |

### 行番号の横の数字

```
5x │  const [user, setUser] = useState<User | null>(null)
3x │  const [loading, setLoading] = useState(true)
0x │  const handleError = () => { ... }
   │
```

- `5x`: この行が5回実行された
- `3x`: この行が3回実行された
- `0x`: この行が実行されなかった（赤で表示）

### 実例

```typescript
1  │ export const LoginPage = () => {
5x │   const [username, setUsername] = useState('')
5x │   const [password, setPassword] = useState('')
5x │   const navigate = useNavigate()
   │
5x │   const handleSubmit = async (e: FormEvent) => {
3x │     e.preventDefault()
3x │     try {
2x │       await login(username, password)
2x │       navigate('/dashboard')
   │     } catch (error) {
1x │       setError(error.message)
   │     }
   │   }
   │
   │   return (
5x │     <form onSubmit={handleSubmit}>
   │       ...
   │     </form>
   │   )
   │ }
```

**解釈**:
- コンポーネントは5回レンダリングされた
- フォーム送信は3回テストされた
- 成功ケース: 2回
- エラーケース: 1回

---

## 5. カバレッジ閾値

### 現在の設定

`frontend/vitest.config.ts` で設定されています:

```typescript
coverage: {
  thresholds: {
    lines: 30,      // 30%以上
    functions: 45,  // 45%以上
    branches: 85,   // 85%以上
    statements: 30, // 30%以上
  },
}
```

### 実際のカバレッジ

| 指標 | 閾値 | 実際 | 判定 |
|------|------|------|------|
| Lines | 30% | 33.27% | ✅ +3.27% |
| Functions | 45% | 48.93% | ✅ +3.93% |
| Branches | 85% | 86.4% | ✅ +1.4% |
| Statements | 30% | 33.27% | ✅ +3.27% |

**すべての閾値をクリアしています！**

### 閾値を超えないとどうなる？

```bash
$ npm run test:coverage

ERROR: Coverage for lines (28.5%) does not meet threshold (30%)
```

テストが失敗し、CI/CD も失敗します。

---

## 6. カバレッジの解釈

### 高カバレッジファイル（80%以上）

```
src/components/Header.tsx:    100%
src/pages/LoginPage.tsx:      99.23%
src/pages/RegisterPage.tsx:   98.79%
src/hooks/useAuth.tsx:        96.51%
src/contexts/ToastContext:    86.36%
```

**意味**: これらのコンポーネントはしっかりテストされている

**理由**:
- 認証フローが重要なため、詳細にテストされている
- ユーザー体験に直結する部分
- バグがあると致命的

### 低カバレッジファイル（50%未満）

```
src/pages/CreatePostPage.tsx:  0%
src/pages/DashboardPage.tsx:   0%
src/pages/EditPostPage.tsx:    0%
src/api/posts.ts:              0%
src/components/Toast/*.tsx:    0%
```

**意味**: これらのファイルはテストされていない

**理由**:
- まだテストを書いていない
- 実装が最近追加された
- 優先度が低い

**対応**:
1. 重要度の高い順にテストを追加
2. まずは CreatePostPage, EditPostPage から
3. Toast コンポーネントは後回しでも OK

### Branch カバレッジが高い理由（86.4%）

```typescript
// 例: すべての分岐がテストされている
if (user) {           // ← テスト済み（user がいる場合）
  return <Dashboard />
} else {              // ← テスト済み（user がいない場合）
  return <Login />
}
```

**意味**: テスト済みコードの分岐が網羅的にテストされている

---

## 7. JSON サマリーの使い方

### ファイル構造

`coverage/coverage-summary.json` には機械読み取り可能な形式でカバレッジデータが格納されています。

```json
{
  "total": {
    "lines": {"total": 1608, "covered": 535, "pct": 33.27},
    "statements": {"total": 1608, "covered": 535, "pct": 33.27},
    "functions": {"total": 47, "covered": 23, "pct": 48.93},
    "branches": {"total": 103, "covered": 89, "pct": 86.4}
  },
  "/path/to/file.tsx": {
    "lines": {...},
    "statements": {...},
    "functions": {...},
    "branches": {...}
  }
}
```

### プログラムで読む

```bash
# 全体のカバレッジを取得
cat coverage/coverage-summary.json | jq '.total'

# カバレッジが50%未満のファイルを抽出
cat coverage/coverage-summary.json | \
  jq 'to_entries | map(select(.value.lines.pct < 50)) | map(.key)'

# 最もカバレッジが低いファイルTop 5
cat coverage/coverage-summary.json | \
  jq -r 'to_entries |
         map({file: .key, pct: .value.lines.pct}) |
         sort_by(.pct) |
         .[:5] |
         .[] | "\(.pct)%\t\(.file)"'
```

---

## 8. カバレッジ向上のコツ

### 優先度の付け方

**高優先度**:
1. ビジネスロジックが複雑なコード
2. バグが発生しやすいコード
3. ユーザー影響が大きいコード

**低優先度**:
1. 自動生成されたコード
2. シンプルな表示コンポーネント
3. 型定義ファイル

### 未カバーのコードを見つける

```bash
# HTMLレポートで赤い行を探す
open coverage/index.html

# カバレッジが低いファイルを特定
# → そのファイルを開く
# → 赤い行を見る
# → テストケースを追加
```

### テストケース追加の例

**Before（カバレッジ 50%）:**
```typescript
it('should login successfully', async () => {
  await login('user', 'pass')
  expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
})
```

**After（カバレッジ 85%）:**
```typescript
it('should login successfully', async () => {
  await login('user', 'pass')
  expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
})

// エラーケースを追加
it('should show error message on login failure', async () => {
  mockLogin.mockRejectedValue(new Error('Invalid credentials'))
  await login('user', 'wrong')
  expect(screen.getByText('Invalid credentials')).toBeInTheDocument()
})

// エッジケースを追加
it('should disable submit button during login', async () => {
  const button = screen.getByRole('button', { name: 'Login' })
  fireEvent.click(button)
  expect(button).toBeDisabled()
})
```

---

## 9. よくある質問

### Q1. なぜバックエンドと違って速いのですか？

**A**: V8 エンジンベースのカバレッジは、実行時のオーバーヘッドが非常に小さいためです。

- バックエンド（sb-cover）: 各式に計測コードを挿入 → 遅い
- フロントエンド（V8）: V8 エンジンがネイティブで計測 → 速い

### Q2. CI/CD で実行すべきですか？

**A**: 現在は実行していませんが、追加は可能です。

**メリット**:
- PRごとにカバレッジ変化を確認できる
- 後退を防げる

**デメリット**:
- 実行時間が5-10秒増加
- カバレッジが閾値を下回るとCIが失敗する

**推奨**: ローカルで開発時に実行し、リリース前に確認

### Q3. 100%を目指すべきですか？

**A**: いいえ。80%程度が現実的な目標です。

**理由**:
- 100%達成のコストが高い
- 些細なコードのテストに時間を取られる
- 重要なのは「重要な部分がテストされているか」

**現実的な目標**:
- Lines: 50-60%
- Functions: 60-70%
- Branches: 85-90%（重要！）
- Statements: 50-60%

### Q4. カバレッジが高い = 品質が高い？

**A**: 必ずしもそうではありません。

**カバレッジが高くてもバグがある例**:
```typescript
// カバレッジ 100% だがバグがある
it('should add numbers', () => {
  const result = add(2, 3)
  expect(result).toBeDefined() // ← アサーションが甘い
})

// 実際の実装
function add(a: number, b: number) {
  return a - b // ← バグ（減算になっている）
}
```

**重要なのは**:
- カバレッジ（実行されたか）
- アサーション（正しく動作するか）
- エッジケース（境界条件は？）

---

## 10. トラブルシューティング

### 問題: カバレッジレポートが生成されない

**原因**: テストが失敗している

**解決方法**:
```bash
npm test  # 通常のテストを実行してエラーを確認
```

### 問題: カバレッジが閾値を下回る

**原因**: 新しいコードを追加したがテストを書いていない

**解決方法**:
```bash
# 閾値を一時的に下げる（vitest.config.ts）
thresholds: {
  lines: 25,  # 30 から 25 に下げる
}

# または、テストを追加する（推奨）
```

### 問題: `coverage/` ディレクトリが Git に含まれる

**原因**: `.gitignore` が正しく設定されていない

**解決方法**:
```bash
# .gitignore に追加
echo "coverage/" >> frontend/.gitignore

# すでに追跡されている場合は削除
git rm -r --cached frontend/coverage/
```

### 問題: 特定のファイルをカバレッジから除外したい

**解決方法**: `vitest.config.ts` の `exclude` に追加
```typescript
coverage: {
  exclude: [
    'src/**/*.test.{ts,tsx}',
    'src/test/**',
    'src/mockData/**',  // ← 追加
  ],
}
```

---

## 11. 参考資料

### Vitest 公式ドキュメント

- [Vitest Coverage](https://vitest.dev/guide/coverage.html)
- [Vitest Configuration](https://vitest.dev/config/)

### プロジェクト内の関連ファイル

- `frontend/vitest.config.ts`: カバレッジ設定
- `frontend/package.json`: スクリプト定義
- `frontend/.gitignore`: カバレッジレポートの除外設定

### 関連コマンド

```bash
# 通常のテスト
npm test              # Watch モード
npm run test:run      # 1回のみ実行
npm run test:ui       # UI モード

# カバレッジ付き
npm run test:coverage    # カバレッジレポート生成
npm run test:coverage:ui # カバレッジ + UI

# カバレッジレポートの削除
rm -rf coverage/
```

---

## まとめ

- **実行**: `npm run test:coverage`
- **レポート**: `frontend/coverage/index.html`
- **現在**: Lines 33.27%, Branches 86.4%
- **目標**: Lines 50%, Branches 85%
- **頻度**: 開発時に定期的に実行
- **注意**: 数値だけでなく、重要な部分がテストされているかを確認

カバレッジは品質改善のツールです。100%を目指すのではなく、重要なビジネスロジックとエラーハンドリングが確実にテストされていることを優先しましょう。
