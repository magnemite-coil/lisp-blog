# テストドキュメント

このディレクトリには、lisp-blog プロジェクトのテスト関連ドキュメントが格納されています。

## 📚 ドキュメント一覧

### 1. [backend-coverage-guide.md](./backend-coverage-guide.md)
**バックエンド（Common Lisp）のカバレッジガイド**

- sb-cover の使い方
- カバレッジレポートの見方
- 色分けの意味
- よくある質問

**こんな人におすすめ**:
- バックエンドコードのカバレッジを確認したい
- どの部分がテストされていないか知りたい
- sb-cover のレポートの読み方が分からない

---

### 2. [frontend-coverage-guide.md](./frontend-coverage-guide.md)
**フロントエンド（React + TypeScript）のカバレッジガイド**

- Vitest + @vitest/coverage-v8 の使い方
- HTMLレポートの見方
- カバレッジ閾値の管理
- カバレッジ向上のコツ

**こんな人におすすめ**:
- フロントエンドコードのカバレッジを確認したい
- テストが不足している箇所を特定したい
- カバレッジレポートの読み方が分からない

---

## 🚀 クイックスタート

### バックエンドのカバレッジを確認したい

```bash
# プロジェクトルートで実行
./run-tests-coverage.sh

# レポートを開く
open coverage-report/cover-index.html
```

詳細: [backend-coverage-guide.md](./backend-coverage-guide.md)

---

### フロントエンドのカバレッジを確認したい

```bash
# frontend/ ディレクトリで実行
cd frontend
npm run test:coverage

# レポートを開く
open coverage/index.html
```

詳細: [frontend-coverage-guide.md](./frontend-coverage-guide.md)

---

## 📊 現在のカバレッジ状況

### バックエンド（Common Lisp）

| レイヤー | ファイル例 | カバレッジ |
|----------|-----------|------------|
| Service 層 | `src/service/auth.lisp` | 97.6% ✅ |
| Service 層 | `src/service/post.lisp` | 98.1% ✅ |
| Util 層 | `src/util/crypto.lisp` | 92.7% ✅ |
| Util 層 | `src/util/json.lisp` | 84.2% ✅ |
| Model 層 | `src/model/post.lisp` | 73.3% ✅ |
| Model 層 | `src/model/user.lisp` | 73.3% ✅ |
| Middleware 層 | `src/middleware/session.lisp` | 70.5% ⚠️ |
| Handler 層 | `src/handler/auth.lisp` | 9.0% ❌ |
| Handler 層 | `src/handler/post.lisp` | 8.2% ❌ |

**合計**: 307テストケース（100%成功率）

---

### フロントエンド（React + TypeScript）

| カテゴリ | ファイル例 | カバレッジ |
|----------|-----------|------------|
| Pages | `src/pages/LoginPage.tsx` | 99.23% ✅ |
| Pages | `src/pages/RegisterPage.tsx` | 98.79% ✅ |
| Hooks | `src/hooks/useAuth.tsx` | 96.51% ✅ |
| Components | `src/components/Header.tsx` | 100% ✅ |
| Contexts | `src/contexts/ToastContext.tsx` | 86.36% ✅ |
| Pages | `src/pages/CreatePostPage.tsx` | 0% ❌ |
| Pages | `src/pages/DashboardPage.tsx` | 0% ❌ |
| Pages | `src/pages/EditPostPage.tsx` | 0% ❌ |

**合計**: 36テストケース（100%成功率）

**全体カバレッジ**:
- Lines: 33.27%
- Functions: 48.93%
- Branches: 86.4% ✅
- Statements: 33.27%

---

## 🎯 カバレッジ目標

### バックエンド

| レイヤー | 現在 | 目標 | ステータス |
|----------|------|------|-----------|
| Service 層 | 97-98% | 90%以上 | ✅ 達成 |
| Util 層 | 70-92% | 80%以上 | ⚠️ 改善中 |
| Model 層 | 73% | 70%以上 | ✅ 達成 |
| Handler 層 | 8-9% | 50%以上 | ❌ 要改善 |

### フロントエンド

| 指標 | 現在 | 目標 | ステータス |
|------|------|------|-----------|
| Lines | 33.27% | 50% | ⚠️ 改善中 |
| Functions | 48.93% | 60% | ⚠️ 改善中 |
| Branches | 86.4% | 85% | ✅ 達成 |
| Statements | 33.27% | 50% | ⚠️ 改善中 |

---

## 📖 関連ドキュメント

### プロジェクト全体

- [README.md](../../README.md) - プロジェクト概要
- [CLAUDE.md](../../CLAUDE.md) - 開発ガイド
- [CHANGELOG.md](../../CHANGELOG.md) - 変更履歴

### 設計ドキュメント

- [docs/design/](../design/) - 詳細設計書（非公開）
- [docs/design/test-coverage-implementation.md](../design/test-coverage-implementation.md) - カバレッジ実装計画

### テストスクリプト

- [run-tests.sh](../../run-tests.sh) - バックエンド通常テスト
- [run-tests-coverage.sh](../../run-tests-coverage.sh) - バックエンドカバレッジテスト
- [frontend/package.json](../../frontend/package.json) - フロントエンドテストスクリプト

---

## 🛠️ よくある質問

### Q: カバレッジテストはいつ実行すべきですか？

**A**: 以下のタイミングで実行することを推奨します:

- **開発中**: 新機能追加後、カバレッジを確認
- **リリース前**: 品質確認として必ず実行
- **定期的**: 週次または月次でチェック
- **PR作成前**: カバレッジが下がっていないか確認

### Q: カバレッジが低い場合、どうすればいいですか？

**A**: 以下の手順で改善します:

1. **HTMLレポートを開く**: 赤い行（未実行）を確認
2. **優先度を付ける**: ビジネスロジックから順に
3. **テストケースを追加**: エラーケースやエッジケースも含める
4. **再確認**: カバレッジレポートを再生成

### Q: バックエンドのカバレッジテストが遅いのはなぜですか？

**A**: sb-cover がすべての式に計測コードを挿入するためです。

- 通常のテスト: 2-3秒
- カバレッジ付き: 5-10分（150-200倍）

そのため、CI/CD での実行は推奨しません。

### Q: 100%カバレッジを目指すべきですか？

**A**: いいえ。80%程度が現実的な目標です。

**理由**:
- 100%達成のコストが非常に高い
- 些細なコードのテストに時間を取られる
- 重要なのは「重要な部分がテストされているか」

---

## 🔗 外部リソース

### SBCL / sb-cover

- [SBCL Manual - sb-cover](http://www.sbcl.org/manual/#sb_002dcover)
- [Common Lisp Testing Framework](https://common-lisp.net/project/fiveam/)

### Vitest / Coverage

- [Vitest Official Documentation](https://vitest.dev/)
- [Vitest Coverage Guide](https://vitest.dev/guide/coverage.html)
- [@vitest/coverage-v8](https://www.npmjs.com/package/@vitest/coverage-v8)

### テストベストプラクティス

- [Testing Best Practices](https://github.com/goldbergyoni/javascript-testing-best-practices)
- [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/)

---

## 📝 このドキュメントについて

**作成日**: 2025-10-21
**対象バージョン**: lisp-blog v1.0
**メンテナ**: プロジェクト開発チーム

質問や改善提案がある場合は、Issue を作成してください。
