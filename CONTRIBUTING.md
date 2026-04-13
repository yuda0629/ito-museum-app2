# コントリビューションとレビューフロー

このリポジトリでは **GitHub の Pull Request（PR）** を通じて変更を取り込みます。

## ブランチ運用

1. **`main` を常にデプロイ可能な状態に近づける**ことを目標にします。
2. 作業は **`main` から分岐したブランチ** で行います。
   - 例: `feature/地図フィルタ改善`, `fix/streamlit-エラー`
3. 作業後、リモートに push し、**`main` 向けの PR** を開きます。

## PR を出す前に

- 変更内容が README や利用者向け説明に影響する場合は、可能な範囲で追記する。
- コミットメッセージは **何をしたか分かる日本語または英語** で書く（将来の自分とレビュアーのため）。

## レビューの進め方

1. **PR 作成**  
   `.github/pull_request_template.md` に沿って概要・確認項目を埋める。  
   未完了の作業なら **Draft PR** にする。
2. **自動・手動でレビュアー指定**  
   `CODEOWNERS` により候補が付く場合があります。必要に応じて追加でメンションする。
3. **レビュアー**  
   - 「Files changed」で差分を確認する。  
   - 行コメント・全体コメントで指摘・質問する。  
   - 問題なければ **Approve**、修正必須なら **Request changes** またはコメントで伝える。
4. **著者**  
   指摘に対応して push する。会話が終わったら再度レビューを依頼する。
5. **マージ**  
   チームのルールに従う（例: **Squash and merge** で履歴を一本化）。  
   Issue と紐づける場合は PR 本文に `Closes #番号` を書く。

## GitHub 上の推奨設定（リポジトリ管理者）

次は **Settings → Branches → Branch protection rules** で `main` に設定すると、レビューフローが安定しやすいです。

- **Require a pull request before merging**（PR 必須）
- **Require approvals**（例: 1）
- （CI を用意した場合）**Require status checks to pass**

設定はリポジトリの人数・運用に合わせて調整してください。

## 質問・相談

仕様や優先度が不明なときは、Issue で相談してから PR に着手すると手戻りが減ります。
