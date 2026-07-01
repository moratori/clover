# CLAUDE.md

このファイルは Claude Code がこのリポジトリで作業する際の指針です。

## プロジェクト概要

clover は Common Lisp 製の実験的な自動定理証明器です。一階述語論理式に対して、
以下の2系統のアルゴリズムで証明を行います。

- **導出原理 (Resolution Principle)** — 節集合の反駁による証明
- **Knuth-Bendix 完備化アルゴリズム** — 等式論理・項書き換え系による証明

REPL・コマンドラインバッチ・ビルド済みバイナリの3形態で利用されます。

## Claude Code の役割（重要・厳守）

このプロジェクトにおける Claude Code の関わり方は、原則として次の3点に**限定**します。

1. **テストコードの生成** — `tests/` 配下へのテスト追加・改善。
2. **既存実装の疑わしい点の指摘** — バグ・健全性/停止性の懸念・規約逸脱などの調査と報告。
3. **ユーザーの質問への応答** — コード理解・設計意図の説明など。

### src/ への変更に関する原則

- **`src/` 配下の実装変更は、原則として人間（ユーザー）が行います。**
- Claude Code は、ユーザーからの**明示的な指示がない限り `src/` を編集しません**。
- 不具合を見つけた場合も、**勝手に修正せず、まず指摘・報告にとどめてください**。
  修正案はパッチではなく「説明＋該当箇所＋推奨する直し方」の形で提示します。
- ユーザーが明示的に「`src/xxx.lisp` を修正して」と指示した場合に限り、編集してよい。

### テストコードの扱い

- `tests/` 配下の新規作成・編集は、上記「役割1」に該当するため**指示があれば実施可**。
- ただし、テストを通すために `src/` の実装を変更してはいけません（役割の原則が優先）。
- テストが失敗した場合、それが「テストの誤り」なのか「実装の不具合」なのかを切り分け、
  後者と判断したら**実装は直さず指摘として報告**してください。

### 指摘の作法（事実と推測の分離）

- ソースの字面から客観的に断定できる事項（"事実"）と、アルゴリズムのあるべき仕様に
  照らした"推測"は、明確に分離して報告してください。
- 推測には「仕様確認が必要」「設計意図次第」など、確度を必ず添えてください。
- 断定できないものを断定しないこと。最高品質とは「誤った断定をしないこと」です。

## ビルドと実行

### ローカルプロジェクトとしてのロード

このプロジェクトは Quicklisp 未登録です。Roswell の local-projects 等、ASDF が探索
できる場所に配置した上でロードします。

```lisp
(ql:quickload :clover)
```

## テスト

テストフレームワークは **1am** を使用します。

### 全テスト実行

```bash
./run-all-tests.sh
```

このスクリプトは Roswell・timeout を必須とし、SBCL の場合は `sb-cover` による
カバレッジと `sb-sprof` によるプロファイルも出力します（Graphviz があれば反駁木も描画）。

### REPL からの個別実行

```lisp
(ql:quickload :clover-test)
(in-package :clover-test)
(run)
```

### テストの追加手順

1. `tests/src/<module>.lisp` に 1am の `test` フォームを追加する。
2. 新規ファイルを作る場合は `clover-test.asd` の `:components` に登録する
   （登録漏れはロードされないので注意）。
3. 既存ファイルへの追記なら `.asd` の変更は不要。

## テスト記述の規約（既存コードに準拠）

- パッケージは `clover.tests.<module>` とし、`:1am` と対象パッケージを `:use` する。
- 構造体の生成にはコンストラクタ関数を使う:
  - 変数項: `(vterm 'x)`
  - 関数項: `(fterm 'f (list (vterm 'x) ...))`
  - 定数:   `(constant 'A)`
  - リテラル: `(literal negation predicate args)`
  - 節:       `(clause (list literal ...))`
  - 等式:     `(equation negation left right)`
  - 書き換え規則: `(rewrite-rule src dst)`
- 等価判定には専用述語を使う: `term=` `literal=` `clause=` `unifier-set=`
  `equation=` `rewrite-rule=`（以上 `clover.equality`）・`alphabet-equivalent-p`
  （`clover.unify`）など（`equal` ではなく）。
- パッケージ外に export されていない内部関数は `clover.<pkg>::<name>`（二重コロン）で参照する。
  例: `(clover.unify::%collect-disagreement-set ...)`。
- LPO を使うテストでは `clover.parameters:*term-order-algorithm*` を `:lpo` に設定し、
  `function-symbol-ordering` で記号順序を与える。
- アサーションは `(is <form>)`、例外を期待する場合は `(signals <condition> <form>)`。

## 主要モジュール構成（src/）

- `types.lisp` — 全データ構造（term/vterm/fterm/constant, literal, clause, equation,
  rewrite-rule, unifier 等）の defstruct 定義。
- `equality.lisp` — 構文的等価判定述語（`term=` `literal=` `clause=` `equation=` 等）。
- `logical-predicates.lisp` — 出現検査 (`occurrence-check`)・節/リテラル/項に関する各種述語。
- `canonicalization.lisp` — 変数リネーム不変な正準キー生成と `remove-duplicates-by-key`。
- `parameters.lisp` — 全域の設定・チューニング値（`defparameter` 群）。
- `substitute.lisp` — 単一化子の適用 (`apply-unifier` / `apply-unifier-set`)。
- `unify.lisp` — 最汎単一化子の計算 (`find-most-general-unifier-set`)、包摂判定。
- `resolution.lisp` — 導出原理（`:default` / `:snl` モード）。
- `termorder.lisp` — 項順序（LPO）。完備化の向き付け・停止性の土台。
- `rewrite.lisp` — 項書き換え (`rewrite` / `rewrite-final` / `rewrite-all-ways`)。
- `criticalpair.lisp` — 危険対の計算。
- `completion.lisp` — KB 完備化本体（orient/compose/collapse/deduce の推論規則）。
- `simplify.lisp` / `rename.lisp` — 節集合の簡約・変数リネーム。
- `search/` — 探索アルゴリズム（dfs, iddfs, astar, extractor）。
- `ui/` — REPL・バッチ・エントリポイント。

## コーディング規約（観察された慣習）

- ジェネリック関数（`defgeneric` / `defmethod`）と型ディスパッチを多用する。
- 構造体は基本的に `:read-only t`（イミュータブル）。新しい値は作り直す。
- パッケージ名は `clover.<module>`、テストは `clover.tests.<module>`。
- 内部専用関数には `%` プレフィックス（例 `%orient`, `%collect-disagreement-set`）。
- エラーは `clover.conditions` の専用コンディションを `make-condition` して `error` する。

## やってはいけないこと

- ユーザーの明示指示なしに `src/` を変更しないこと。
- バグを見つけても勝手に修正しないこと（指摘にとどめる）。
- テストを通すために実装を書き換えないこと。
- 確度の低い推測を事実として断定しないこと。
- `clover-test.asd` への登録を忘れた新規テストファイルを「追加済み」と扱わないこと。
