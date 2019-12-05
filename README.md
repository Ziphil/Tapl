<div align="center">
<h1>TaPL の Haskell 実装 (＋α)</h1>
</div>

## 概要
『Types And Programming Languages』に出てくる言語の Haskell 実装です。
OCaml 分からないし Haskell が好きなので Haskell で書きます。

## 内容

### `Ziphil.Lang.UntypedLambda`
第 7 章の実装です。
型なしラムダ計算を de Bruijn インデックスを用いて実装しています。

### `Ziphil.Recursive`
catamorphism とか hylomorphism とかの実装です。
私のサイトで公開している[日記シリーズ](http://ziphil.com/diary/application/)で利用しています。
TaPL とは関係ありませんが、置くところがなかったのでここに置いてあります。