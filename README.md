# gakuryoku-saiten

## What is this?

学力テストの採点機です．

## How to use?

Haskell のビルドツール Stack が必要です．  
<a href="https://docs.haskellstack.org/en/stable/README/" target="_blank">公式</a> に従ってインストールしてください．

```
$ stack build
```
が通ったら，
```
$ stack exec gakuryoku-saiten-exe sample
Done! -- See './documents/sample-result.csv'
```
で結果が CSV に出力されます．