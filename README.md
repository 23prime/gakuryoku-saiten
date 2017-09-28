# gakuryoku-saiten

## What is this?

某学力テストの採点機です．

## How to use?

Haskell のビルドツール Stack が必要です．  
<a href="https://docs.haskellstack.org/en/stable/README/" target="_blank">公式</a> に従ってインストールしてください．  
Stack が入ったら，次の手順でインストール・ビルドします．

```
$ git clone https://github.com/23prime/gakuryoku-saiten.git
$ cd gakuryoku-saiten
$ stack setup
$ stack build
```

`stack setup` は，必要なバージョンの GHC がインストールされていない場合に必要です．  
これが通ったら，
```
$ stack exec gakuryoku-saiten-exe sample
Done! -- See './documents/sample-result.csv'
```
で結果が CSV に出力されます．  
入出力 CSV のフォーマットは `./documents/` 以下を参照してください．
