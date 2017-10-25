{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO


main :: IO ()
main = do
  file' <- getArgs
  -- コマンドライン引数からファイル名を取ってくる．
  let file = head file'
      -- 第1引数を抜き出す．
  withFile ("./documents/" ++ file ++".csv") ReadMode $ \answers' -> do
    -- () 内はファイルパス．
    -- 指定したファイルを読み込み専用モードで開く．
    ans <- hGetContents answers'
    -- ファイルの中身を String として取ってくる．
    let (num : answers) = csvReader ans
        -- CSV を配列化．
    when (exception answers) $ fail "Specified CSV file is invalid."
    -- 例外処理．不正なファイルはここで弾く．
    let marked = num : (map (mark $ last answers) answers)
        -- 配列化したものを採点．
        scored = ["Name", "Score"] : (map score $ tail marked)
        -- 一応点数も出す．
    writeFile ("./documents/" ++ file ++ "-result.csv") $ csvWriter marked
    -- 採点結果を新しいファイルに出力．
    writeFile ("./documents/" ++ file ++ "-score.csv" ) $ csvWriter scored
    -- 点数を新しいファイルに出力．
    putStrLn $ "Done! -- See './documents/" ++ file ++ "-result.csv' and './documents/" ++ file ++ "-score.csv'"
    -- 採点がうまくいったらコマンドラインにメッセージを表示．
    return ()


-- 例外（答案部に "a" -- "e" 以外が含まれてる時，問題 No. 以外が空の時）-> True
exception :: [[String]] -> Bool
exception [x] = True -- CSV が1行しかない時
exception answers
  | 0 `elem` map check answers = True -- 1人でも check が 0 だったら弾く．
  | otherwise                  = False
  where
    check :: [String] -> Integer
    -- 各人の答案に "a" -- "e" 以外が含まれてるかどうかチェック．
    -- 含まれていれば 0 含まれていなければ 1
    check [] = 0
    check (name : answer)
      | filter (`notElem` ["a", "b", "c", "d", "e"]) answer == [] = 1
        -- ["a", "b", "c", "d", "e"] のどれにもに一致しないものを抽出．
        -- 1つもなければ 1
      | otherwise = 0


-- 採点部分
-- ["right","a","b","c","d","d"] ["Okkey","a","b","c","d","d"]
-- -> ["Okkey","1","1","1","1","0"]
mark :: [String] -> [String] -> [String]
mark (right : rightAnswer) (name : answer) = name : zipWith check rightAnswer answer
-- 正答と1人の答案を受け取り，各問題の解答が正答に一致しているかをチェック．
  where
    check :: String -> String -> String
    check r a
      | r == a    = "1" -- 正答
      | otherwise = "0" -- 誤答


-- CSV 操作
-- CSV -> 配列
csvReader :: String -> [[String]]
csvReader = map splitByCommas . lines

-- 文字列をカンマで区切って配列化
splitByCommas :: String -> [String]
splitByCommas [] = []
splitByCommas line = beforeComma : splitByCommas other
  where
    (beforeComma, other) = split (== ',') line

-- 述語 p を最初に満たす地点でリストを前後に分割
split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p (x : xs)
      | p x       = ([], xs)
        -- 先頭の要素が述語を満たすかチェック．
        -- 述語を満たす要素は出力しないから []
      | otherwise = (x : ys, zs) -- 述語を満たさない要素は左側へ．
      where (ys, zs) = split p xs


-- 配列 -> CSV
csvWriter :: [[String]] -> String
csvWriter = unlines . map concatByCommas

-- ["Okkey","a","b","c","d","d"] -> "Okkey,a,b,c,d,d"
concatByCommas :: [String] -> String
concatByCommas [] = []
concatByCommas (x : xs) = concat $ x : (map ("," ++) xs)
                    -- 先頭以外の各要素の頭に "," を付けて，全部つなげる．

-- 正答数の数え上げ
-- ["Okkey","1","1","1","1","0"] -> ["Okkey", "4"]
score :: [String] -> [String]
score [] = ["No data"]
score (name : xs) = name : [show $ length $ filter (== "1") xs]
                    -- "1" を抽出して，その長さが正答数．
