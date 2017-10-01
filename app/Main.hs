{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import System.Environment (getArgs)
import System.IO


main :: IO ()
main = do
  file' <- getArgs
  let file = head file'
  withFile ("./documents/" ++ file ++".csv") ReadMode $ \answers -> do
    ans <- hGetContents answers
    let answers = csvReader ans
    when (exception $ tail answers) $ fail "Specified CSV file is invalid."
    saiten file answers
    return ()

saiten :: String -> [[String]] -> IO ()
saiten file answers = do
  let marked = head answers : (map (mark $ last answers) $ tail answers)
  writeFile ("./documents/" ++ file ++ "-result.csv") $ csvWriter marked
  writeFile ("./documents/" ++ file ++ "-score.csv") $ csvWriter $ ["Name", "Score"] : (map score $ tail marked)
  putStrLn $ "Done! -- See './documents/" ++ file ++ "-result.csv' and './documents/" ++ file ++ "-score.csv'"
  return ()


-- 例外（CSV の答案部に "a" -- "e" 以外が含まれてる時）-> True
exception :: [[String]] -> Bool
exception answers
  | 0 `elem` checked = True
  | otherwise        = False
  where
    checked = map check answers :: [Integer]
    check :: [String] -> Integer -- 正しい形式のリストなら1，おかしければ0
    check answer
      | let answer' = tail answer
        in  filter (`elem` ["a", "b", "c", "d", "e"]) answer' == answer' = 1
      | otherwise = 0


-- 模範解答と照合して正答は "1"，誤答は "0"
-- ["right","a","b","c","d","d"]
-- ["Okkey","a","b","c","d","d"] -> ["Okkey","1","1","1","1","0"]
mark :: [String] -> [String] -> [String]
mark rightAnswer answer = head answer : zipWith check (tail rightAnswer) (tail answer)
  where
    check :: String -> String -> String
    check r a
      | r == a    = "1"
      | otherwise = "0"

-- CSV をリスト化
csvReader :: String -> [[String]]
csvReader = map csv2List . lines

-- リストを CSV 化
csvWriter :: [[String]] -> String
csvWriter = unlines . map list2Csv

-- 最初の "," で分割
csv2List :: String -> [String]
csv2List [] = []
csv2List l = h : csv2List i
  where
    (h, i) = split (\x -> x == ',') l

-- ["Okkey","a","b","c","d","d"] -> "Okkey,a,b,c,d,d"
list2Csv :: [String] -> String
list2Csv [] = []
list2Csv xs = foldl1 (++) $ head xs : (map ("," ++) $ tail xs)

-- リストを述語 p を満たす地点で分割
split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p (x:xs)
      | p x       = ([], xs)
      | otherwise = (x : ys, zs)
      where (ys, zs) = split p xs

-- 正答数を数え上げる
-- ["Okkey","1","1","1","1","0"] -> ["Okkey", "4"]
score :: [String] -> [String]
score [] = ["0"]
score (x : xs) = x : [show $ length $ filter (== "1") xs]
