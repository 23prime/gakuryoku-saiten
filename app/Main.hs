{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.IO


-- CSV フォーマット
-- No.,1,2,3,4,5
-- Okkey,a,b,c,d,d
-- right,a,b,c,d,e

-- ans :: String
-- "No.,1,2,3,4,5\nOkkey,a,b,c,d,d\nright,a,b,c,d,e"

-- ansList :: [[String]]
-- [["No.","1","2","3","4","5"]
-- ,["Okkey","a","b","c","d","d"]
-- ,["right","a","b","c","d","e"]]

-- marked :: [[String]]
-- [["No.","1","2","3","4","5"]
-- ,["Okkey","1","1","1","1","0"]
-- ,["right","1","1","1","1","1"]]

main :: IO ()
main = do
  file' <- getArgs
  let file = head file'
  withFile ("./documents/" ++ file ++".csv") ReadMode $ \answer -> do
    ans <- hGetContents answer
    let
      ansList = csvReader ans
      marked = head ansList : (map (mark $ last ansList) $ tail ansList)
    writeFile ("./documents/" ++ file ++ "-result.csv") $ csvWriter marked
    putStrLn $ "Done! -- See './documents/" ++ file ++ "-result.csv'"
    return ()

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
