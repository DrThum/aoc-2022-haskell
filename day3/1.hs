#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print $ sum $ map (letterValue . findDuplicate . splitInHalf) lns

splitInHalf :: String -> (String, String)
splitInHalf list = (take n list, drop n list)
  where n = (length list) `div` 2

findDuplicate :: (String, String) -> Char
findDuplicate (left, right) = head $ filter (\c -> c `elem` right) left

letterValue :: Char -> Int
letterValue c
  | c `elem` ['A'..'Z'] = (fromEnum c) - 38
  | c `elem` ['a'..'z'] = (fromEnum c) - 96
  | otherwise = error "unexpected character"
