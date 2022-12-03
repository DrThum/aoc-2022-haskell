#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print $ sum $ map (letterValue . findCommon) $ groupsOf3Elves lns []

groupsOf3Elves :: [String] -> [[String]] -> [[String]]
groupsOf3Elves [] acc = acc
groupsOf3Elves rest acc = groupsOf3Elves (drop 3 rest) (acc ++ [(take 3 rest)])

findCommon :: [String] -> Char
findCommon (first:second:third:_) = head $ filter (\c -> c `elem` second && c `elem` third) first
findCommon _ = error "unexpected group of less than 3 elves"

letterValue :: Char -> Int
letterValue c
  | c `elem` ['A'..'Z'] = (fromEnum c) - 38
  | c `elem` ['a'..'z'] = (fromEnum c) - 96
  | otherwise = error "unexpected character"
