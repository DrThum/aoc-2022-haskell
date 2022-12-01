#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print (maximum $ groupAndSumByElf lns)

-- We have lines like
-- "1"
-- "2"
-- "3"
-- <empty>
-- "4"
-- "5"
-- <empty>
-- ...
-- And we want
-- [6, 9, ...]
groupAndSumByElf :: [String] -> [Int]
groupAndSumByElf lines = f lines [0]
  where f [] agg = agg
        f (l:ls) agg = if l == ""
                      then f ls (agg ++ [0])
                      else f ls (addToLast agg (read l :: Int))

-- addToLast [1, 2, 3] 4 = [1, 2, 7]
addToLast :: [Int] -> Int -> [Int]
addToLast [] n = [n]
addToLast nums n = init nums ++ [n + last nums]