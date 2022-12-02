#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print $ sum $ map (lineScore . words) lns

lineScore :: [String] -> Int
lineScore (p1:p2:_) = case roundResult p1 p2 of
  1 -> shapeValue p2
  2 -> 6 + shapeValue p2
  3 -> 3 + shapeValue p2
  _ -> error "unexpected roundResult"
lineScore other = error "line must have format 'A<space>X'"

-- for now: 1 = elf wins, 2 = player wins, 3 = draw
roundResult :: String -> String -> Int
roundResult "A" "Z" = 1
roundResult "C" "Y" = 1
roundResult "B" "X" = 1
roundResult "A" "Y" = 2
roundResult "B" "Z" = 2
roundResult "C" "X" = 2
roundResult "A" "X" = 3
roundResult "B" "Y" = 3
roundResult "C" "Z" = 3
roundResult _ _ = error "invalid token"

shapeValue :: String -> Int
shapeValue shape
  | shape == "A" || shape == "X" = 1
  | shape == "B" || shape == "Y" = 2
  | shape == "C" || shape == "Z" = 3
  | otherwise = error "invalid token"
