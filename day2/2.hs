#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print $ sum $ map (lineScore . words) lns

lineScore :: [String] -> Int
lineScore (p1:p2:_) = aV + oV
  where
    actionPlayed = roundAction p1 p2
    aV = actionValue actionPlayed
    oV = outcomeValue p2
lineScore other = error "line must have format 'A<space>X'"

-- A/X: Rock
-- B/Y: Paper
-- C/Z: Scissor
-- X: player needs to lose
-- Y: player needs a draw
-- Z: player needs to win
roundAction :: String -> String -> String
roundAction "A" "X" = "Z"
roundAction "A" "Y" = "X"
roundAction "A" "Z" = "Y"
roundAction "B" "X" = "X"
roundAction "B" "Y" = "Y"
roundAction "B" "Z" = "Z"
roundAction "C" "X" = "Y"
roundAction "C" "Y" = "Z"
roundAction "C" "Z" = "X"
roundAction _ _ = error "invalid token"

-- How many points do I get for each outcome
outcomeValue :: String -> Int
outcomeValue "X" = 0 -- lose
outcomeValue "Y" = 3 -- draw
outcomeValue "Z" = 6 -- win
outcomeValue _ = error "format"

-- How many points do I get for each action
actionValue :: String -> Int
actionValue "X" = 1 -- rock
actionValue "Y" = 2 -- paper
actionValue "Z" = 3 -- scissor
actionValue _ = error "format"