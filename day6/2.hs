#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import qualified Data.Set as Set

main :: IO ()
main = do
  content <- readFile "input"
  let line = head $ lines content
  print $ firstIndexNoDuplicate line 14

firstIndexNoDuplicate :: [Char] -> Int -> Int
firstIndexNoDuplicate chars index = if hasDuplicates firstFourteen then
                                      firstIndexNoDuplicate (drop 1 chars) (index + 1)
                                    else
                                      index
  where firstFourteen = take 14 chars

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list
