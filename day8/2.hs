#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import Data.Char

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  let grid = map (map digitToInt) lns
  print $ (maximum . concat . treesToScenicScore) grid

treesToScenicScore :: [[Int]] -> [[Int]]
treesToScenicScore grid = map (\rowIndex -> map (scenicScore grid rowIndex) colIndexes) rowIndexes
  where
    width = (length . head) grid
    height = length grid
    rowIndexes = [0..(width - 1)]
    colIndexes = [0..(height - 1)]

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore grid row col = leftScore  * rightScore * topScore * bottomScore
  where
    treeHeight = (grid !! row) !! col
    width = (length . head) grid
    height = length grid
    leftTrees = take col (grid !! row)
    rightTrees = drop (col + 1) (grid !! row)
    topTrees = take row verticalSlice
    bottomTrees = drop (row + 1) verticalSlice
    verticalSlice = map (!! col) grid
    leftScore = scoreInDirection (reverse leftTrees) treeHeight
    rightScore = scoreInDirection rightTrees treeHeight
    topScore = scoreInDirection (reverse topTrees) treeHeight
    bottomScore = scoreInDirection bottomTrees treeHeight

scoreInDirection :: [Int] -> Int -> Int
scoreInDirection trees refHeight
  | all (< refHeight) trees = length trees
  | otherwise = 1 + length(takeWhile (< refHeight) trees)