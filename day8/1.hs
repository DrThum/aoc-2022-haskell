#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import Data.Char

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  let grid = map (map digitToInt) lns
  print $ (countVisible . digitToVisible) grid

countVisible :: [[Bool]] -> Int
countVisible gridBool = (length . filter (==True)) $ concat gridBool

digitToVisible :: [[Int]] -> [[Bool]]
digitToVisible grid = map (\rowIndex -> map (isVisible grid rowIndex) colIndexes) rowIndexes
  where
    width = (length . head) grid
    height = length grid
    rowIndexes = [0..(width - 1)]
    colIndexes = [0..(height - 1)]

isVisible :: [[Int]] -> Int -> Int -> Bool
isVisible grid row col
  | row == 0 || row == (height - 1) = True
  | col == 0 || col == (width - 1) = True
  | otherwise = isHigherThan leftTrees || isHigherThan rightTrees || isHigherThan topTrees || isHigherThan bottomTrees
  where
    treeHeight = (grid !! row) !! col
    isHigherThan = isMaxValue treeHeight
    width = (length . head) grid
    height = length grid
    leftTrees = take col (grid !! row)
    rightTrees = drop (col + 1) (grid !! row)
    topTrees = take row verticalSlice
    bottomTrees = drop (row + 1) verticalSlice
    verticalSlice = map (!! col) grid

isMaxValue :: Int -> [Int] -> Bool
isMaxValue n = all (< n)
