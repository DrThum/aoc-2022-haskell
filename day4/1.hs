#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import qualified Data.Text as T

data Bounds = Bounds
  { lower :: Int
  , upper :: Int
  }

data Pair = Pair
  { first :: Bounds
  , second :: Bounds
  }

instance Show Pair where
  show (Pair first second) = show first ++ "," ++ show second

instance Show Bounds where
  show (Bounds lower upper) = show lower ++ "-" ++ show upper

main :: IO ()
main = do
  content <- readFile "input"
  let lns = lines content
  print $ length $ filter isPairOverlapping (map parsePair lns)

isPairOverlapping :: Pair -> Bool
isPairOverlapping Pair {
    first=Bounds {
      lower=lower1,
      upper=upper1 },
    second=Bounds {
      lower=lower2,
      upper=upper2
    }
  } = (lower1 >= lower2 && upper1 <= upper2) || (lower2 >= lower1 && upper2 <= upper1)

parsePair :: String -> Pair
parsePair str = Pair { first = head bounds, second = last bounds }
  where rawBounds = T.splitOn (T.pack ",") (T.pack str)
        bounds = map (parseBounds . T.unpack) rawBounds

parseBounds :: String -> Bounds
parseBounds str = Bounds { lower = lower, upper = upper }
  where (lowerStr:upperStr:_) = T.splitOn (T.pack "-") (T.pack str)
        lower = read (T.unpack lowerStr) :: Int
        upper = read (T.unpack upperStr) :: Int
