#!/usr/bin/env cabal
{- cabal:
  build-depends: base
               , parsec
-}

import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

-- data Body = Body [Statement]

data Statement = Crates [Char]
                | Instruction Int Int Int
                | NoStatement
  deriving (Show)

data CrState = CrState [Stack]

newtype Stack = Stack [Char]

main :: IO ()
main = do
  content <- readFile "input"
  let lns = (filter (/= "\r") . lines) content
  print $ map (parse (choice [cratesParser, instructionParser, noStatementParser]) "") lns

processStatement :: CrState -> Statement -> CrState
processStatement state (Crates chars) = error "crates"
processStatement state (Instruction qty from to) = error "instruction"
processStatement state NoStatement = state

cratesParser :: GenParser Char st Statement
cratesParser = Crates <$> many1 crateParser

crateParser :: GenParser Char st Char
crateParser = nonEmptyCrateParser <|> try emptyCrateParser

nonEmptyCrateParser :: GenParser Char st Char
nonEmptyCrateParser = do
  content <- between (char '[') (char ']') upper
  optional space
  return content

emptyCrateParser :: GenParser Char st Char
emptyCrateParser = do
  count 3 space
  optional space
  return ' '

noStatementParser :: GenParser Char st Statement
noStatementParser = do
  many stackIndexParser <|> many emptyLineToStatementParser
  return NoStatement

emptyLineToStatementParser :: GenParser Char st ()
emptyLineToStatementParser = do
  endOfLine
  optional space

stackIndexParser :: GenParser Char st ()
stackIndexParser = do
  space
  digit
  space
  optional space

instructionParser :: GenParser Char st Statement
instructionParser = do
  string "move "
  qty <- many digit
  string " from "
  from <- digit
  string " to "
  to <- digit
  return $ Instruction (read qty :: Int) (digitToInt from) (digitToInt to)

-- Stack impl
push :: Stack -> Char -> Stack
push stack ' ' = stack
push (Stack cs) c = Stack (cs ++ [c])

pop :: Stack -> (Char, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

reverse :: Stack -> Stack
reverse (Stack cs) = Stack $ reverse cs