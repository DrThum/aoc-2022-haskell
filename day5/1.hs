#!/usr/bin/env cabal
{- cabal:
  build-depends: base
               , parsec
-}

import Data.Char
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

-- data Body = Body [Statement]

data Statement = Crates [Char]
                | Instruction Int Int Int
                | NoStatement
  deriving (Show)

newtype CrState = CrState [Stack] deriving (Show)

newtype Stack = Stack [Char] deriving (Show)

main :: IO ()
main = do
  content <- readFile "input"
  let lns = (filter (/= "\r") . lines) content
  let statements = map parseStatement lns
  let clearedStatements = map (fromRight NoStatement) statements
  let nbStacks = calcNbStacks clearedStatements
  let initialState = CrState $ map newStack [0..(nbStacks - 1)]
  let finalState = foldl processStatement initialState clearedStatements
  print finalState

calcNbStacks :: [Statement] -> Int
calcNbStacks [] = error "no Crates statement"
calcNbStacks ((Crates chars):tail) = length chars
calcNbStacks (_:tail) = calcNbStacks tail

processStatement :: CrState -> Statement -> CrState
processStatement (CrState stacks) (Crates chars) = CrState $ map (uncurry push) zipped
  where zipped = zip stacks chars
processStatement state (Instruction qty from to) = error "todo instruction"
processStatement state NoStatement = state

parseStatement :: String -> Either ParseError Statement
parseStatement = either
  where either = parse (choice [cratesParser, instructionParser, noStatementParser]) ""

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
newStack :: Int -> Stack
newStack _ = Stack []

push :: Stack -> Char -> Stack
push stack ' ' = stack
push (Stack cs) c = Stack (cs ++ [c])

pop :: Stack -> (Char, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

reverseStack :: Stack -> Stack
reverseStack (Stack cs) = Stack $ reverse cs