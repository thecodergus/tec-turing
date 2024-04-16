module Parser.Algoritmos where

import Parser.Tipos ( TuringTuple(TuringTuple) )
import Text.Parsec
    ( alphaNum,
      char,
      endOfLine,
      noneOf,
      oneOf,
      many1,
      optional,
      (<|>),
      many )
import Text.Parsec.String (Parser)

symbolParser :: Parser Char
symbolParser = noneOf ";*_\n\r\t "

stateParser :: Parser String
stateParser = many1 (alphaNum <|> char '*')

directionParser :: Parser Char
directionParser = oneOf "lrr*"

breakpointParser :: Parser Char
breakpointParser = char '!'

turingTupleParser :: Parser TuringTuple
turingTupleParser = do
    currentState <- stateParser
    _ <- char ' '
    currentSymbol <- symbolParser
    _ <- char ' '
    newSymbol <- (char '_' >> return currentSymbol) <|> symbolParser
    _ <- char ' '
    direction <- directionParser
    _ <- char ' '
    newState <- (char '*' >> return currentState) <|> stateParser
    optional breakpointParser

    return $ TuringTuple currentState currentSymbol newSymbol direction newState

turingProgramParser :: Parser [TuringTuple]
turingProgramParser = many (turingTupleParser <* endOfLine)