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

-- | O parser 'symbolParser' reconhece um caractere que não seja um dos seguintes: ";", "*", "_", "\n", "\r" ou "\t".
symbolParser :: Parser Char
symbolParser = noneOf ";*_\n\r\t "

-- | O parser 'stateParser' reconhece uma sequência de caracteres alfanuméricos ou o caractere "*".
stateParser :: Parser String
stateParser = many1 (alphaNum <|> char '*')

-- | O parser 'directionParser' reconhece um dos seguintes caracteres: "l", "r", "*" ou "r".
directionParser :: Parser Char
directionParser = oneOf "lrr*"

-- | O parser 'breakpointParser' reconhece o caractere "!".
breakpointParser :: Parser Char
breakpointParser = char '!'

-- | O parser 'turingTupleParser' reconhece uma tupla de transição da Máquina de Turing.
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

-- | O parser 'turingProgramParser' reconhece uma lista de tuplas de transição da Máquina de Turing, separadas por quebras de linha.
turingProgramParser :: Parser [TuringTuple]
turingProgramParser = many (turingTupleParser <* endOfLine)