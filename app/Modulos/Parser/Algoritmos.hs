module Parser.Algoritmos where

import Text.Parsec
    ( anyChar,
      char,
      newline,
      noneOf,
      oneOf,
      endBy,
      many1,
      many,
      parse,
      ParseError )
import Text.Parsec.String (Parser)
import Parser.Tipos ( Instrucao(Instrucao) )
import Data.Bifunctor
import Tipos (Tipo (DuplamenteInfinita, Sipser))

-- | O parser para uma única instrução.
instructionParser :: Parser Instrucao
instructionParser = do
  estadoAtual' <- many1 (noneOf " \n")
  _ <- char ' '
  simboloAtual' <- anyChar
  _ <- char ' '
  novoSimbolo' <- anyChar
  _ <- char ' '
  direcao' <- oneOf "l*r"
  _ <- char ' '
  novoEstado' <- many1 (noneOf " \n")
  _ <- many (char ' ')
  return $ Instrucao estadoAtual' simboloAtual' novoSimbolo' direcao' novoEstado'

-- | O parser para uma lista de instruções.
parserInstrucoes :: Parser [Instrucao]
parserInstrucoes = instructionParser `endBy` newline

-- | Remove os comentários de um código.
removerComentarios :: String -> (Tipo, String)
removerComentarios str = (definirTipo $ head $ lines str, unlines . filter (not . null) . map (takeWhile (/= ';')) $ lines str)
  where
    definirTipo :: String -> Tipo
    definirTipo ";I" = DuplamenteInfinita
    definirTipo ";S" = Sipser
    definirTipo _ = error "Tipo de máquina não reconhecido."

-- | Faz o parsing de um código para uma lista de instruções.
parser :: String -> (Tipo, Either ParseError [Instrucao])
parser str = second (parse parserInstrucoes "") (removerComentarios str)