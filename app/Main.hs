module Main where

import Parser.Algoritmos 
import System.IO (readFile)
import Parser.Tipos (Instrucao(estadoAtual))


main :: IO ()
main = do
    putStrLn "Hello, Haskell!"-- | O parser 'turingProgramParser' reconhece uma lista de tuplas de transição da Máquina de Turing, separadas por quebras de linha.
    input <- readFile "teste.in"
    print "Input:"
    print input
    print "Input limpo:"
    print $ removerComentarios input
    print "Instrucoes:"
    print $ parser input

    -- Imprindo todos os estados
    print "Estados:"
    either print (print . map estadoAtual) (parser input)
