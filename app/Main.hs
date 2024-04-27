module Main where

import Parser.Algoritmos ( removerComentarios, parser ) 


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


