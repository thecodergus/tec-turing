module Main where

import Parser.Algoritmos ( removerComentarios, parser ) 
import Utils (instrucoesParaMaquinaTuring)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"-- | O parser 'turingProgramParser' reconhece uma lista de tuplas de transição da Máquina de Turing, separadas por quebras de linha.
    input <- readFile "teste.in"
    print "Input:"
    print input
    print "Input limpo:"
    print $ removerComentarios input
    print "Instrucoes:"
    let retorno = parser input

    case retorno of
        Left erro -> print erro
        Right instrucoes -> do
            print "Instrucoes:"
            print instrucoes
            let maquina = instrucoesParaMaquinaTuring instrucoes
            print "Maquina:"
            print maquina
