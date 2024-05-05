module Main where

import Parser.Algoritmos (parser) 
import Utils (instrucoesParaMaquinaTuring)

main :: IO ()
main = do
    input <- readFile "teste.in"
    let retorno = parser input

    case snd retorno of
        Left erro -> print erro
        Right instrucoes -> do
            let maquina = instrucoesParaMaquinaTuring (fst retorno, instrucoes)
            print maquina