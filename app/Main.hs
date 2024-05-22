module Main where

import Parser.Algoritmos (parser) 
import Utils (instrucoesParaMaquinaTuring)
import Algoritmos.Sipser (conveterParaDuplamenteInfinita)
import Algoritmos (salvarArquivo)
import Tipos (transicoes)
import Algoritmos.DuplamenteInfinita (converterParaSipser)

main :: IO ()
main = do
    input <- readFile "/home/udesc/Documentos/tec-turing/teste_duplamente_infinita.txt"
    let retorno = parser input

    case snd retorno of
        Left erro -> print erro
        Right instrucoes -> do
            let maquina = instrucoesParaMaquinaTuring (fst retorno, instrucoes)
            let maquina' = converterParaSipser maquina
            _ <- salvarArquivo maquina' "/home/udesc/Documentos/tec-turing/output.out"
            print maquina