module Main where

import Parser.Algoritmos (parser) 
import Utils (instrucoesParaMaquinaTuring)
import Algoritmos.Sipser (conveterParaDuplamenteInfinita)
import Algoritmos (salvarArquivo)
import Tipos (Tipo (..))
import Algoritmos.DuplamenteInfinita (converterParaSipser)

main :: IO ()
main = do
    putStrLn "Digite o caminho do arquivo de entrada:"
    putStrLn "Se ele for de Sipser, vai converter para a duplamente infinita e o inverso tambem ocorrera."
    pathFile <- getLine
    input <- readFile pathFile

    let (tipo, maquina) = parser input

    case maquina of
        Left erro -> print erro
        Right instrucoes -> do
            let maquina' = instrucoesParaMaquinaTuring (tipo, instrucoes)
            case tipo of
                Sipser -> do
                    let maquinaSipser = conveterParaDuplamenteInfinita maquina'
                    _ <- salvarArquivo maquinaSipser (pathFile ++ ".out")
                    return ()
                DuplamenteInfinita -> do
                    let maquinaDuplamenteInfinita = converterParaSipser maquina'
                    _ <- salvarArquivo maquinaDuplamenteInfinita (pathFile ++ ".out")
                    return ()

    putStrLn $ "Arquivo gerado com sucesso em " ++ pathFile ++ ".out"