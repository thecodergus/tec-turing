module Algoritmos where
import qualified Tipos
import Text.Read (Lexeme(String))


salvarArquivo :: Tipos.MaquinaTuring -> String -> IO ()
salvarArquivo mt nomeOutput = writeFile nomeOutput (converterParaString mt)


converterParaString :: Tipos.MaquinaTuring -> String
converterParaString (Tipos.MaquinaTuring _ _ transicoes _ Tipos.Sipser) = ";I\n" ++ transicaoParaString transicoes
converterParaString (Tipos.MaquinaTuring _ _ transicoes _ Tipos.DuplamenteInfinita) = ";S\n" ++ transicaoParaString transicoes

transicaoParaString :: [Tipos.Transicao] -> String
transicaoParaString [] = ""
transicaoParaString ((Tipos.Transicao estadoAtual simboloAtual simboloSerEscrito direcao estadoDestino) : xs) = estadoAtualParaString estadoAtual ++ " " ++ simboloAtualParaString simboloAtual ++ " " ++ simboloSerEscritoParaString simboloSerEscrito ++ " " ++ direcaoParaString direcao ++ " " ++ estadoDestinoParaString estadoDestino ++ "\n" ++ transicaoParaString xs
    where
        estadoAtualParaString :: Tipos.Estado -> String
        estadoAtualParaString (Tipos.Estado simb) = simb

        simboloAtualParaString :: Tipos.SimboloAtual -> String
        simboloAtualParaString (Tipos.SimboloAtual (Tipos.Simbolo simb)) = simb
        simboloAtualParaString (Tipos.SimboloAtual Tipos.Vazio) = "_"
        simboloAtualParaString Tipos.Todos = "*"

        simboloSerEscritoParaString :: Tipos.SimboloSerEscrito -> String
        simboloSerEscritoParaString Tipos.Manter = "*"
        simboloSerEscritoParaString (Tipos.SimboloSerEscrito Tipos.Vazio) = "_"
        simboloSerEscritoParaString (Tipos.SimboloSerEscrito (Tipos.Simbolo simb)) = simb

        direcaoParaString :: Tipos.Direcao -> String
        direcaoParaString Tipos.Esquerda = "l"
        direcaoParaString Tipos.Direita = "r"
        direcaoParaString Tipos.Parado = "*"

        estadoDestinoParaString :: Tipos.Estado -> String
        estadoDestinoParaString (Tipos.Estado simb) = simb