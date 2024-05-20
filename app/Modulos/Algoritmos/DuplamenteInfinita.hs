module Algoritmos.DuplamenteInfinita where
import qualified Tipos


-- converterParaSipser :: Tipos.MaquinaTuring -> Tipos.MaquinaTuring
-- converterParaSipser (Tipos.MaquinaTuring estados alfabeto transicoes estadoInicial tipo) =  Tipos.MaquinaTuring estados alfabeto (inicioTransicao transicoes) estadoInicial tipo
--     where
        -- inicioTransicao :: [Tipos.Transicao] -> [Tipos.Transicao]
        -- inicioTransicao = substituirEstadoInicial
        --     where
        --         substituirEstadoInicial :: [Tipos.Transicao] -> [Tipos.Transicao]
        --         substituirEstadoInicial [] = adicionarNovosEstadosIniciais
        --         substituirEstadoInicial ((Tipos.Transicao (Tipos.Estado "0") simboloAtual' simboloSerEscrito' direcao' (Tipos.Estado "0")) : estados') = Tipos.Transicao (Tipos.Estado "estadoInicial") simboloAtual' simboloSerEscrito' direcao' (Tipos.Estado "estadoInicial") : substituirEstadoInicial estados'
        --         substituirEstadoInicial ((Tipos.Transicao (Tipos.Estado "0") simboloAtual' simboloSerEscrito' direcao' proximoEstado') : estados') = Tipos.Transicao (Tipos.Estado "estadoInicial") simboloAtual' simboloSerEscrito' direcao' proximoEstado' : substituirEstadoInicial estados'
        --         substituirEstadoInicial (estado' : estados') = estado' : substituirEstadoInicial estados'
        --             where
        --                 adicionarNovosEstadosIniciais :: [Tipos.Transicao] -> [Tipos.Transicao]
        --                 adicionarNovosEstadosIniciais = hashtagInicial
        --                     where
        --                         hashtagInicial :: [Tipos.Simbolo] -> [Tipos.Transicao]
        --                         hashtagInicial alfabeto' = map passoUm alfabeto'
        --                             where
        --                                 passoUm :: Tipos.Simbolo -> Tipos.Transicao
        --                                 passoUm Tipos.Vazio = Tipos.Transicao (Tipos.Estado "0") (Tipos.SimboloAtual $ Tipos.Simbolo "_") (Tipos.SimboloSerEscrito $ Tipos.Simbolo "#") Tipos.Direita (Tipos.Estado ("hashtagInicial" ++ "_"))
        --                                 passoUm (Tipos.Simbolo sim) = Tipos.Transicao (Tipos.Estado "0") (Tipos.SimboloAtual $ Tipos.Simbolo sim) (Tipos.SimboloSerEscrito $ Tipos.Simbolo "#") Tipos.Direita (Tipos.Estado ("hashtagInicial" ++ sim))