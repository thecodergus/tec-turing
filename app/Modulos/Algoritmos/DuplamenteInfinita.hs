module Algoritmos.DuplamenteInfinita where
import Tipos


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

converterParaSipser :: MaquinaTuring -> MaquinaTuring
converterParaSipser (MaquinaTuring _ _ _ _ Sipser) = error "A Maquina de Turing já é do tipo fita Sipser"
converterParaSipser (MaquinaTuring estados' alfabeto' transicoes' estadoInicial' _) = MaquinaTuring estados' alfabeto' (passoQuatro $ passoUm transicoes' ++ passoDois alfabeto' ++ passoTres alfabeto') estadoInicial' Sipser
        where
        -- Substituir estado "0" por estado inicio
        passoUm :: [Transicao] -> [Transicao]
        passoUm [] = []
        passoUm ((Transicao (Estado "0") simbAtual simbSerEscrito direcao (Estado "0")) : xs) = Transicao (Estado "inicio") simbAtual simbSerEscrito direcao (Estado "inicio") : passoUm xs
        passoUm ((Transicao (Estado "0") simbAtual simbSerEscrito direcao paraEstado) : xs) = Transicao (Estado "inicio") simbAtual simbSerEscrito direcao paraEstado : passoUm xs
        passoUm ((Transicao estadoAtual' simbAtual simbSerEscrito direcao (Estado "0")) : xs) = Transicao estadoAtual' simbAtual simbSerEscrito direcao (Estado "inicio") : passoUm xs
        passoUm ((Transicao estadoAtual' simbAtual simbSerEscrito direcao paraEstado) : xs) = Transicao estadoAtual' simbAtual simbSerEscrito direcao paraEstado : passoUm xs

        -- Adicionar meu estado inicial "0", o que inclui o uso de # e mover os itens para a direita
        passoDois :: [Simbolo] -> [Transicao]
        passoDois alfabeto'' = estado0 alfabeto'' ++ estadosMover alfabeto'' alfabeto''
                where
                estado0 :: [Simbolo] -> [Transicao]
                estado0 = map aux
                        where
                                aux :: Simbolo -> Transicao
                                aux Vazio = Transicao (Estado "0") (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo "#")) Direita (Estado "mover-direita-{}")
                                aux s@(Simbolo simb) = Transicao (Estado "0") (SimboloAtual s) (SimboloSerEscrito (Simbolo "#")) Direita (Estado ("mover-direita-{" ++ simb ++ "}"))

                -- mover-x :: {a,b,c,d...}, x, r, mover-{a,b,c,d...}
                estadosMover :: [Simbolo] -> [Simbolo] -> [Transicao]
                estadosMover [] _ = []
                estadosMover _ [] = []
                estadosMover (a : as) alfabetoAux = map (a `aux`) alfabetoAux ++ estadosMover as alfabetoAux
                        where
                                aux :: Simbolo -> Simbolo -> Transicao
                                aux Vazio Vazio = Transicao (Estado "mover-direita-vazio") (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Esquerda (Estado "retorno-{}")
                                aux Vazio (Simbolo simb) = Transicao (Estado "mover-direita-vazio") (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo "_")) Direita (Estado ("mover-direita-{" ++ simb ++ "}"))
                                aux (Simbolo x) (Simbolo simb) = Transicao (Estado ("mover-direita-{" ++ x ++ "}")) (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo x)) Direita (Estado ("mover-direita-{" ++ simb ++ "}"))
                                aux (Simbolo x) Vazio = Transicao (Estado ("mover-direita-{" ++ x ++ "}")) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo x)) Direita (Estado "mover-direita-{}")

        -- Apos encontrar branco duas vezes, voltar para a esquerda até achar o #
        passoTres :: [Simbolo] -> [Transicao]
        passoTres alfabeto'' = map (\(Simbolo s) -> Transicao (Estado ("mover-direita-{" ++ s ++ "}")) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo s)) Esquerda (Estado "retornar-hashtag")) alfabeto'' ++ [Transicao (Estado "retornar-hashtag") (SimboloAtual (Simbolo "#")) Manter Direita (Estado "inicio"), Transicao (Estado "retornar-hashtag") Todos Manter Esquerda (Estado "retornar-hashtag")]

        passoQuatro :: [Transicao] -> [Transicao]
        passoQuatro [] = []
        passoQuatro ((Transicao estadoAtual' simboloAtual' simboloSerEscrito' Esquerda paraEstado') : estados'') = 
        passoQuatro (t : ts) = passoQuatro ts