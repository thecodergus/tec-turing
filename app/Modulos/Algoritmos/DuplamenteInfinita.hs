module Algoritmos.DuplamenteInfinita where
import Tipos


converterParaSipser :: MaquinaTuring -> MaquinaTuring
converterParaSipser (MaquinaTuring _ _ _ _ Sipser) = error "A Maquina de Turing já é do tipo fita Sipser"
converterParaSipser (MaquinaTuring estados' alfabeto' transicoes' estadoInicial' _) = 
    MaquinaTuring 
    estados' 
    alfabeto' 
        (
            passoUm transicoes' ++ 
            passoDois alfabeto' ++ 
            passoTres alfabeto' (alfabeto' ++ [Vazio]) ++ 
            passoQuatro ++
            passoCinco (alfabeto' ++ [Vazio])
            -- passoSeis estados' (alfabeto' ++ [Vazio])
        ) 
        estadoInicial' 
        Sipser
        where
        -- Substituir "0" por inicio
        passoUm :: [Transicao] -> [Transicao]
        passoUm [] = []
        passoUm (t@(Transicao (Estado "0") _ _ _ (Estado "0")) : ts) = t {estadoAtual = Estado "inicio", proximoEstado = Estado "inicio"} : passoUm ts
        passoUm (t@(Transicao (Estado "0") _ _ _ _) : ts) = t{estadoAtual = Estado "inicio"} : passoUm ts
        passoUm (t@(Transicao _ _ _ _ (Estado "0")) : ts) = t {proximoEstado = Estado "inicio"} : passoUm ts
        passoUm (t : ts) = t : passoUm ts

        -- Adicionando nova configuração inicial
        passoDois :: [Simbolo] -> [Transicao]
        passoDois [] = []
        passoDois (Vazio : ss) = Transicao (Estado "0") (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo "#")) Direita (Estado "mover-direita-{}") : passoDois ss
        passoDois (s@(Simbolo simb) : ss) = Transicao (Estado "0") (SimboloAtual s) (SimboloSerEscrito (Simbolo "#")) Direita (Estado ("mover-direita-{" ++ simb ++ "}")) : passoDois ss

        -- Adicionandos os demais movimentos a direita "mover-direita-{a,b,c,d...}"
        passoTres :: [Simbolo] -> [Simbolo] -> [Transicao]
        passoTres [] _ = []
        passoTres _ [] = []
        passoTres (s : ss) alfabeto'' =
            map (s `passoTres'`) alfabeto'' ++
            passoTres ss alfabeto''
            where
                passoTres' :: Simbolo -> Simbolo -> Transicao
                passoTres' Vazio Vazio = Transicao (Estado "mover-direita-{}") (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Esquerda (Estado "retornar-quase-vazio")
                passoTres' Vazio (Simbolo simb) = Transicao (Estado "mover-direita-{}") (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo "_")) Direita (Estado ("mover-direita-{" ++ simb ++ "}"))
                passoTres' (Simbolo x) (Simbolo simb) = Transicao (Estado ("mover-direita-{" ++ x ++ "}")) (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo x)) Direita (Estado ("mover-direita-{" ++ simb ++ "}"))
                passoTres' (Simbolo x) Vazio = Transicao (Estado ("mover-direita-{" ++ x ++ "}")) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo x)) Direita (Estado "mover-direita-{}")

        -- Adicionando retorno para o inicio a partir do segundo vazio encontrado
        passoQuatro :: [Transicao]
        passoQuatro = [
                Transicao (Estado "mover-direita-{}") (SimboloAtual Vazio) Manter Direita (Estado "retornar-quase-vazio"),
                Transicao (Estado "retornar-vazio") (SimboloAtual Vazio) Manter Direita (Estado "retornar-comeco"),
                Transicao (Estado "retornar-comeco") (SimboloAtual (Simbolo "#")) Manter Direita (Estado "inicio"),
                Transicao (Estado "retornar-comeco") Todos Manter Esquerda (Estado "retornar-comeco")
            ]

        -- Adicinando transição que decide se é final de entrada ou não
        passoCinco :: [Simbolo] -> [Transicao]
        passoCinco [] = []
        passoCinco s = map passoCinco' s
            where
                passoCinco' :: Simbolo -> Transicao
                passoCinco' (Simbolo s) = Transicao (Estado "retornar-quase-vazio") (SimboloAtual (Simbolo s)) (SimboloSerEscrito Vazio) Esquerda (Estado ("mover-direita-{" ++ s ++ "}"))
                passoCinco' Vazio = Transicao (Estado "retornar-quase-vazio") (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Esquerda (Estado "retornar-vazio")




        -- Agora falta adptar que para cada estado, se ele encontrar #, tem que mover toda a fita mais para a direita e então retornar para o novo simbolo em branco e retornar pro estado que encontrou o # em vez de branco
        passoSeis :: [Estado] -> [Simbolo] -> [Transicao]
        passoSeis [] _ = []
        passoSeis _ [] = []
        passoSeis (e : es) alfabeto''' =
            [passoSeis' e] ++
            passoSeis'' e alfabeto''' ++
            passoSeis''' e alfabeto''' alfabeto''' ++
            [passoSeis'''' e] ++
            passoSeis es alfabeto'''
            where
                -- Ao estado encontrar o #, mover a cabeça para a direita com [<estado>]-mover-direita
                passoSeis' :: Estado -> Transicao
                passoSeis' (Estado e') = Transicao (Estado e') (SimboloAtual (Simbolo "#")) Manter Direita (Estado ("[" ++ e' ++ "]-mover-direita"))

                -- -- Para cada simbolo, mover para a direita
                passoSeis'' :: Estado -> [Simbolo] -> [Transicao]
                passoSeis'' _ [] = []
                passoSeis'' (Estado e') (s@(Simbolo s') : ss) = Transicao (Estado ("[" ++ e' ++ "]-mover-direita")) (SimboloAtual s) (SimboloSerEscrito Vazio) Direita (Estado ("[" ++ e' ++ "]-mover-direita-{"++ s' ++"}")) : passoSeis'' (Estado e') ss
                passoSeis'' (Estado e') (Vazio : ss) = Transicao (Estado ("[" ++ e' ++ "]-mover-direita")) (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Direita (Estado ("[" ++ e' ++ "]-mover-direita-{}")) : passoSeis'' (Estado e') ss

                -- Adicionando os estados de transição
                passoSeis''' :: Estado -> [Simbolo] -> [Simbolo] -> [Transicao]
                passoSeis''' _ [] _ = []
                passoSeis''' _ _ [] = []
                passoSeis''' e' (s : ss) alfabeto'' =
                    map (passoSeis'''aux e' s) alfabeto'' ++
                    map (passoSeis'''aux' e') alfabeto'' ++
                    passoSeis''' e' ss alfabeto''
                    where
                        passoSeis'''aux :: Estado -> Simbolo -> Simbolo -> Transicao
                        passoSeis'''aux (Estado e'') Vazio Vazio = Transicao (Estado ("[" ++ e'' ++ "]-mover-direita-{}")) (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Direita (Estado ("[" ++ e'' ++ "]-quase-retornar"))
                        passoSeis'''aux (Estado e'') Vazio (Simbolo simb) = Transicao (Estado ("[" ++ e'' ++ "]-mover-direita-{}")) (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo "_")) Direita (Estado ("[" ++ e'' ++ "]-mover-direita-{" ++ simb ++ "}"))
                        passoSeis'''aux (Estado e'') (Simbolo x) (Simbolo simb) = Transicao (Estado ("[" ++ e'' ++ "]-mover-direita-{" ++ x ++ "}")) (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo x)) Direita (Estado ("[" ++ e'' ++ "]-mover-direita-{" ++ simb ++ "}"))
                        passoSeis'''aux (Estado e'') (Simbolo x) Vazio = Transicao (Estado ("[" ++ e'' ++ "]-mover-direita-{" ++ x ++ "}")) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo x)) Direita (Estado ("[" ++ e'' ++ "]-mover-direita-{}"))

                        passoSeis'''aux' :: Estado -> Simbolo -> Transicao
                        passoSeis'''aux' (Estado e'') (Simbolo s') = Transicao (Estado ("[" ++ e'' ++ "]-quase-retornar")) (SimboloAtual (Simbolo s')) (SimboloSerEscrito Vazio) Esquerda (Estado ("[" ++ e'' ++ "]-mover-direita-{" ++ s' ++ "}"))
                        passoSeis'''aux' (Estado e'') Vazio = Transicao (Estado ("[" ++ e'' ++ "]-quase-retornar")) (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Esquerda (Estado ("[" ++ e'' ++ "]-retornar"))


                -- -- Ao encontrar o final da fita, retornar para o estado que encontrou o #
                passoSeis'''' :: Estado -> Transicao
                passoSeis'''' (Estado e') = Transicao {simboloSerEscrito = SimboloSerEscrito Vazio, proximoEstado = Estado ("[" ++ e' ++ "]-retornar"), simboloAtual = SimboloAtual Vazio, estadoAtual = Estado ("[" ++ e' ++ "]-mover-direita-{}"), direcao = Esquerda}

                -- -- Ao encontrar o #, retornar para o estado que encontrou o #
                -- passoSeis'''' :: Estado -> Transicao
                -- passoSeis'''' (Estado e') = Transicao {simboloSerEscrito = SimboloSerEscrito Vazio, proximoEstado = Estado e', simboloAtual = SimboloAtual (Simbolo "#"), estadoAtual = Estado ("[" ++ e' ++ "]-retornar"), direcao = Direita}
