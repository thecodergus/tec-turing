module Algoritmos.Sipser where
import Tipos
    ( Direcao(Direita, Esquerda, Parado),
      SimboloSerEscrito(SimboloSerEscrito, Manter),
      SimboloAtual(SimboloAtual, Todos),
      Tipo(DuplamenteInfinita, Sipser),
      Transicao(Transicao, estadoAtual),
      Simbolo(..),
      Estado(Estado),
      MaquinaTuring(MaquinaTuring, transicoes) )
import Data.List (nub)


conveterParaDuplamenteInfinita :: MaquinaTuring -> MaquinaTuring
conveterParaDuplamenteInfinita (MaquinaTuring _ _ _ _ DuplamenteInfinita) = error "A Maquina de Turing já é do tipo fita duplamente infinita"
conveterParaDuplamenteInfinita (MaquinaTuring estados alfabeto transicoes' estadoInicial Sipser) = MaquinaTuring estados alfabeto (passoUm transicoes' ++ passoDois alfabeto ++ passoTres alfabeto ++ passoQuatro estados) estadoInicial DuplamenteInfinita
    where
        -- Substituir estado "0" por estado inicio
        passoUm :: [Transicao] -> [Transicao]
        passoUm [] = []
        passoUm ((Transicao (Estado "0") simbAtual simbSerEscrito direcao (Estado "0")) : xs) = Transicao (Estado "inicio-s") simbAtual simbSerEscrito direcao (Estado "inicio-s") : passoUm xs
        passoUm ((Transicao (Estado "0") simbAtual simbSerEscrito direcao paraEstado) : xs) = Transicao (Estado "inicio-s") simbAtual simbSerEscrito direcao paraEstado : passoUm xs
        passoUm ((Transicao estadoAtual' simbAtual simbSerEscrito direcao (Estado "0")) : xs) = Transicao estadoAtual' simbAtual simbSerEscrito direcao (Estado "inicio-s") : passoUm xs
        passoUm ((Transicao estadoAtual' simbAtual simbSerEscrito direcao paraEstado) : xs) = Transicao estadoAtual' simbAtual simbSerEscrito direcao paraEstado : passoUm xs

        -- Adicionar meu estado inicial "0", o que inclui o uso de # e mover os itens para a direita
        passoDois :: [Simbolo] -> [Transicao]
        passoDois alfabeto' = estado0 alfabeto' ++ estadosMover alfabeto' alfabeto'
            where
                estado0 :: [Simbolo] -> [Transicao]
                estado0 = map aux
                    where
                        aux :: Simbolo -> Transicao
                        aux Vazio = Transicao (Estado "0") (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo "#")) Direita (Estado "mover-direita-vazio")
                        aux s@(Simbolo simb) = Transicao (Estado "0") (SimboloAtual s) (SimboloSerEscrito (Simbolo "#")) Direita (Estado ("mover-direita-" ++ simb))

                -- mover-x :: {a,b,c,d...}, x, r, mover-{a,b,c,d...}
                estadosMover :: [Simbolo] -> [Simbolo] -> [Transicao]
                estadosMover [] _ = []
                estadosMover _ [] = []
                estadosMover (a : as) alfabetoAux = map (a `aux`) alfabetoAux ++ estadosMover as alfabetoAux
                    where
                        aux :: Simbolo -> Simbolo -> Transicao
                        aux Vazio Vazio = Transicao (Estado "mover-direita-vazio") (SimboloAtual Vazio) (SimboloSerEscrito Vazio) Esquerda (Estado "retorno-vazio") 
                        aux Vazio (Simbolo simb) = Transicao (Estado "mover-direita-vazio") (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo "_")) Direita (Estado ("mover-direita-" ++ simb))
                        aux (Simbolo x) (Simbolo simb) = Transicao (Estado ("mover-direita-" ++ x)) (SimboloAtual (Simbolo simb)) (SimboloSerEscrito (Simbolo x)) Direita (Estado ("mover-direita-" ++ simb))
                        aux (Simbolo x) Vazio = Transicao (Estado ("mover-direita-" ++ x)) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo x)) Direita (Estado "mover-direita-vazio")

        -- Apos encontrar branco duas vezes, voltar para a esquerda até achar o #
        passoTres :: [Simbolo] -> [Transicao]
        passoTres alfabeto'' = map (\(Simbolo s) -> Transicao (Estado ("mover-direita-" ++ s)) (SimboloAtual Vazio) (SimboloSerEscrito (Simbolo s)) Esquerda (Estado "retornar-hashtag")) alfabeto'' ++ [Transicao (Estado "retornar-hashtag") (SimboloAtual (Simbolo "#")) Manter Direita (Estado "inicio-s"), Transicao (Estado "retornar-hashtag") Todos Manter Esquerda (Estado "retornar-hashtag")]


        passoQuatro :: [Estado] -> [Transicao]
        passoQuatro [] = []
        passoQuatro ((Estado e) : estados') = Transicao (Estado e) (SimboloAtual (Simbolo "#")) Manter Direita (Estado e) : passoQuatro estados'