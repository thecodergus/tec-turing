module Algoritmos.Sipser where
import Tipos
    ( Direcao(Esquerda, Direita),
      Estado(Estado),
      MaquinaTuring(MaquinaTuring),
      Simbolo(..),
      SimboloAtual(Todos, SimboloAtual),
      SimboloSerEscrito(Manter, SimboloSerEscrito),
      Tipo(DuplamenteInfinita, Sipser),
      Transicao(Transicao, estadoAtual, proximoEstado) )
import Data.List (nub)


conveterParaDuplamenteInfinita :: MaquinaTuring -> MaquinaTuring
conveterParaDuplamenteInfinita (MaquinaTuring _ _ _ _ DuplamenteInfinita) = error "A Maquina de Turing já é do tipo fita duplamente infinita"
conveterParaDuplamenteInfinita (MaquinaTuring estados alfabeto transicoes' estadoInicial Sipser) = 
    MaquinaTuring 
    estados 
    alfabeto 
    (
        passoUm transicoes'
        ++ passoDois alfabeto
        ++ passoTres alfabeto (alfabeto ++ [Vazio])
        ++ passoQuatro
        ++ passoCinco (alfabeto ++ [Vazio])
        ++ passoSeis estados
    ) 
    estadoInicial 
    DuplamenteInfinita
    where
    -- Substituir "0" por inicio
    passoUm :: [Transicao] -> [Transicao]
    passoUm [] = []
    passoUm (t@(Transicao (Estado "0") _ _ _ (Estado "0")) : ts) = t {estadoAtual = Estado "inicio", proximoEstado = Estado "inicio"} : passoUm ts
    passoUm (t@(Transicao (Estado "0") _ _ _ _) : ts) = t {estadoAtual = Estado "inicio"} : passoUm ts
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
        map (s `passoTres'`) alfabeto''
            ++ passoTres ss alfabeto''
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

    -- Caso algum dos estados chegue ao #, voltar para o estado anterior
    passoSeis :: [Estado] -> [Transicao]
    passoSeis [] = []
    passoSeis ((Estado e) : estados') = Transicao (Estado e) (SimboloAtual (Simbolo "#")) Manter Direita (Estado e) : passoSeis estados'