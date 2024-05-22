module Algoritmos.DuplamenteInfinita where
import Tipos


converterParaSipser :: MaquinaTuring -> MaquinaTuring
converterParaSipser (MaquinaTuring _ _ _ _ Sipser) = error "A Maquina de Turing já é do tipo fita Sipser"
converterParaSipser (MaquinaTuring estados' alfabeto' transicoes' estadoInicial' _) = MaquinaTuring estados' alfabeto' (passoUm transicoes') estadoInicial' Sipser
        where
        -- Substituir "0" por inicio
        passoUm :: [Transicao] -> [Transicao]
        passoUm [] = []
        passoUm (t@(Transicao (Estado "0") _ _ _ (Estado "0")) : ts) = t {estadoAtual = Estado "inicio", proximoEstado = Estado "inicio"} : passoUm ts
        passoUm (t@(Transicao (Estado "0") _ _ _ _) : ts) = t{estadoAtual = Estado "inicio"} : passoUm ts
        passoUm (t@(Transicao _ _ _ _ (Estado "0")) : ts) = t {proximoEstado = Estado "inicio"} : passoUm ts
        passoUm (t : ts) = t : passoUm ts

        -- Adicionando nova configuração inicial
        passoDois :: [(Simbolo, Simbolo)] -> [Transicao]
        passoDois [] = []
        passoDois ((Vazio, Vazio) : ss) = Transicao Estado SimboloAtual SimboloSerEscrito Direcao Estado : passoDois ss
        passoDois ((Vazio, Simbolo b) : ss) = Transicao Estado SimboloAtual SimboloSerEscrito Direcao Estado : passoDois ss
        passoDois ((Simbolo a, Vazio) : ss) = Transicao Estado SimboloAtual SimboloSerEscrito Direcao Estado : passoDois ss
        passoDois ((Simbolo a, Simbolo b) : ss) = Transicao Estado SimboloAtual SimboloSerEscrito Direcao Estado : passoDois ss
