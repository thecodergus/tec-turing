module Tipos.DuplamenteInfinita where


import Tipos.Geral (Transicao, Estado, Simbolo)


-- Define a fita da máquina de Turing Duplamente Infinita
data Fita
  = Fita
  { esquerda :: Maybe Fita, -- Parte da fita à esquerda da cabeça
    atual :: Simbolo, -- Símbolo atual
    direita :: Maybe Fita -- Parte da fita à direita da cabeça
  } deriving (Show, Eq)
  

-- Define a máquina de Turing Duplamente Infinita
data MaquinaTuring
  = MaquinaTuring
  { estados :: [Estado], -- Lista de estados
    simbolos :: [Simbolo], -- Lista de símbolos
    estadoInicial :: [Estado], -- Estado inicial
    estadoFinal :: [Estado], -- Estado final
    fita :: Fita, -- Fita
    estadoAtual :: Estado, -- Estado atual
    transicoes :: [Transicao] -- Lista de transições
  } deriving (Show, Eq)