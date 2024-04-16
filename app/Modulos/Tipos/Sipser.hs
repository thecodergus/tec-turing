module Tipos.Sipser where

import Tipos.Geral ( Transicao, Estado, Simbolo )



-- Define a fita da máquina de Turing da definição do Sipser
newtype Fita
  = Fita String -- String que representa a fita
  deriving (Show, Eq)

-- Define a máquina de Turing da definição do Sipser
data MaquinaTuring
  = MaquinaTuring
  { estados :: [Estado], -- Lista de estados
    simbolos :: [Simbolo], -- Lista de símbolos
    estadoInicial :: [Estado], -- Estado inicial
    estadoFinal :: [Estado], -- Estado final
    fita :: Fita, -- Fita
    posicaoCabeca :: Int, -- Posição da cabeça na fita
    estadoAtual :: Estado, -- Estado atual
    transicoes :: [Transicao] -- Lista de transições
  } deriving (Show, Eq)