{-# LANGUAGE GADTs #-}
module Tipos where


-- Define o tipo da máquina de Turing da atual configuração
data Tipo = Sipser 
          | DuplamenteInfinita 
          deriving (Show, Eq)

-- Define os símbolos usados na fita da máquina de Turing
data Simbolo
  = Simbolo String
  | Vazio
  deriving (Show, Eq)

--- A justifica para separar a tipagem de Simbolo Atual e Simbolo a ser escrito é que "*" ganha propriedades diferentes tanto em um quanto no outro, enquanto no simbolo atual ele se refere "a qualque simbolo" no simbolo a ser escrito ele se refere a "manter o simbolo atual"

-- Define os simbolos lidos
data SimboloAtual = SimboloAtual Simbolo
                  | Todos
                  deriving (Show)

-- Define os simbolos que serão escritos
data SimboloSerEscrito = SimboloSerEscrito Simbolo
                        | Manter
                        deriving (Show)

-- Define os estados da máquina de Turing
newtype Estado
  = Estado String -- Estado que representa um conjunto de transições
  deriving (Show, Eq)

-- Define a direção que a cabeça da máquina de Turing pode se mover
data Direcao
  = Esquerda -- Representa a direção para a esquerda
  | Direita -- Representa a direção para a direita
  | Parado -- Representa a ordem da maquina ficar parada na fita
  deriving (Show, Eq)

-- Define a transição da máquina de Turing
data Transicao = Transicao{
  estadoAtual :: Estado, -- Estado atual
  simboloAtual :: SimboloAtual, -- Símbolo atual
  simboloSerEscrito :: SimboloSerEscrito, -- Símbolo a ser escrito
  direcao :: Direcao, -- Direção da cabeça a ser seguida
  proximoEstado :: Estado -- Próximo estado
} deriving (Show)

-- Define a máquina de Turing
data MaquinaTuring = MaquinaTuring{
  estados :: [Estado], -- Lista de estados
  alfabeto :: [Simbolo], -- Alfabeto
  transicoes :: [Transicao], -- Lista de transições
  estadoInicial :: Estado, -- Estado inicial
  tipo :: Tipo -- Tipo da máquina
} deriving (Show)