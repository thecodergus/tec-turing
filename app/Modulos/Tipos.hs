module Tipos where


-- Define o tipo da máquina de Turing da atual configuração
data Tipo = Sipser 
          | DuplamenteInfinita 
          deriving (Show, Eq)

-- Define os símbolos usados na fita da máquina de Turing
data Simbolo
  = Simbolo String -- Símbolo que representa uma letra
  | Vazio -- Símbolo que representa o espaço vazio
  | Manter -- Símbolo que representa a ordem de manter o símbolo atual
  | Todos -- Simbolo que representa a ordem de aceitar todos os simbolos
  deriving (Show, Eq)

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
  simboloAtual :: Simbolo, -- Símbolo atual
  simboloSerEscrito :: Simbolo, -- Símbolo a ser escrito
  direcao :: Direcao, -- Direção da cabeça a ser seguida
  proximoEstado :: Estado -- Próximo estado
} deriving (Show, Eq)

-- Define a máquina de Turing
data MaquinaTuring = MaquinaTuring{
  estados :: [Estado], -- Lista de estados
  alfabeto :: [Simbolo], -- Alfabeto
  transicoes :: [Transicao], -- Lista de transições
  estadoInicial :: Estado, -- Estado inicial
  tipo :: Tipo -- Tipo da máquina
} deriving (Show, Eq)