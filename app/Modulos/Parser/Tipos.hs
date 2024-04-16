module Parser.Tipos where

-- | Representa uma instrução em uma máquina de Turing.
data Instrucao = Instrucao
  { estadoAtual :: String,    -- ^ O estado atual da máquina de Turing.
    simboloAtual :: Char,     -- ^ O símbolo atual lido pela máquina de Turing.
    novoSimbolo :: Char,      -- ^ O novo símbolo a ser escrito pela máquina de Turing.
    direcao :: Char,          -- ^ A direção em que a cabeça da máquina de Turing deve se mover.
    novoEstado :: String      -- ^ O novo estado da máquina de Turing.
  }
  deriving (Show)