module Parser.Tipos where


-- | Definição do tipo de dado 'TuringTuple' que representa uma tupla de transição em uma máquina de Turing.
data TuringTuple = TuringTuple
  { currentState :: String,    -- ^ O estado atual da máquina de Turing.
    currentSymbol :: Char,     -- ^ O símbolo atual lido pela máquina de Turing.
    newSymbol :: Char,         -- ^ O novo símbolo a ser escrito pela máquina de Turing.
    direction :: Char,         -- ^ A direção em que a cabeça de leitura/escrita da máquina de Turing deve se mover.
    newState :: String          -- ^ O novo estado em que a máquina de Turing deve entrar.
  }
  deriving (Show)