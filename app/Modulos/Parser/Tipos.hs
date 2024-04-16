module Parser.Tipos where


data TuringTuple = TuringTuple
  { currentState :: String,
    currentSymbol :: Char,
    newSymbol :: Char,
    direction :: Char,
    newState :: String
  }
  deriving (Show)