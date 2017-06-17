module Lights


import Data.Fin
import Data.Vect
import Debug.Trace


public export
Lights : Nat -> Type
Lights n =
  Vect n (Vect n Bool)


export
Position : Nat -> Type
Position n =
  (Fin n, Fin n)


export
position : {n : Nat} -> Fin n -> Fin n -> Position n
position x y =
  (x, y)


move : Integer -> Integer -> Position n -> Maybe (Position n)
move {n} dx dy (x, y) =
  case (integerToFin (finToInteger x + dx) n, integerToFin (finToInteger y + dy) n) of
    (Just x, Just y) => Just (x, y)
    _ => Nothing


toggleOne : Position n -> Lights n -> Lights n
toggleOne (x, y) rows =
  updateAt x (updateAt y not) rows


export
toggle : Position n -> Lights n -> Lights n
toggle position rows =
  foldl (\r, p => toggleOne p r) rows $
    Prelude.List.catMaybes
      [ Just position
      , move 0 1 position
      , move 0 (-1) position
      , move 1 0 position
      , move (-1) 0 position
      ]


export
empty : (n : Nat) -> Lights n
empty n =
  Data.Vect.replicate n (Data.Vect.replicate n False)


export
isEmpty : Lights n -> Bool
isEmpty rows =
  all (\row => all not row) rows


formatRow : Vect n Bool -> String
formatRow Nil = ""
formatRow (True :: xs) = "□ " ++ formatRow xs
formatRow (Flase :: xs) = "■ " ++ formatRow xs



export
format : Vect m (Vect n Bool) -> String
format Nil = ""
format (r :: rows) =
  "  " ++ formatRow r ++ "\n" ++ format rows
