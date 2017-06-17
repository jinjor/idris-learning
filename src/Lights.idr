module Lights


import Data.Fin
import Data.Vect
import Debug.Trace


public export
Lights : Nat -> Type
Lights n =
  Vect n (Vect n Bool)


data Direction =
  Center | Left | Right | Up | Down


toggleOne : Direction -> Fin n -> Fin n -> Lights n -> Lights n
toggleOne {n} direction i j rows =
  case direction of
    Center =>
      updateAt i (updateAt j not) rows
    Up =>
      case integerToFin (finToInteger i - 1) n of
        Just i' =>
          updateAt i' (updateAt j not) rows
        Nothing =>
          rows
    Down =>
      case integerToFin (finToInteger i + 1) n of
        Just i' =>
          updateAt i' (updateAt j not) rows
        Nothing =>
          rows
    Left =>
      case integerToFin (finToInteger j - 1) n of
        Just j' =>
          updateAt i (updateAt j' not) rows
        Nothing =>
          rows
    Right =>
      case integerToFin (finToInteger j + 1) n of
        Just j' =>
          updateAt i (updateAt j' not) rows
        Nothing =>
          rows


directions : Prelude.List.List Direction
directions =
  [ Center, Left, Right, Up, Down]


export
toggle : Fin n -> Fin n -> Lights n -> Lights n
toggle i j rows =
  foldl (\rs, d => toggleOne d i j rs) rows directions


export
empty : (n : Nat) -> Lights n
empty n =
  Data.Vect.replicate n (Data.Vect.replicate n False)


formatRow : Vect n Bool -> String
formatRow Nil = ""
formatRow (True :: xs) = "□ " ++ formatRow xs
formatRow (Flase :: xs) = "■ " ++ formatRow xs



export
format : Vect m (Vect n Bool) -> String
format Nil = ""
format (r :: rows) =
  "  " ++ formatRow r ++ "\n" ++ format rows
