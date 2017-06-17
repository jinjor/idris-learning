module Main


import Data.Fin
import Data.Vect
import Lights


size : Nat
size = 5


Model : Type
Model =
  Lights Main.size


init : Model
init =
  empty size


data Msg =
  Valid (Fin Main.size) (Fin Main.size) | Invalid


charToFin : Char -> Maybe (Fin Main.size)
charToFin char =
  if isDigit char then
    natToFin (cast $ singleton char) size
  else
    Nothing


readInput : IO Msg
readInput = do
  input <- getLine
  case unpack input of
    [ a, b ] =>
      case (charToFin a, charToFin b) of
        (Just i, Just j) =>
          pure $ Valid i j

        _ =>
          pure Invalid

    _ =>
      pure Invalid


update : Msg -> Model -> Model
update msg model =
  case msg of
    Valid i j =>
      toggle i j model

    Invalid =>
      model


view : Model -> String
view model =
  format model


loop : Model -> IO ()
loop model =
  do putStrLn (view model)
     msg <- readInput
     loop (update msg model)


main : IO ()
main =
  loop init
