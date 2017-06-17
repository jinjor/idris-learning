module Main


import Data.Fin
import Data.Vect
import Lights


size : Nat
size = 5


data Model =
  Start | Playing (Lights Main.size)


init : Model
init =
  Start


data Msg
  = NoOp
  | StartGame
  | Toggle (Position Main.size)


charToFin : Char -> Maybe (Fin Main.size)
charToFin char =
  if isDigit char then
    natToFin (cast $ singleton char) size
  else
    Nothing


readInput : (String -> Msg) -> IO Msg
readInput decode = do
  input <- getLine
  pure (decode input)


decodePosition : String -> Msg
decodePosition input =
  case unpack input of
    [ a, b ] =>
      case (charToFin a, charToFin b) of
        (Just i, Just j) =>
          Toggle (position i j)

        _ =>
          NoOp

    _ =>
      NoOp


update : Msg -> Model -> Model
update msg model =
  case (msg, model) of
    (NoOp, _) =>
      model

    (StartGame, _) =>
      Playing $ toggle (position 2 2) (empty size)

    (Toggle position, Playing lights) =>
      Playing $ toggle position lights

    _ =>
      model


view : Model -> (String, String -> Msg)
view model =
  case model of
    Start =>
      ("Press any key to start.", \_ => StartGame)

    Playing lights =>
      if isEmpty lights then
        ("Cleared! Press any key to continue.", \_ => StartGame)
      else
        (format lights, decodePosition)


loop : Model -> IO ()
loop model =
  do let (s, decoder) = view model
     putStrLn s
     msg <- readInput decoder
     loop (update msg model)


main : IO ()
main =
  loop init
