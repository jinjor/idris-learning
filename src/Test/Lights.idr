module Test.Lights

import Lights

%access export


assertEqual : Eq a => a -> a -> IO ()
assertEqual a b =
  if a == b
  then putStrLn "ok"
  else putStrLn "not ok"


testSome : IO ()
testSome =
  assertEqual (1 + 1) 2


testAnother : IO ()
testAnother =
  assertEqual (1 + 1) 1
