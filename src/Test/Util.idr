module Test.Util

import My.Util

%access export


assertEqual : Eq a => a -> a -> IO ()
assertEqual a b =
  if a == b
  then putStrLn "ok"
  else putStrLn "not ok"


testSome : IO ()
testSome =
  assertEqual (plus 1 1) 2


testAnother : IO ()
testAnother =
  assertEqual (plus 1 1) 1
