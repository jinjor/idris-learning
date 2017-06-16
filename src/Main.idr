module Main


readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing


main : IO ()
main =
  putStrLn "Hey"
  -- do Just x <- readNumber | Nothing => main
  --    putStrLn (show x)
  --    main
