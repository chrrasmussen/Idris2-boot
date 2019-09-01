
main : IO ()
main = do
  putStr "Start"
  fork (do { putStrLn "Called from thread #1" })
  fork (do { putStrLn "Called from thread #2" })
  putStr "End"
