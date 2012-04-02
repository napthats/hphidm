f :: IO ()
f = do
  putStrLn "hi"
  x <- g
  putStrLn (show x)
    
g :: IO Int
g = do return 0
