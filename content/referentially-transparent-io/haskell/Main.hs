
pureAdd :: Int -> Int -> Int
pureAdd x y = x + y

impureAdd :: Int -> Int -> IO Int
impureAdd x y = do
  putStrLn "I'm going to add now" 
  return (x + y)

main :: IO ()
main = do
  result <- do
    putStrLn "I'm going to add now" 
    return (2 + 3)
  putStrLn $ "2 + 3 = " ++ show (result)
  return ()
