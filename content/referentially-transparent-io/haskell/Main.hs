
pureAdd :: Int -> Int -> Int
pureAdd x y = x + y

impureAdd :: Int -> Int -> Int
impureAdd x y = undefined

main :: IO ()
main = putStrLn $ "2 + 3 = " ++ show (pureAdd 2 3)
