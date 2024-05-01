main :: IO ()
main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))
  print(luhn16 [1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8])
  where
    luhnDouble :: Integer -> Integer
    luhnDouble d = if n > 9 then n-9 else n
      where n = d*2
    alldigits :: [Integer] -> Bool
    alldigits [] = True
    alldigits (x:xs) | (x>=0) && (x<=9) = alldigits xs
                     | otherwise= False
    valid :: [Integer] -> Bool
    valid xs = (length xs == 16) && (alldigits xs)
    luhnalg :: [Integer] -> [Integer]
    luhnalg [] = []
    luhnalg [x] = [x]
    luhnalg (x:y:zs)
             | 2*x >9 = 2*x -9 : y : luhnalg zs
             | otherwise = 2*x : y : luhnalg zs
    luhncriteria :: [Integer] -> Bool
    luhncriteria xs = v `mod` 10 ==0
      where v= sum (luhnalg xs)
    luhn16 :: [Integer] -> Bool
    luhn16 xs = (valid xs) && (luhncriteria xs)
    



