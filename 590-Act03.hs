
-- This is luhn algorithm
> luhnDouble :: Int -> Int
> luhnDouble d = if n > 9 then n-9 else n
> where n = d*2
> luhn :: Int -> Int -> Int -> Int -> Bool
> luhn a b c d = ((x + b + y + d) `mod` 10) == 0
> where
> x = luhnDouble a
> y = luhnDouble c



-- Vishal Dnyaneshwar Wavhale (590)
-- Activity 3

alldigits :: [Integer] -> Bool
alldigits [] = True
alldigits (x:xs) | (x>=0) && (x<=9) = alldigits xs
                 | otherwise= False


valid :: [Integer] -> Bool
valid xs = (length xs == 16) && (alldigits xs)


luhnalg :: [Interger] -> [Integer]
luhnalg [] = []
luhnalg [x] = [x]
luhnalg (x:y:zs)
         | 2*x >9 = 2*x -9 : y : luhnalg zs
         | otherwise = 2*x : y : luhnalg zs

luhncriteria :: [Integer] -> Bool
luhncriteria xs = v 'mod' 10 ==0
where v= sum (luhnalg xs)

luhn16 :: [Integer] -> Bool
luhn16 = (valid xs) && (luhncriteria xs)

main :: IO ()
main =  do
print(luhn16 [1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8])
