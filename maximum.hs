

mymaximum :: Ord a -> [a] -> a
mymaximum [] = error "empty list"
mymaximum [x] = x
mymaximum (x: xs) = max x (mymaximum xs)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (xs: xss) = xs ++ myconcat xss

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x: xs) = myreverse xs ++ [x]

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n (x : xs) = mydrop (n - 1) xs

mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ [] = []
mytake n (x: xs) = x : mytake (n - 1) xs

myzip :: [a] -> [b] -> [(a, b)]
myzip [] _  = []
myzip _ [] = []
myzip (x: xs) (y:ys) = (x, y) : myzip xs ys

myelement :: Eq a => a -> [a] -> Bool
myelement - [] = False
myelement x (y : ys) | x == y = True
                     | otherwise = myelement x ys


mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x: xs) = f x : mymap f xs
                    