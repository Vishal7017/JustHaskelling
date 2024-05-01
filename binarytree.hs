import GHC.Base (TrName(TrNameD))
import Distribution.Simple.Utils (xargs)
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

makeNode :: a -> Tree a -> Tree a -> Tree a
makeNode x left right = Node x left right

isEmptyTree :: Tree a -> Bool
isEmptyTree EmptyTree = True
isEmptyTree _ = False

rootNode :: Tree a -> a
rootNode (Node x _ _) = x

leftTree :: Tree a -> Tree a
leftTree (Node _ left _ ) = left

rightTree :: Tree a -> Tree a 
rightTree (Node _ _ right) = right

insertElement :: (Ord a) => a -> Tree a -> Tree a
insertElement x EmptyTree = makeNode x EmptyTree EmptyTree
insertElement x (Node y left right)
    | x == y = Node y left right
    | x < y = makeNode y (insertElement x left) right
    | otherwise = makeNode y left (insertElement x right)

{-
elementInTree :: (Ord a) => a -> Tree a -> Tree a
elementInTree x EmptyTree = False
elementInTree x (Node y left right)
    | x == y = True
    | x < y = elementInTree x left
    | otherwise = elementInTree x right

-}

preOrder :: Tree a -> [a]
preOrder EmptyTree = []
preOrder (Node x left right) = x : (preOrder left) ++ (preOrder right)

inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Node x left right) = (inOrder left) ++ [x] ++ (inOrder right)

postOrder :: Tree a -> [a]
postOrder EmptyTree = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

