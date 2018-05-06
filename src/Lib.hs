module Lib where

-- My own list append 
append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : append xs ys

-- List update
replace :: Int -> a -> [a] -> [a]
replace 0 v (x : xs) = v : xs
replace n v (x : xs) = x : replace (n-1) v xs


-- list suffixes
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)


-- Binary search trees
data Tree a = Branch a (Tree a) (Tree a)
            | Empty
            deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = Branch v Empty Empty
insert v (Branch x l r)
  | v <= x    = Branch x (insert v l) r
  | otherwise = Branch x l (insert v r)


-- build a tree from a list
makeTree :: Ord a => [a] -> Tree a
makeTree [] = Empty
makeTree (x:xs) = insert x (makeTree xs)

