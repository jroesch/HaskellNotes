module Homework.One.Code where 
{- HomeworkOne Solutions, Jared Roesch -}
data List a = Cons a (List a) | Nil deriving (Show, Read, Eq)

-- define a function cons' that is the equivalent to (:)
cons' :: a -> List a -> List a
cons' x y = Cons x y

-- returns the two list joined
append' :: List a -> List a -> List a
append' Nil y = y
append' x Nil = x
append' (Cons x xs) y = x `Cons` (append' xs y)

-- returns the first element of the list
head' :: List a -> a
head' Nil = error "Can't call head on an empty list."
head' (Cons x _) = x

-- returns everything but the head of a list
tail' :: List a -> List a
tail' Nil = error "Empty List"
tail' (Cons _ xs) = xs

-- returns every element of a list but the last
init' :: List a -> List a
init' Nil          = error "Empty List"
init' (Cons x Nil) = Nil
init' (Cons x xs)  = Cons x (init' xs)

-- returns the last element of the list
last' :: List a -> a
last' Nil          = error "Empty List"
last' (Cons x Nil) = x
last' (Cons _ xs)  = last' xs

-- returns true if a list is empty 
null' :: List a -> Bool
null' Nil = True
null' _   = False

-- returns n elements from a list
take' :: Int -> List a -> List a    
take' n _ | n <= 0 = Nil
take' _ Nil        = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

-- returns true if an elem exists in a list
elem' :: Eq a => a -> List a -> Bool
elem' e (Cons x Nil) = if e == x then True else False
elem' e (Cons x xs)  = if e == x then True else (elem' e xs)

-- returns true if an element is not in a list
notElem' :: Eq a => a -> List a -> Bool
notElem' e xs = not $ elem' e xs

-- returns removes n elements from the front a list
drop' :: Int -> List a -> List a
drop'  _  Nil = Nil
drop'  n  xs | n <= 0 = xs
drop' n (Cons x xs) = drop' (n - 1) xs

-- returns the length of list
length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + (length' xs)

-- takes one element and returns an infinite list of the element
repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

-- takes a list and returns an infinite list composed of the list
cycle' :: List a -> List a
cycle' xs = xs `append'` cycle' xs

-- takes two lists and returns a list of tuples
-- note: only goes as far as the shortest list
zip' :: List a -> List b -> List (a, b)
zip' _ Nil                   = Nil
zip' Nil _                   = Nil
zip' (Cons x xs) (Cons y ys) = Cons (x, y) (zip' xs ys)

-- returns a list intersperesed with an element 
-- ex intersperse 4 [1,2,3] => [1,4,2,4,3]
intersperse :: a -> List a -> List a
intersperse y xs = join y xs
    where join s (Cons x Nil) = (Cons x Nil)
          join s (Cons x xs)  = Cons x (Cons x (join s xs))
          
{- Section 2: Tuples
   This section is a small section focused on the use of tuples and the pattern matching syntax 
   that can be used to extract values from tuples.
-}

-- Functions that operate on two-tuples

-- Returns the first element of the tuple
fst' :: (a, b) -> a
fst' (a, _) = a

-- Returns the second element of the tuple
snd' :: (a, b) -> b
snd' (_, b) = b

-- define functions that operate on three-tuples
fst3 :: (a,b,c) -> a
fst3 (a, _, _) = a

snd3 :: (a,b,c) -> b
snd3 (_, b, _) = b

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

{- Section 3: Trees
   This section is focused on the implementation of a binary tree using algebraic data types.
   There are also provided function signatures and descriptions for you to implement.
-}

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show, Read)

height :: Tree a -> Int
height = undefined 

findElem :: a -> Tree a -> Bool
findElem = undefined 

insert :: a -> Tree a -> Tree a
insert = undefined 

remove :: a -> Tree a -> Tree a
remove = undefined
