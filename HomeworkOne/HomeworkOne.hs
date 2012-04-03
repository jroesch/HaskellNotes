-- HomeworkOne 
{-  Section One: Lists
    The main purpose of this Section is too understand how to use types to deduce behavior 
    of functions from their type signatures, as well as exploring Algebraic Data Types through 
    the process reimplementing the built in List. The assignment also covers the implementation
    of a simple Binary Tree and some basic operations that will be useful 
-}


data List a = {- Insert Definition Here -} deriving (Show, Read)

cons' :: a -> List a -> List a
cons' = undefined 

append' :: List a -> List a -> List a
append' = undefined 

head' :: List a -> a
head' x = undefined 

tail' :: List a -> List a
tail' x = undefined 

init' :: List a -> List a
init' = undefined 

last' :: List a -> a
last' = undefined 

take' :: Int -> List a -> List a
take' = undefined

drop' :: Int -> List a -> List a
drop' = undefined 

repeat' :: a -> List a
repeat' = undefined 

cycle' :: List a -> List a
cycle' = undefined 

zip' :: List a -> List b -> List (a, b)
zip' = undefined 

data Tree a = {- Insert Definition Here -} deriving (Show, Read)

{- Section 2: Tuples
   This section is a small section focused on the use of tuples and the pattern matching syntax 
   that can be used to extract values from tuples.
-}

fst :: (a, b) -> a
fst = undefined

snd :: (a, b) -> b
snd = undefined

{- Section 3: Trees
   This section is focused on the implementation of a binary tree using algebraic data types.
   There are also provided function signatures and descriptions for you to implement.
-}

data Tree a = {- Insert Implementation Here -} deriving (Show, Read)