-- HomeworkOne 
{-  Section One: Lists
    The main purpose of this Section is to understand using types signatures to deduce behavior 
    of functions, as well as exploring Algebraic Data Types through the process reimplementing 
    the built in List. The assignment also covers the implementation of a simple Binary Tree and 
    some basic operations that will be useful to us. Each of these functions have in Section One
    have an equivalent function in the Prelude that operates on built in Lists. I encourage you
    to use those functions to figure out the behavior of these functions when reimplementing them.
    You can also reference Chapter 3, 4 and 8 in the Learn You a Haskell for Great Good book, or the first few chapters of the Real World Haskell book.
-}

-- Note all function have ' appended to them is is just part of the identifier and not a piece 
-- of syntax. This is to disambiguate them from the Prelude functions.

-- Definition of our List we will cover this in class
data List a = Cons a (List a) | Nil deriving (Show, Read)

-- define a function cons' that is the equivalent to (:)
cons' :: a -> List a -> List a
cons' = undefined 

-- returns the two list joined
append' :: List a -> List a -> List a
append' = undefined 

-- returns the first element of the list
head' :: List a -> a
head' x = undefined 

-- returns everything but the head of a list
tail' :: List a -> List a
tail' x = undefined 

-- returns every element of a list but the last
init' :: List a -> List a
init' = undefined 

-- returns the last element of the list
last' :: List a -> a
last' = undefined 

-- returns true if a list is empty 
null' :: List a -> Bool
null' = undefined 

-- returns n elements from a list
take' :: Int -> List a -> List a
take' = undefined

-- returns true if an elem exists in a list
elem' :: Eq a => a -> List a -> Bool
elem = undefined 

-- returns true if an element is not in a list
notElem' :: Eq a => a -> List a -> Bool
notElem = undefined 

-- returns removes n elements from the front a list
drop' :: Int -> List a -> List a
drop' = undefined 

-- returns the length of list
length' :: List a -> Int
length = undefined 

-- takes one element and returns an infinite list of the element
repeat' :: a -> List a
repeat' = undefined 

-- takes a list and returns an infinite list composed of the list
cycle' :: List a -> List a
cycle' = undefined 

-- takes two lists and returns a list of tuples
-- note: only goes as far as the shortest list
zip' :: List a -> List b -> List (a, b)
zip' = undefined 

-- returns a list intersperesed with an element 
-- ex intersperse 4 [1,2,3] => [1,4,2,4,3]
intersperse :: a -> List a -> List a
intersperse = undefined 

{- Section 2: Tuples
   This section is a small section focused on the use of tuples and the pattern matching syntax 
   that can be used to extract values from tuples.
-}

-- Functions that operate on two-tuples

-- Returns the first element of the tuple
fst' :: (a, b) -> a
fst' = undefined

-- Returns the second element of the tuple
snd' :: (a, b) -> b
snd' = undefined

-- define functions that operate on three-tuples
fst3 :: (a,b,c) -> a
fst3 = undefined 

snd3 :: (a,b,c) -> b
snd3 = undefined 

thrd3 :: (a, b, c) -> c
thrd3 = undefined 

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
