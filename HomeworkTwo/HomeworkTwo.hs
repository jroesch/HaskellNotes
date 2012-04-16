-- HomeworkTwo
-- Spring 2012, Jared Roesch
--
-- This assignment is focused on the implementation of combinators and understanding of currying, higher order fuctions.
-- There will be one more short assignment focusing on Monads and Typeclasses.

-- These can all be found in the Prelude, as Prelude.map(just map in ghci), they will be prefixed with m here to
-- diferentiate from the builtin names. 

mMap :: (a -> b) -> [a] -> [b]
mMap = undefined 

mFoldl :: (a -> b -> a) -> a -> [b] -> a
mFoldl = undefined

mFoldr :: (a -> b -> b) -> b -> [a] -> b
mFoldr = undefined

mFilter' :: (a -> Bool) -> [a] -> [a]
mFilter = undefined

-- use function currying to define a doubling function with just (+)
doubleMe :: (Num a) => a -> a
doubleMe = undefined

-- write a function in point-free style(i.e no explicit parameters) that returns all uppercase letters
-- use isUpper, and filter
upperCaseLetters :: [Char] -> [Char]
upperCaseLetters = undefined 

-- compose functions to take a string a split it into lines then words
-- you can look at the funtions lines and words to do this, think about
-- function composition
linesAndWords :: String -> [[String]]
linesAndWords = undefined 

-- Let's revist our List type from last time, and do some cool things with it
data List a = a `Cons` (List a) | Nil deriving (Read)

-- Lets define an operator that acts as cons

(<:>) :: a -> List a -> List a
(<:>) = undefined 

-- We didn't use derive above for Show and Eq so let's now manually 
-- make List a memeber of the Show, and Eq typeclasses.

-- Show defines how a type is converted to a string, make it so
-- a List a prints just like [a].

instance (Show a) => Show (List a) where
    show = undefined

-- Eq defined how a type is compared for equality, make it so we 
-- can compare to Lists equal.

instance (Eq a) => Eq (List a) where
    (==) = undefined
    (\=) = undefined
