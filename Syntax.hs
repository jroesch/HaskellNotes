-- Introduction to Haskell's Syntax
-- Jared Roesch 2012

-- Haskell obeys relatively traditional primative operators, for example
2 * 2 == 4 
2 / 2 == 1 
2 + 2 == 4

-- There are a few differences such as, not equals is \= not !=
2 \= 4

-- Haskell also has the primative types you would expect

-- Bool
True

-- Int
2

-- Double
3.14

-- Char
'A'

-- String (is an alias for [Char])
"String"

-- [](List)
[1, 2, 3]

-- The basic unit of Haskell is the function
-- A function is defined by the form <id> [<args>] = <body>

-- A doubling function 
double x = x + x

-- We can call this function now by simply writing
double 2 == 4

-- Functions can be defined over their domain piecewise, just like in Math
fact 0 = 1
fact n = n * fact n - 1

-- It matches the cases starting with the most specific to the least specific 

