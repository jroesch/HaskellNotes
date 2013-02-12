-- Haskell Examples of Recursion
-- Jared Roesch, 2012

-- Basic Recursion Examples
-- Fibonacci Sequence 
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Factorial 
fact 0 = 1
fact n = n * fact n - 1

-- Recursing Over a List
double []     = []
double (x:xs) = 2*x : double xs