# Haskell
We will be talking about Haskell in these notes. The intention is to provide an outline of a two or three hour session on Haskell. 

## What makes Haskell unique?
You may have heard Haskell mentioned in strange hushed tones, talked of either of disgust, fear, or delight.  
- Purity
- Laziness
- Advanced Type system
- Declarative vs. Imperative
- Functional*
    - What does it mean to be functional?
    - overloaded word
    - many meanings some implication of a procedural style
    - many functions, pipeline of transformations
    - type compositions to go from input -> outputs

- Myth: Haskell is Hard, Truth: Haskell is radically different from most people's idea of programming and therefore scares people intially. Its like writing in English your entire life, then attempting to switch to French, it will look foreign and werid at first, but can quickly become a comforting environment.

## Syntax and Semantics 
- ML Syntax(What does that mean)?
- White space *is* signifigant(Layout Rule)
- very little syntax, function application, and case statement, if desugars to case, maps to Core
- a program is a set of function defintions.

### Layout Rule
-- Fill IN

### Types
What are types? A lot of terms are thrown around in regards to Type Systems. [[1]](http://lucacardelli.name/Papers/TypeSystems.pdf)

### Type Signatures 
Haskell has a radically different way of annotating types then any language you have seen before. Haskell's syntax is the same as the mathematical syntax used in the programing language community. Value : Type, the Haskell commitee wanting a clean syntax for lists modified it slightly by replacing the single colon with the double colon. Value :: Type. 
```haskell
4 :: Int
5.0 :: Double
"Hello" :: String
'a' :: Char
True :: Boolean
False :: Boolean
[1,2,3] :: [Int]
succ :: Int -> Int 
```

### Parametric Polymorphism 
Limited forms known as Generics, Templates.
The idea is that there are many types of functionality that don't require *any* information about the type. For example
the  funciton return 
### Types Redux

### Type Inference
Haskell has the property of being a language with type inference, this give you the bonuses of static typing, while being as concise as dynamic languages like Python, or Ruby.

In C++: 
```Cpp
template <typename T>
T max(T x, T y) {
    if (x > y)
        return x;
    else 
        return y
    }
```
In Python: 
```python
def max(x, y): 
    if x > y:
        return x
    else:
        return y
```
In Ruby: 
```ruby
def max(x, y)
    x > y ? x : y
end
```
In Scala:
```scala
def max[A](x: A, y: A): A = if (x > y) x else y
```
In Haskell:
```haskell 
-- notice we don't have to annotate the function type
max x y = if x > y then x else y
```
TODO: check this 
```c
int fibonacci(int n) {
    fib1 = 0;
    fib2 = 1;
    if (n == 0) {
        return fib1;
    } else {
        int temp;
        for(int i = 0; i < n; i++) {
            temp = fib2;
            fib2 = fib1 + fib2;
            fib1 = fib2;
        }
        return fib1;
    }
}
```

```haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
```

### Functions
Functions are the way we pacakge functionality in Haskell. There are no influence of OOP here. We can do all kinds of things with functions, but lets first look at how to define functions in Haskell. 

```haskell 
min :: Int -> Int -> Int -- Type Sig 
min x y = if x > y then y else x -- Implmentation
```

### Type Classes
Type classes are Haskell's answer to traditional polymorphism.

Common Type Classes

### Eq
```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

### Show
```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
      -- Defined in `GHC.Show'
```
### Read 
```haskell
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec ::
    Text.ParserCombinators.ReadPrec.ReadPrec [a]
      -- Defined in `GHC.Read'
```

### Num
```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

### Fractional 
```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
      -- Defined in `GHC.Real'
```
### Integral
```haskell
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
      -- Defined in `GHC.Real'
```

### Bounded 
```haskell
class Bounded a where
  minBound :: a
  maxBound :: a
      -- Defined in `GHC.Enum'
```

### Enum
```haskell
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
      -- Defined in `GHC.Enum'
```

### Let Binding
Let bindings are super cool they allow us to introduce a new set of variable bindings into scope. 
```Cpp
/* Set of Declarations */
int a = 3, b = 4, c = 5;
int result = pow(a, 2) + pow(b, 2);
bool triangle = result == pow(c, 2);

/* indv. layout? */
/* rest of code */
```

```haskell
let triangle = let a = 3, b = 4, c = 5 in
    let result = a ^ 2 + b ^ 2 in
        result == c ^ 2 in {- rest of code here -}
        
-- more straight forward way

let a = 3, b = 4, c = 5,
    result = pow(a, 2) + pow(b, 2)
    triangle = result == pow(c, 2) in {- rest of code -}
        
```
Let bindings introduce a new level of scope.

Binding declrartion encourages less code, can also be more straight forward.

### Where Binding 
Haskell also has another method for introducting variable bindings, but it is simply syntatic sugar of a let bindings.
Where bindings are not as versatile as Let bindings. They can only occur at the end of a function defintion, and they introduce
names to all branches and guards of the functions.

```haskell
foo x y = baz + bar
    where bax = quux(x)
          bar = quux(y)
```

### If Expression
The If expression is very similar to the if statemment you know and love with some minor caveats. Like the section says if is an expression in this language it returns a value. There is also the fact that since if is an expression they are nestable. They also have an extra keyword `if <pred> then <trueb> else <flaseb>`. 

```haskell
let bool = True in
    if (if (if (if b then True else False) then True else False) True else False) then 'a' else 'b'
```

### Case Expression
The case expression is a very powerful construct that can encompass all control flow behavior. If is a trival instance of the case expression:
```haskell
case p of 
    True  -> trueb
    False -> falseb
```
doing a C style switch usage:
```haskell
case i of 
    1 -> e1
    2 -> e2
    3 -> e3
    {- ... -}
```
in C:
```c
switch i {
    case: 1: e1;
    break;
    case 2: e2;
    break;
}
```

#### Digression about Pattern Matching
At this Point we haven't seen anything radically new on the semantics level, most we have just observed syntactic differences
between Haskell and C style languages. Pattern matching is pervasive in Haskell, it is by far one of the most powerful features
of the lanuage, it allows for data access and object deconstruction in a way unlike many traditional imperative languages. So
you may have a lot of ideas about what "Pattern Matching" means. My defintion is it is a syntic construct for binding componenet parts of a data structure. So what does that mean?

We introduce the concept of binders, or binding patterns which are used to bind names in a match. Here is an example of a binding pattern for Tuples.

Tuples:
```haskell
let (a, b) = (4,5)
```
What value is a? b?

We can bind values like this in many spots:
    - In a `let` binding
    - In a `where` binding
    - In a `case` expression
    - In a function defintion
    
So let's move back to `case` expressions!

### Case Expression and Functions Redux
We can see the full power of matching here

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1) 

-- take the first element of a tuple 
fst :: (a, b) -> a
fst (x, _) = x

-- take the second element of a tuple 
snd :: (a, b) -> b
snd (_, y) = y

case ("hello", "world") of 
    t@(s@"hello", _) -> "Found :" ++ (show s) ++ "in: " ++ (show t) ++ "\n"
    _                -> "Did not find \"hello\""
```
    
In order to
## Guards 

## Things with Complicated Names

### What the Functor?

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

### Pointed Functors?

```haskell 
class (Functor f) => Pointed f where
    pure :: a -> f a
```

### Applicative
```haskell
class (Pointed f) => Applicative f where
    <*> :: f (a -> b) -> f a -> f b
```

### Monad
```haskell

-- class (Applicative m) = Monad m where 
class Monad m where 
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    (>>) x y = x >>= \_ -> y
```

### Monad Transformers
Just a note, about ... 

### Monoids 
```haskell
class Monoid a where
    mconcat :: a -> a -> a
    mappend :: [a] -> a
```
# Applying it all

A Haskell program starts with a module definition, similarly to C Haskell programs all begin with the same entry point
of a function `main :: IO`, contained in a module named `Main`. 
We also see a compiler pragma at the top of the Program these are similar to preprocessor directives of the C preprocessor, 
in this case we are using a LANGUAGE pragma which allows us to enable language extensions. This may sound bad or strange, like we are using non-standard features, but many of these extensions have become ubiqutous in the community and it is quite common, for programs to use them. We will explain more about `FlexibleInstances` at a later point. 

```haskell
{-# LANGUAGE FlexibleInstances #-}
module Main where
```
Next we have a set of imports, which function very similarly to Java's import they bring a set of names into Scope, the first import brings all top-level names in System.IO into scope. That is not always advantegous though, and many times will lead to name clashes, so Haskell offers a few other ways to resolve name conflicts. The first option is to perform a qualified import which brings all names from a module into scope fully qualified. To reduce typing they also have module aliasing with they keyword `as` which by itself brings all names in, unqualified, qualified, and qualified by the new prefix.

```haskell
module TopModule.InnerModule (name1, name2, name3) where
    ...
    
import TopModule.InnerModule as IM
```
The above import then brings TopModule.InnerModule.{name1, name2, name3}, {name1, name2, name3}, IM.{name1, name2, name3}.
```haskell
import System.IO
import qualified Data.Map as M
import Debug.Trace 
```

The idea is to build a simple templating library, so let's first discuss how we build such a thing. Idea is to take a file, a stream of characters, and somehow, build a template that will take an environment and return a filled in file.

At a high level this is a simple algorithm:
    - tokenize the input
    - parse it and figure out what must be filled in.
    - compile it into a template
    - apply the template to get our output

Here is what our main function is going to look like:

```haskell    
main = do 
    args <- getArgs
    contents <- readFile (args !! 0)
    let template = (compile . parse . tokenize) contents
        result   = template env
    writeFile (args !! 0) result
    
```
TODO: talk about args, readFile, writeFile

```haskell
readFile f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    return contents 

writeFile f s = do
    handle <- openFile f WriteMode
    contents <- hPutContents handle s
    return contents
```
If we follow our above stated algorithm we first want to determine where the sections are that we are going to fill in.
We want to treat any token that isn't a open delimiter or close delimiter the same, and only give precedence to those, and
we finally are going to encode our EOF, as a token so we can cleanly see when we finish. Although it is not strictly necessary.

```haskell
-- Token Type
data Token = AnyT Char 
           | OpenSpliceT
           | CloseSpliceT
           | EOFT
           deriving (Eq)
```

So we declare a Token type that has four Data Constructors, if we encounter a sequence corresponding to a open or close delimiter we put the
corresponding token into the stream, if we encounter any other character we simply wrap it up with a AnyT constructor, when we finish reading the stream
we push a EOFT token into the stream.

TODO: Eq

```haskell 
instance Show Token where
    show (AnyT c) = show c
    show OpenSpliceT = "OpenSliceT"
    show CloseSpliceT = "CloseSliceT"
    show EOFT = "|"
```

We add this instance to make Token a member of the Show typeclass. If you remember from before Show is essentially Haskell's version of toString. Adding 
this instance allows us to play with an inspect values inside of GHCi. 

So now that we have a stream let's try to take our string input and tokenize it. 
```haskell


{- tokeknize :: String -> Token
tokenize = 
    where tokenize s
          itokenize '<':'%'
          itokenize '%':'>' -}

{- tokenize :: String -> [Token]
tokenize s = case nextToken s of {
  (EOFT, []) -> [];
  (t, rs)    -> t : nextToken rs }
    where nextToken []           = (EOFT, [])
          nextToken ('<':'%':rs) = (OpenSpliceT, rs)
          nextToken ('%':'>':rs) = (CloseSpliceT, rs)
          nextToken (c:cs)       = (AnyT c, cs) -}

type TokenStream = [Token]

tokenize :: String -> TokenStream
tokenize s = case nextToken s of {
  (EOFT, []) -> [EOFT];
  (t, rs)    -> t : tokenize rs }
    where nextToken []           = (EOFT, [])
          nextToken ('<':'%':rs) = (OpenSpliceT, rs)
          nextToken ('%':'>':rs) = (CloseSpliceT, rs)
          nextToken (c:cs)       = (AnyT c, cs)
```

```haskell
-- type Template = ???

type Env = M.Map String String 
type Template = Env -> String

{- parse :: TokenStream -> (Env -> String)
parse ts = undefined -}

instance Show (Env -> String) where 
    show _ = "F: Env -> String"

data TemplateItem = TS String | THole (Env -> String) deriving (Show)


mkHole :: String -> Template 
mkHole key = \e ->
  show $ case key `M.lookup` e of
    Just v  -> v
    Nothing -> error "undefined var"

mkTemplate :: [TemplateItem] -> (Env -> String)
mkTemplate ts = \e ->
  concat $ (flip map) ts $ \i -> case i of
    TS    s -> s
    THole h -> h e

parse :: TokenStream -> [TemplateItem]
parse ts = let (x, y, z) = foldl reduce (TS "", [], False) ts in reverse y

compile :: [TemplateItem] -> Template
compile = mkTemplate

reduce :: (TemplateItem, [TemplateItem], Bool) -> Token -> (TemplateItem, [TemplateItem], Bool)
reduce rd t = case (t, rd) of
  (OpenSpliceT, (TS s, xs, False)) -> (TS "", (TS $ reverse s):xs, True)
  (CloseSpliceT, (TS s, xs, True)) -> (TS "", (THole $ mkHole $ eatWhiteSpace $ reverse s):xs, False)
  (AnyT c, (TS s, xs, b))          -> (TS (c:s), xs, b)
  (EOFT, (TS s, xs, False))        -> (TS "", (TS $ reverse s):xs, True)
  _                                -> error "bad reasoning about function"

-- (rest, nstrm, inside) :: (Token, TI, Splicing)

eatWhiteSpace = filter (/= ' ')
```

Let's go a little further and factor it into multiple files:
### Template.hs
```haskell
module Template ...
```
### Main.hs
```haskell
module Main where

import Template ({- stuff here -}) 
```