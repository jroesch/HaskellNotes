{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.IO
import qualified Data.Map as M
import Debug.Trace
import System.Environment (getArgs)

-- Let's first build a simple parser

-- Token Type
data Token = AnyT Char 
           | OpenSpliceT
           | CloseSpliceT
           | EOFT
           deriving (Eq)

instance Show Token where
    show (AnyT c) = show c
    show OpenSpliceT = "OpenSliceT"
    show CloseSpliceT = "CloseSliceT"
    show EOFT = "|"

data TemplateItem = TS String 
                  | THole (Env -> String) 
                  deriving (Show)

type TokenStream = [Token]
type Env = M.Map String String 

type Template = Env -> String

instance Show Template where 
    show _ = "F: Env -> String"

type ParserState = (TemplateItem, [TemplateItem], Bool)

tokenize :: String -> TokenStream
tokenize s = case nextToken s of {
  (EOFT, []) -> [EOFT];
  (t, rs)    -> t : tokenize rs }
    where nextToken []           = (EOFT, [])
          nextToken ('<':'%':rs) = (OpenSpliceT, rs)
          nextToken ('%':'>':rs) = (CloseSpliceT, rs)
          nextToken (c:cs)       = (AnyT c, cs)

mkHole :: String -> Template 
mkHole key = \e ->
  case key `M.lookup` e of
    Just v  -> v
    Nothing -> error "undefined var"

mkTemplate :: [TemplateItem] -> (Env -> String)
mkTemplate ts = \e ->
  concat $ (flip map) ts $ \i -> case i of
    TS    s -> s
    THole h -> h e

parse :: TokenStream -> [TemplateItem]
parse ts = let (x, y, z) = foldl nextState (TS "", [], False) ts in reverse y

compile :: [TemplateItem] -> Template
compile = mkTemplate

nextState :: ParserState -> Token -> ParserState
nextState rd t = case (t, rd) of
  (OpenSpliceT, (TS s, xs, False)) -> (TS "", (TS $ reverse s):xs, True)
  (CloseSpliceT, (TS s, xs, True)) -> (TS "", (THole $ mkHole $ eatWhiteSpace $ reverse s):xs, False)
  (AnyT c, (TS s, xs, b))          -> (TS (c:s), xs, b)
  (EOFT, (TS s, xs, False))        -> (TS "", (TS $ reverse s):xs, True)
  _                                -> error "bad reasoning about function"

eatWhiteSpace :: String -> String
eatWhiteSpace = filter (\c -> c /= ' ' && c /= '\t')

demoEnv = M.fromList $ zip ["name", "phone", "email"] 
                           ["Jared Roesch", "805-405-5544", "roeschinc@gmail.com"]
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (args !! 0)
  let template = (compile . parse . tokenize) contents
      result   = template demoEnv
  writeFile (args !! 0 ++ ".out") result