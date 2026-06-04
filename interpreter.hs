module Main where

import Data.Map qualified as Map

data Expr = Atom Atom | Cons (Expr) (Expr)

data Atom = Boolean Bool | And

data Env = Map Symbol (Expr)

data Symbol = Char

data EnvStack = List Env

cons :: Expr -> Expr -> Expr
cons = Cons

interpreter :: EnvStack -> Expr -> EnvStack
interpreter envs expr = envs

eval :: Expr -> Bool
eval (Atom (Boolean a)) = a

-- eval (Cons e1 e2) = cons (eval e1) (eval e2)

sampleExpr :: Expr
sampleExpr = Atom (Boolean True)

sampleAnd :: Expr
sampleAnd = Cons (Atom And) (Cons (Atom (Boolean True)) (Atom (Boolean False)))

main :: IO ()
main = do
  putStrLn "this prints"
