module Main where

import Data.Map qualified as Map

data Expr = Atom Atom | Cons (Expr) (Expr)

data Atom = Boolean Bool | And | Symbol Symbol

type Env = Map.Map Symbol (Expr)

type Symbol = Char

type EnvStack = [Env]

cons :: Expr -> Expr -> Expr
cons = Cons

and' :: Expr -> Expr -> Expr
and' (Atom (Boolean b1)) (Atom (Boolean b2)) = Atom (Boolean (b1 && b2))

interpreter :: EnvStack -> Expr -> EnvStack
interpreter envs expr = envs

eval :: Env -> Expr -> Expr
eval _ (Atom (Boolean b)) = Atom (Boolean b)
eval env (Atom (Symbol s)) = Map.findWithDefault (Atom (Symbol s)) s env
eval env (Cons (Atom And) (Cons e1 e2)) = and' (eval env e1) (eval env e2)
eval env (Cons e1 e2) = cons (eval env e1) (eval env e2)

sampleExpr :: Expr
sampleExpr = Atom (Boolean True)

sampleAnd :: Expr
sampleAnd = Cons (Atom And) (Cons (Atom (Boolean True)) (Atom (Boolean False)))

render :: Expr -> String
render (Atom (Boolean b)) = if b then "True" else "False"
render (Atom (Symbol s)) = [s]
render (Atom And) = "and"
render (Cons e1 e2) = "(" ++ render e1 ++ " . " ++ render e2 ++ ")"

main :: IO ()
main = do
  let env = Map.empty
  let result = eval env sampleAnd
  putStrLn (render result)
