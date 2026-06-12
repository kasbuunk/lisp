module Main where

import Data.Map qualified as Map

data Expr = Atom Atom | Cons (Expr) (Expr)

data Atom = Boolean Bool | Nand | Symbol String | Apply

eval :: Expr -> Expr
eval (Atom (Boolean b)) = Atom (Boolean b)
eval (Cons (Atom Nand) (Cons b1 b2)) = nand b1 b2
eval (Cons (Atom Apply) (Cons (Cons params body) arg)) = eval (apply params body arg)

-- apply takes a list of params, a function body and a list of arguments to apply the function to.
-- Both the params and the args are a list, to implement currying and ergonomic multivariate function
-- application with just one operation.
--
-- nil arguments means the function is not fully applied, it still has unbound params.
apply :: Expr -> Expr -> Expr -> Expr
apply params body (Atom (Symbol "nil")) = Cons params body
apply (Cons head tail) body (Cons arg args) = apply tail (substitute head body arg) args

-- substitute takes a parameter and an argument and a value, and if they match, it replaces the body's parameter with the given value.
substitute :: Expr -> Expr -> Expr -> Expr
substitute (Atom (Symbol s1)) (Atom (Symbol s2)) arg = if s1 == s2 then arg else Atom (Symbol s2) -- substitute for s2's value (arg) if symbols match
substitute param (Cons e1 e2) arg = Cons (substitute param e1 arg) (substitute param e2 arg)

nil :: Expr
nil = Atom (Symbol "nil")

nand :: Expr -> Expr -> Expr
nand (Atom (Boolean b1)) (Atom (Boolean b2)) = Atom (Boolean (not (b1 && b2)))
nand e1 e2 = Cons (Atom Nand) (Cons e1 e2)

render :: Expr -> String
render (Atom (Boolean b)) = if b then "True" else "False"
render (Atom (Symbol s)) = s
render (Atom Nand) = "nand"
render (Cons e1 e2) = "(" ++ render e1 ++ " . " ++ render e2 ++ ")"

sampleProgram :: Expr
sampleProgram = Cons (Cons (Atom Apply) nandFn) (Atom (Boolean True))

nandFn :: Expr
nandFn = (Cons (Atom Nand) (Cons b1 b2))

-- sampleNand :: Expr
-- sampleNand = Cons (Atom Nand) (Cons (Atom (Boolean True)) (Atom (Boolean False)))

-- nandFn :: Expr
-- nandFn = Cons (Atom Nand) (Cons (Atom (Symbol "x")) (Atom (Symbol "x")))

-- defX :: Bool -> Expr
-- defX b = Cons (Atom Define) (Cons (Atom (Symbol "x")) (Atom (Boolean b)))

-- defNand :: Expr
-- defNand = Cons (Atom Define) (Cons (Atom (Symbol "f")) (nandFn))

-- applyFtoX :: Expr
-- applyFtoX = Cons (Atom Apply) (Cons (Atom (Symbol "f")) (Atom (Symbol "x")))

-- sampleProgram :: [Expr]
-- sampleProgram = [defX True, defNand, applyFtoX]

-- sampleProgram :: Expr
-- sampleProgram = Cons (Atom Apply)

main :: IO ()
main = do
  let result = eval sampleNand
  putStrLn (render result)
