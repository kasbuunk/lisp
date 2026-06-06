module Main where

import Data.Map qualified as Map

data Expr = Atom Atom | Cons (Expr) (Expr)

data Atom = Boolean Bool | Nand | Symbol Symbol | Define | Apply

type Env = Map.Map Symbol (Expr)

type Symbol = Char

type EnvStack = [Env]

cons :: Expr -> Expr -> Expr
cons = Cons

nand :: Expr -> Expr -> Expr
nand (Atom (Boolean b1)) (Atom (Boolean b2)) = Atom (Boolean (not (b1 && b2)))
nand e1 e2 = Cons (Atom Nand) (Cons e1 e2)

interpreter :: EnvStack -> Expr -> EnvStack
interpreter envs expr = envs

interpret :: Env -> Expr -> (Env, Expr)
interpret env (Atom (Boolean b)) = (env, eval env (Atom (Boolean b)))
interpret env (Atom (Symbol s)) = (env, eval env (Atom (Symbol s)))
interpret env (Cons (Atom Nand) (Cons e1 e2)) = (env, eval env (Cons (Atom Nand) (Cons e1 e2)))
interpret env (Cons (Atom Define) (Cons (Atom (Symbol s)) e)) = (define env s e, e)
interpret env (Cons (Atom Apply) (Cons (e) (Atom (Symbol s)))) = (env, substitute s (deref env s) e)
interpret env (Cons e1 e2) = (env, cons (snd (interpret env e1)) (snd (interpret env e2)))

eval :: Env -> Expr -> Expr
eval env (Atom (Boolean b)) = Atom (Boolean b)
eval env (Atom (Symbol s)) = Map.findWithDefault (Atom (Symbol s)) s env
eval env (Cons (Atom Nand) (Cons e1 e2)) = nand (eval env e1) (eval env e2)

define :: Env -> Symbol -> Expr -> Env
define env s e = Map.insert s e env

deref :: Env -> Symbol -> Expr
deref env s = Map.findWithDefault (Atom (Symbol s)) s env

substitute :: Symbol -> Expr -> Expr -> Expr
substitute s value (Atom (Symbol replaceMe)) = if s == replaceMe then value else Atom (Symbol replaceMe)
substitute s value (Cons e1 e2) = Cons (substitute s value e1) (substitute s value e2)
substitute s value (Atom a) = Atom a

sampleExpr :: Expr
sampleExpr = Atom (Boolean True)

sampleNand :: Expr
sampleNand = Cons (Atom Nand) (Cons (Atom (Boolean True)) (Atom (Boolean False)))

nandFn :: Expr
nandFn = Cons (Atom Nand) (Cons (Atom (Symbol 'x')) (Atom (Symbol 'x')))

defX :: Bool -> Expr
defX b = Cons (Atom Define) (Cons (Atom (Symbol 'x')) (Atom (Boolean b)))

defNand :: Expr
defNand = Cons (Atom Define) (Cons (Atom (Symbol 'f')) (nandFn))

applyFtoX :: Expr
applyFtoX = Cons (Atom Apply) (Cons (Atom (Symbol 'f')) (Atom (Symbol 'x')))

sampleProgram :: Expr
sampleProgram = Cons (defX True) (Cons defNand applyFtoX)

render :: Expr -> String
render (Atom (Boolean b)) = if b then "True" else "False"
render (Atom (Symbol s)) = [s]
render (Atom Nand) = "nand"
render (Cons e1 e2) = "(" ++ render e1 ++ " . " ++ render e2 ++ ")"

main :: IO ()
main = do
  let env = Map.empty
  -- let result = eval env sampleNand
  let (_, result) = interpret env sampleProgram
  putStrLn (render result)
