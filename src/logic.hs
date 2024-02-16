module Logic where

import Sudoku

type VarT = Int -- This is strange and bad but might work for the purpose of freshValue :)) 
type Term = VarT 
data Domain = BLOCK | ROW | COLLUMN | VALUE
    deriving (Eq,Show)
data SudSem = SudVal
    deriving (Eq,Show)
type Enviorment = ([(VarT, Domain)], Sudoku)

freshValue :: Domain -> Enviorment -> Enviorment
freshValue dom (env, sud) = (env ++ (name, dom), sud)
    where name = last env + 1


evalT :: Term -> Enviorment -> SudSem
evalT t env = case lookup t env of 
                Nothing -> error "Not in enviorment"
                Just d -> d

type PSym = String
data FOL  =  Implies FOL FOL | And FOL FOL | Or FOL FOL | Not FOL
          |  FORALL VarT Domain FOL    |  EXISTS VarT Domain FOL
          |  PName PSym [Term]    |  Equal  Term  Term
    deriving Show
{-
--eval0 :: PSym -> [SudSem] -> Bool
--eval0 "Equal"     [t1,t2]  =  t1 == t2
--eval0 "LessThan"  [t1,t2]  =  t1 < t2
--eval0 "Positive"  [t1]     =  t1 > 0
-}
eval :: FOL -> Enviorment -> Bool
eval formula env = ev formula
  where  ev (PName n args)  = error "TODO"--eval0 n (map (flip evalT env) args)
         ev (Equal a b)     = evalT a env == evalT b env
         ev (And p q)       = ev p  &&  ev q
         ev (Or  p q)       = ev p  ||  ev q
         ev (FORALL v BLOCK p)      = undefined
         ev (FORALL v ROW p)        = undefined
         ev (FORALL v COLLUMN p)    = undefined
         ev (FORALL v VALUE p)      = undefined
         ev (EXISTS v BLOCK p)      = undefined
         ev (EXISTS v ROW p)        = undefined
         ev (EXISTS v COLLUMN p)    = undefined
         ev (EXISTS v VALUE p)      = undefined

forall :: FOL -> Enviorment -> Bool
forall (FORALL v BLOCK p) (vs, sud) = and (eval p) [FORALL freshValue VALUE | b <- blocks sud]