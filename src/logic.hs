module Logic where

import Sudoku

data FOL = And FOL FOL | Or FOL FOL | Not FOL | Implies FOL FOL | FORALL String FOL | EXISTS String FOL | True | False
    deriving (Eq, Show)

check :: FOL -> Bool
check = error "Not implemented"

