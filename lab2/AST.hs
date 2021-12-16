{- not allowed to change this file. -}

module AST where

import qualified Data.List as L

newtype Program = Program [Rule] deriving Show

{- double nested [[Rel]] not strictly needed in our prolog, but later on can be used to support a conjunction of disjunction of Rel.-}
{- so a conjuction would look like [[Rel], [Rel], [Rel]] -}
{- Rule is a clause. Rel is heaad, [[Rel]] is predicates (aka body). -}
data Rule = Rule Rel [[Rel]] deriving Show

{- this supports either a relation or a cut operator -}
{- each predicate is name of functor + list of args. or it might be a cut operator. -}
data Rel = Rel String [Term] 
         | Cut deriving Show

{- this supports either an atom, variable or functor term -} 
data Term =
  Atom String
  | Var String
  | Func String [Term] deriving (Eq, Ord)

instance Show Term where
  show (Atom a) = a
  show (Var x) = x
  show (Func name terms) = name ++ "(" ++ L.intercalate ", " (map show terms) ++ ")"
