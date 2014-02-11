module Feldspar.Reify where

import Feldspar.Core

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import GHC.Exts

{- A possible interface. I'm going to wait with that and just implement
   Observable Sharing first

class Observable a where
  type Variable a
  observe :: Applicative f =>
             (Variable a -> a -> a) -> f (Variable a) ->
             (a -> f a) -> a -> f a
-}

type SeenSet = IntMap [(Expr,Var)]

patch :: Expr -> IntMap [(Expr,Var)]
  
share :: Expr -> Expr
share e = go M.empty e

go :: Expr -> IntMap [Expr] -> (Expr,IntMap [Expr])
go e@(Int i) m = (e,m)
go e@(Float f) m = (f,m)
go e@(Rational r) m = (r,m)
go e@(Binop bop e1 e2) m = apa
  where m' = M.insert m (I# (dataToTag# e)) e
        (e1',m1) = go e1 m'
        (e2',m2) = go e2 m1

replace :: Expr -> IntMap [Expr] -> (Expr,IntMap [Expr])
replace e m | uninteresting e = (e,m)
replace e m = maybe (e,M.insert m [I# (dataToTag# e)]) $ do
  es <- M.lookup m e 
  e' <- find (reallyUnsafePointerEquality# e) es
  return (e',m)

uninteresting (Int _)      = True
uninteresting (Float _)    = True
uninteresting (Rational _) = True
uninteresting _            = False

{-
   data Expr =
    Int Int
  | Float Float 
  | Rational Rational
  | Binop Binop Expr Expr
  | Unop Unop Expr
  | Parallel Expr Var Expr
  | Index Expr Expr Expr
  | Pair [Expr]
  | ForLoop Expr Expr Var Var Expr
-- Binding
  | Let Var Expr Expr
  | Var Var
    deriving Show
-}
