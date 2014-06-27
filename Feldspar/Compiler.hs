{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
module Feldspar.Compiler where

import Prelude hiding (LT,GT)
import qualified Prelude as P

import Feldspar.Core
import Feldspar.Frontend

import Data.Loc (noLoc)
import Data.Symbol
import "language-c-quote" Language.C.Syntax hiding (Assign,Div,Eq,And,Or,Xor,Not,If,Type)
import Language.C.Quote.C

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Lens

compileDest :: TupleTree (Maybe Id) -> Expr -> CompileM [Stm]
compileDest (Atom Nothing) _ = return []
compileDest (Atom (Just d)) (Int _ i) = just [cstm| $id:d = $(int i) ; |]
compileDest (Tuple ts) (Pair es) = fmap concat $ zipWithM compileDest ts es
compileDest (Atom (Just d)) (Float f) = just [cstm| $id:d = $(float f) ; |]
compileDest (Atom (Just d)) (Binop bop e1 e2) = do
  (s1,c1) <- compile e1
  (s2,c2) <- compile e2
  return $
    s1 ++
    s2 ++
    [ [cstm| $id:d = $(binop bop c1 c2) ; |] ]
compileDest tt (Unop (Proj n) e) = compileDest (Tuple (repeat (Atom Nothing) & element n .~ tt)) e
compileDest (Atom (Just d)) (Unop op e) = do
  (s,c) <- compile e
  return $
   s ++ [ [cstm| $id:d = $(unop op c) ; |] ]
compileDest ds (If b t e) = do
  (cn,c) <- compile b
  th     <- compileDest ds t
  el     <- compileDest ds e
  return $
    cn ++ [ [cstm| if($c) {
                     $stms:th ;
                   } else {
                     $stms:el ;
                   } |] ]
compileDest ds (Let v e1 e2) = do
  v' <- freshId
  stms  <- compileDest (Atom (Just v')) e1
  subst v v'
  stms' <- compileDest ds e2
  return (stms ++ stms')
compileDest ds e = error "Internal error: compile dest"

int :: Integer -> Exp
int i = Const (IntConst "" Signed (fromIntegral i) noLoc) noLoc

float :: P.Float -> Exp
float f = Const (FloatConst "" (toRational f) noLoc) noLoc

binop Plus  e1 e2 = [cexp| $e1 + $e2 |]
binop Minus e1 e2 = [cexp| $e1 - $e2 |]
binop Times e1 e2 = [cexp| $e1 * $e2 |]
binop Div   e1 e2 = [cexp| $e1 / $e2 |]
binop Eq    e1 e2 = [cexp| $e1 == $e2 |]
binop NEq   e1 e2 = [cexp| $e1 != $e2 |]
binop LT    e1 e2 = [cexp| $e1 < $e2 |]
binop GT    e1 e2 = [cexp| $e1 > $e2 |]
binop LTE   e1 e2 = [cexp| $e1 <= $e2 |]
binop GTE   e1 e2 = [cexp| $e1 >= $e2 |]
binop And   e1 e2 = [cexp| $e1 && $e2 |]
binop Or    e1 e2 = [cexp| $e1 || $e2 |]
binop Xor   e1 e2 = [cexp| $e1 ^ $e2 |]

unop Abs    e = [cexp| abs($e) |] -- make it work for other types
unop Signum e = [cexp| ($e < 0) ? -1 : ($e > 0) |] -- we probably don't want to
                                                   -- duplicate e here.
unop Recip  e = [cexp| 1 / $e |]
unop Not    e = [cexp| !$e |] 


compile :: Expr -> CompileM ([Stm],Exp)
compile (Int _ i) = merely [cexp| $(int i) |]
compile (Float f) = merely [cexp| $(float f) |]
compile _ = error "compile: Undefined"

merely e = return ([],e)

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
-}

just :: Stm -> CompileM [Stm]
just = return . return

compileP :: P -> Id -> CompileM [Stm]
compileP (Assign i el) arr = do
  ix <- compileExp i
  e  <- compileExp el
  just [cstm| $id:arr[$ix] = $e ; |]
compileP (ParFor e v body) arr = do
  b <- compileP body arr
  l <- freshId
  i <- freshId
  s <- compileDest (Atom (Just l)) e
  return $ s ++
         [ [cstm| #pragma omp parallel for nowait
                  for(int $id:i = 0; $id:i < $id:l ; $id:i ++) {
                    $stms:b
                  } |]
         ]
compileP (Par p1 p2) arr = do
  s1 <- compileP p1 arr
  s2 <- compileP p2 arr
  let sec1 = [cstm| #pragma omp section 
                    { $stms:s1 } |]
      sec2 = [cstm| #pragma omp section
                    { $stms:s2 } |]
  just [cstm| #pragma omp parallel sections nowait
              {
                $stm:sec1
                $stm:sec2
              } |]
compileP (LetP v e p) arr = do
  ex <- compileExp e
  s  <- compileP p arr
  return ([cstm| $id:(show v) = $ex ; |] : s)

compileExp :: Expr -> CompileM Exp
compileExp = undefined

newtype CompileM a = CompileM a

instance Monad CompileM where
  return = CompileM
  CompileM a >>= f = f a

instance Functor CompileM where
  fmap f (CompileM a) = CompileM (f a)


freshId :: CompileM Id
freshId = error "freshId: Unimplemented"
