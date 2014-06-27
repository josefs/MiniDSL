{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
module Feldspar.Frontend where

import qualified Prelude as P
import Prelude (Show(..),Num(..),Eq(..),Functor(..),
                Maybe(..),(.),($),error,
                (++),map)
import qualified Data.Int as I
import qualified Data.Word as W
import Feldspar.Core

-- Class for embeddings on top
class Syntax a where
  desug :: a -> Expr
  sug   :: Expr -> a

bind :: (Expr -> Expr) -> (Var, Expr)
bind f = (v,body)
  where v    = newVar body
        body = f (Var v)

let_ :: (Syntax a, Syntax b) => a -> (a -> b) -> b
let_ a f = sug (Let v (desug a) body)
  where (v,body) = bind (desug . f . sug)

parallel_ :: forall a. Syntax a => Length -> (Index -> a) -> Manifest a
parallel_ l ixf = Manifest (Parallel len v body) len
  where (v,body) = bind (desug . ixf . sug)
        len      = desug l

for_ :: Syntax s => Length -> s -> (Index -> s -> s) -> s
for_ l init f = sug (ForLoop (desug l) (desug init) v1 v2 body)
  where v2 = newVar body
        v1 = v2 P.+ 1
        body = desug (f (sug (Var v1)) (sug (Var v2)))

and,or :: Bool -> Bool -> Bool
and F _ = F
and _ F = F
and b c = BAnd b c
or T _ = T
or _ T = T
or b c = BOr b c

cond :: Syntax a => Bool -> a -> a -> a
cond T t _ = t
cond F _ e = e
cond b t e = sug $ If (boolToExpr b) (desug t) (desug e)

(!) :: Syntax a => Manifest a -> Index -> a
Manifest arr _ ! i = sug (Index arr (desug i))

-- P

newtype Prog a = Prog { unP :: P }

runP :: Prog a -> Index -> Manifest a
runP (Prog p) l = Manifest (RunP (desug l) p) (desug l)

(=:) :: Syntax a => Index -> a -> Prog a
i =: e = Prog (Assign (desug i) (desug e))

parFor :: Index -> (Index -> Prog a) -> Prog a
parFor l f = Prog (ParFor (integralToExpr l) v body)
  where body = unP (f (IMem intTy (Var v)))
        v    = newVarP body

let_p :: Syntax a => a -> (a -> Prog b) -> Prog b
let_p a f = Prog (LetP v (desug a) body)
  where body = unP (f (sug (Var v)))
        v    = newVarP body

-- Deep on Deep

type Int   = Integral P.Int
type Int8  = Integral I.Int8
type Int16 = Integral I.Int16
type Int32 = Integral I.Int32
type Int64 = Integral I.Int64

type Index  = Int
type Length = Int

-- http://www.cs.cmu.edu/afs/cs/academic/class/15745-s06/web/handouts/03.pdf
data Integral a = IConst a
                | Add [Integral a] (Maybe a)
                | Mult (Maybe a) [Integral a]
                | IMem Type Expr

integralToExpr :: (Ty a,P.Integral a) => Integral a -> Expr
integralToExpr (IConst a)  = Int (reify a) (P.toInteger a)
integralToExpr (Add ts mc) = P.maybe add1 add mc
  where es = P.map (integralToExpr) ts
        add1  = P.foldr1 (Binop Plus) es
        add c = P.foldr  (Binop Plus) (Int (reify c) (P.toInteger c)) es
integralToExpr (Mult mc ts) = P.maybe mul1 mul mc
  where es = P.map (integralToExpr) ts
        mul1  = P.foldr1 (Binop Times) es
        mul c = P.foldr  (Binop Times) (Int (reify c) (P.toInteger c)) es
integralToExpr (IMem ty d) = d

instance (Num a,Eq a,Ty a, P.Integral a) => Num (Integral a) where
  IConst i + IConst j = IConst (i+j)
  c@(IConst _) + t = t + c
  t + IConst 0 = t
  (Add ts mc) + (IConst c) = Add ts (mc <+ c)
  (Add ts1 mc1) + (Add ts2 mc2) = Add (ts1++ts2) (mc1 <+> mc2)
  (Add ts mc) + t = Add (t:ts) mc
  t + (Add ts mc) = Add (t:ts) mc
  t1 + t2 = Add [t1,t2] Nothing

  IConst i * IConst j = IConst (i*j)
  t * c@(IConst _) = c * t
  IConst c * Mult mc ts = Mult (mc <* c) ts
  c@(IConst i) * Add ts mc = Add (map (*c) ts) (mc <+ i)
  Mult mc1 ts1 * Mult mc2 ts2 = Mult (mc1 <*> mc2) (ts1 ++ ts2)
  Mult mc ts * t = Mult mc (t:ts)
  t * Mult mc ts = Mult mc (t:ts)
  t1 * t2 = Mult Nothing [t1,t2]

  negate (IConst a) = IConst (negate a)
  negate t          = IMem (reify (P.undefined :: a)) (negate (integralToExpr t))

  signum (IConst a) = IConst (signum a)
  signum t          = IMem (reify (P.undefined :: a))(signum (integralToExpr t))

  abs (IConst a) = IConst (abs a)
  abs t          = IMem (reify (P.undefined :: a)) (abs (integralToExpr t))

  fromInteger = IConst . fromInteger

Nothing <+ c  = Just c
Just c1 <+ c2 = Just (c1 + c2)
Just c1 <+> Just c2 = Just (c1+c2)
Just c  <+> _       = Just c
_       <+> Just c  = Just c
_       <+> _       = Nothing
Nothing <* c  = Just c
Just c1 <* c2 = Just (c1 * c2)
Just c1 <*> Just c2 = Just (c1 * c2)
Just c  <*> _       = Just c
_       <*> Just c  = Just c
_       <*> _       = Nothing

instance (Ty a, P.Integral a) => Syntax (Integral a) where
  desug = integralToExpr
  sug e = IMem (reify (P.undefined :: a)) e

type Double = Floating P.Double
type Float = Floating P.Float

data Floating a = FConst a
                | FMem Expr

-- Booleans

data Bool =
  T | F | BAnd Bool Bool | BOr Bool Bool | BXor Bool Bool | BNot Bool
  | DBool Expr

instance Ty P.Bool where
  reify _ = BoolT

instance Syntax Bool where
  desug = boolToExpr
  sug   = DBool

boolToExpr :: Bool -> Expr
boolToExpr T = Boolean P.True
boolToExpr F = Boolean P.False
boolToExpr (BAnd b1 b2) = Binop And (boolToExpr b1) (boolToExpr b2)
boolToExpr (BOr  b1 b2) = Binop Or  (boolToExpr b1) (boolToExpr b2)
boolToExpr (BXor b1 b2) = Binop Xor (boolToExpr b1) (boolToExpr b2)
boolToExpr (BNot b) = Unop Not (boolToExpr b)
boolToExpr (DBool d) = d

-- Arrays

data Manifest a = Manifest Expr Expr

data Pull a = Pull (Index -> a) Length

data Push a = Push (forall b. Syntax b => (Index -> a -> Prog b) -> Prog b) Length

instance Functor Pull where
  fmap f (Pull ixf l) = Pull (f . ixf) l

instance Functor Push where
  fmap f (Push g l) = Push (\k -> g (\i a -> k i (f a)))  l

class Pully vec where
  toPull :: Syntax a => vec a -> Pull a

instance Pully Pull where
  toPull p = p

instance Pully Manifest where
  toPull m@(Manifest _ l) = Pull (\i -> m ! i) (sug l)

class Pushy vec where
  toPush :: Syntax a => vec a -> Push a

instance Pushy Pull where
  toPush (Pull ixf l) = Push (\k -> parFor l (\i -> k i (ixf i))) l

instance Pushy Push where
  toPush push = push

instance Pushy Manifest where
  toPush manifest = toPush (toPull manifest)

class Storable vec where
  store :: Syntax a => vec a -> Manifest a

instance Storable Manifest where
  store manifest = manifest

instance Storable Pull where
  store pull = store (toPush pull)

instance Storable Push where
  store (Push loop l) = runP (loop (\i a -> i =: a)) l
