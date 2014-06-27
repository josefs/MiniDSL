module Feldspar.Core where

import Data.List
import Data.Int
import Data.Word

type Var = Int

data Expr =
    Int Type Integer
  | Float Float 
  | Double Double
  | Boolean Bool
  | Rational Rational
  | Binop Binop Expr Expr
  | Unop Unop Expr
  | Parallel Expr Var Expr
-- Array value. Without this, the language is not closed under evaluation
  | Array [Expr]
  | Index Expr Expr
  | Pair [Expr]
  | If Expr Expr Expr
  | ForLoop Expr Expr Var Var Expr
-- Binding
  | Let Var Expr Expr
  | Var Var
-- P, The length of the array that is created and the program which
--    writes to it
  | RunP Expr P
    deriving Show

data P =
    Assign Expr Expr
  | ParFor Expr Var P
  | LetP Var Expr P
  | Par P P
    deriving Show

data Binop =
    Plus
  | Minus
  | Times

  | Div

  | Eq
  | NEq
  | LT
  | GT
  | LTE
  | GTE

  | And
  | Or
  | Xor
  deriving Show

data Unop =
    Abs
  | Signum
  | Recip

  | Not
    -- Should I have this?
  | Proj Int
  deriving Show

instance Num Expr where
  (+) = Binop Plus
  (-) = Binop Minus
  (*) = Binop Times
  abs = Unop Abs
  signum = Unop Signum
  fromInteger = Int Unknown . fromInteger

instance Fractional Expr where
  (/) = Binop Div
  recip = Unop Recip
  fromRational = Rational

data Type =
    IntT { signed :: Bool, bits :: Size }
  | FloatT
  | DoubleT
  | BoolT
  | ArrayT Type
  | PairT [Type]
  | Unknown -- See if I really need this one
    deriving Show

intTy = IntT True Machine

data Size = Eight | Sixteen | Thirtytwo | Sixtyfour
          | Machine
            deriving Show

-- Built-in types
class Ty a where
  reify :: a -> Type

instance Ty Int where
  reify _ = IntT True Machine

instance Ty Int8 where
  reify _ = IntT True Eight

instance Ty Int16 where
  reify _ = IntT True Sixteen

instance Ty Int32 where
  reify _ = IntT True Thirtytwo

instance Ty Int64 where
  reify _ = IntT True Sixtyfour

instance Ty Word where
  reify _ = IntT False Machine

instance Ty Word8 where
  reify _ = IntT False Eight

instance Ty Word16 where
  reify _ = IntT False Sixteen

instance Ty Word32 where
  reify _ = IntT False Thirtytwo

instance Ty Word64 where
  reify _ = IntT False Sixtyfour

instance Ty Float where
  reify _ = FloatT

instance Ty Double where
  reify _ = DoubleT

instance Ty Bool where
  reify _ = BoolT

data TupleTree a = Tuple [TupleTree a] | Atom a

splitPairs :: Type -> TupleTree Type
splitPairs (PairT ts) = Tuple (map splitPairs ts)
splitPairs t = Atom t


newVar :: Expr -> Var
newVar (Binop _ e1 e2)  = newVar e1 ⊔ newVar e2
newVar (Unop _ e)       = newVar e
newVar (Parallel e v _) = newVar e  ⊔ (v + 1)
newVar (Index e1 e2)    = newVar e1 ⊔ newVar e2
newVar (Pair es)        = foldl' (⊔) 0 (map newVar es)
newVar (If e1 e2 e3)    = newVar e1 ⊔ newVar e2 ⊔ newVar e3
newVar (Var v)          = 0
newVar (RunP e p)       = newVar e  ⊔ newVarP p
newVar _                = 0

newVarP :: P -> Var
newVarP (Assign e1 e2) = newVar e1  ⊔ newVar e2
newVarP (ParFor e v _) = newVar e   ⊔ v
newVarP (LetP v e _)   = newVar e   ⊔ v
newVarP (Par p1 p2)    = newVarP p1 ⊔ newVarP p2

v1 ⊔ v2 = max v1 v2

newtype EvalM a = EvalM { unEvalM :: [(Var,Expr)] -> Maybe a }

instance Monad EvalM where
  return a = EvalM (\_ -> Just a)
  EvalM f >>= m = EvalM (\e -> case f e of
                                 Nothing -> Nothing
                                 Just a  -> unEvalM (m a) e)

newtype EvalP a = EvalP { unEvalP :: [(Var,Expr)] -> Maybe (a,[(Int,Expr)]) }

instance Monad EvalP where
  return a = EvalP (\_ -> Just (a,[]))
  EvalP f >>= m = EvalP (\e -> case f e of
                            Nothing -> Nothing
                            Just (a,arr) -> case unEvalP (m a) e of
                              Nothing -> Nothing
                              Just (b,arr') -> Just (b,arr++arr'))

runEvalP :: EvalP a -> EvalM [(Int,Expr)]
runEvalP (EvalP f) = EvalM (\e -> case f e of
                               Nothing -> Nothing
                               Just (_,arr) -> Just arr)

liftEvalM :: EvalM a -> EvalP a
liftEvalM (EvalM f) = EvalP (\e -> case f e of
                                Nothing -> Nothing
                                Just a -> Just (a,[]))

evalM :: Expr -> EvalP Expr
evalM = liftEvalM . eval

crash :: EvalM a
crash = EvalM (\_ -> Nothing)

eval :: Expr -> EvalM Expr
eval (Int ty i)         = return (Int ty i)
eval (Float f)          = return (Float f)
eval (Rational r)       = return (Rational r)
eval (Binop bop e1 e2)  = do let f = evalBinop bop
                             a <- eval e1 
                             b <- eval e2
                             f a b
eval (Unop unop e)      = do let f = evalUnop unop
                             a <- eval e
                             f a
eval (Parallel l v body) = do (Int _ l) <- eval l
                              es <- mapM (evalPar v body) [0..l-1]
                              return (Array es)
  where evalPar v body i = bindV v (Int intTy i) $ eval body
eval (Array es) = do vs <- mapM eval es
                     return (Array vs)
eval (Index e1 e2) = do Array arr <- eval e1
                        Int _ i <- eval e2
                        return (arr!!fromInteger i)
eval (Pair es) = do vs <- mapM eval es
                    return (Pair vs)
eval (If c t e) = do b <- eval c
                     case b of
                       Boolean True  -> eval t
                       Boolean False -> eval e
eval (ForLoop l init i s body) =
  do (Int _ len) <- eval l
     ist <- eval init
     loop 0 len ist i s body
  where loop i n s _ _ _ | i >= n = return s
        loop i n s v w body =
          do s' <- bindV v (Int intTy i) $ bindV w s $ eval body
             loop i n s v w body
eval (Let v e1 e2) = do val <- eval e1
                        bindV v val $ eval e2
eval (Var v) = lookupVar v
eval (RunP e p) = error "Unimplemented"  

evalBinop :: Binop -> (Expr -> Expr -> EvalM Expr)
evalBinop Plus (Int ty i) (Int _ j) = return (Int ty (i+j))
evalBinop Plus (Float a) (Float b) = return (Float (a+b))
evalBinop _ _ _ = crash

evalUnop :: Unop -> (Expr -> EvalM Expr)
evalUnop Abs (Int ty i) = return (Int ty (abs i))
evalUnop _ _ = crash

bindV :: Var -> Expr -> EvalM a -> EvalM a
bindV v expr (EvalM f) = EvalM (\e -> f ((v,expr):e))

bindP :: Var -> Expr -> EvalP a -> EvalP a
bindP v expr (EvalP f) = EvalP (\e -> f ((v,expr):e))

lookupVar :: Var -> EvalM Expr
lookupVar v = EvalM (lookup v)

evalP (Assign ix e) = do (Int _ i) <- evalM ix
                         elm <- evalM e
                         assign (fromInteger i) elm
evalP (ParFor len v body) =
  do (Int ty l) <- evalM len
     mapM_ (\i -> bindP v (Int ty i) (evalP body)) [0..l-1]
evalP (LetP v e p) = do val <- evalM e
                        bindP v val $ evalP p
evalP (Par p1 p2) = do evalP p1
                       evalP p2

assign i e = EvalP (\_ -> Just ((),[(i,e)]))

