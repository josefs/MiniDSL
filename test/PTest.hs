{-# LANGUAGE QuasiQuotes #-}
module PTest where

import Data.Loc (noLoc)
import Language.C.Syntax
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

main = putDoc $ ppr (Block [BlockStm [cstm| a += 1; |]
                           ,BlockDecl (InitGroup (DeclSpec [] [] (Tint Nothing noLoc) noLoc) [] [Init (Id "a" noLoc) (DeclRoot noLoc) Nothing Nothing [] noLoc] noLoc)
                           ] noLoc)
