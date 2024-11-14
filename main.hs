{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set

type family Var a where
    Var a = String

data Term = Var (Var Term) | Unit | Lam (Var Type) Term | App Term Term | Anno Term Type
    deriving (Show, Eq)

data Type = TUnit | TVar (Var Type) | ForAll (Var Type) Type | Arrow Type Type
    deriving (Show, Eq)

data MonoType = MUnit | MVar (Var Type) | MArrow MonoType MonoType
    deriving (Show, Eq)

type TypeContext = Set.Set (Var Type)
type TermContext = Map.Map (Var Term) Type

type Context = (TypeContext, TermContext)

typeCheck :: Context -> Type -> Term -> Maybe Context
typeCheck fullCtx@(tVars, ctx) ty = check
    where
        check Unit = Just fullCtx
        check term = do
            (ty, ctx) <- typeInfer fullCtx term
            return (ty, ctx)

typeInfer :: Context -> Term -> Maybe (Type, Context)
typeInfer fullCtx@(tVars, ctx) = infer
    where 
        infer Unit = Just (TUnit, fullCtx)
        infer (Var x) | Just ty <- Map.lookup x ctx = Just (ty, fullCtx)
        -- infer term


main :: IO ()
main = putStrLn "Some"

