{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Function

type family Var a where
    Var Context = Either (Var Term) (Var Type)
    Var a = String

data Term = Var (Var Term) | Unit | Lam (Var Type) Term | App Term Term | Anno Term Type
    deriving (Show, Eq)

data Type = TUnit | TVar (Var Type) | ForAll (Var Type) Type | Arrow Type Type
    deriving (Show, Eq)

data MonoType = MUnit | MVar (Var Type) | MArrow MonoType MonoType
    deriving (Show, Eq)

type Context = [Entry]
data Entry = CAnno (Var Term) Type | CVar (Var Type)
    deriving (Show, Eq)

type Substitution a = [(Var a, SubResult a)]

class Subst a where
    type SubResult a :: *
    substOne :: Var a -> SubResult a -> a -> a
    substOne v t self = subst self [(v, t)]
    subst :: a -> Substitution a -> a
    subst self = \case
        [] -> self
        (v, t):subs -> substOne v t self

instance Subst Term where
    type SubResult Term = Term 
    substOne :: Var Term -> Term -> Term -> Term
    substOne var sub = \case
        Var a | a == var -> sub
        Lam a t | a == var -> Lam a t -- bad case, do not substitute
        Lam a t | a /= var -> Lam a (subst t)
        App t1 t2 -> (App `on` subst) t1 t2
        Anno t ty -> Anno (subst t) ty
        other -> other
        where subst = substOne var sub 

instance Subst Type where
    type SubResult Type = Type
    substOne :: Var Type -> Type -> Type -> Type
    substOne var sub = \case
        TVar a | a == var -> sub
        ForAll a ty | a == var -> ForAll a ty
        ForAll a ty | a /= var -> ForAll a (subst ty)
        Arrow ty1 ty2 -> (Arrow `on` subst) ty1 ty2
        other -> other
        where subst = substOne var sub

instance Subst MonoType where
    type SubResult MonoType = MonoType
    substOne :: Var MonoType -> MonoType -> MonoType -> MonoType
    substOne var sub = \case
        MVar a | a == var -> sub
        MArrow ty1 ty2 -> (MArrow `on` subst) ty1 ty2
        other -> other
        where subst = substOne var sub

instance Subst Context where
    type SubResult Context = Either Term Type
    substOne :: Var Context -> Either Term Type -> Context -> Context
    substOne var sub ctx = let 
            f = \case
                CAnno x ty | Left x == var -> CAnno  ty
                CAnno x ty | Right ty == var -> CAnno (sub) (ty)
                CVar tv | Right tv == var -> sub
        in map f ctx


main :: IO ()
main = putStrLn "Some"

