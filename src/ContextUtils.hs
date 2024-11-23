module ContextUtils (module ContextUtils) where

import Data.Maybe
import Data.Function
import Data.List

import Lib
import Syntax

infix 2 |-

class UnderContext a where
    type ContextResult a
    type ContextResult a = Maybe Context
    (|-) :: Context -> a -> ContextResult a

instance UnderContext Type where -- type well formdness
    type ContextResult Type = Bool
    (|-) :: Context -> Type -> Bool
    context |- a = case a of
        TUnit -> True
        Basic name -> EBasic name `elem` context
        TVar tv -> varIn tv context
        ForAll a ty -> EBasic a : context |- ty
        ty1 :-> ty2 -> ((&&) `on` (|-) context) ty1 ty2

typeFor :: Var Term -> Context -> Maybe Type
typeFor x ctx = (\(EAnno y ty) -> ty) <$> find check ctx
    where
        check (EAnno y ty) = y == x
        check _ = False

takeAllBefore :: ContextEntry -> Context -> Context
takeAllBefore entry context = tail $ dropWhile (/= entry) context

constrainedType :: Var Type -> Context -> Maybe Type
constrainedType tv ctx = (\(EEq _ ty) -> ty) <$> find check ctx
    where
        check (EEq tvar _) = tvar == tv
        check _ = False

varIn :: Var Type -> Context -> Bool
varIn tv = any find
    where
        find (EVar tvar) = tvar == tv
        find (EEq tvar _) = tvar == tv
        find _ = False

isWellFormed :: Context -> Bool
isWellFormed = allWithSlices well
    where
        well context = \case
            EBasic name -> EBasic name `notElem` context
            EAnno x a -> isNothing (typeFor x context) && (context |- a)
            EVar alpha -> not $ varIn alpha context
            EEq alpha a -> not $ varIn alpha context && (context |- a)
            EMark alpha -> not $ varIn alpha context && EMark alpha `notElem` context

applyContext :: Context -> Type -> Type
applyContext context = \case
    TVar alpha | Just a <- constrainedType alpha context -> applyContext context a
    ForAll alpha a -> ForAll alpha $ applyContext context a
    a :-> b -> ((:->) `on` applyContext context) a b
    other -> other

replace :: Context -> Var Type -> Context -> Context
replace context typeVar substitute = let
    (after, _:before) = break (== EVar typeVar) context
    in after ++ substitute ++ before
