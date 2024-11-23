module Inference (module Inference, (|-)) where

import Data.Function
import Data.List

import Syntax
import Embedding
import ContextUtils
import TypeUtils

infix 3 :=<
infix 3 :>=
data Constraint = Var Type :=< Type | Var Type :>= Type
deriving instance Show Constraint
deriving instance Eq Constraint

infix 3 :<
data SubtypeJudgement = Type :< Type

infix 3 :<=
infix 3 :*:
data TypeChecking = Term :<= Type
data AppSynthesys = Type :*: Term
newtype Synthesys = Synthesys Term

articulate :: String -> (Context, String, String)
articulate alpha = (addons, alpha1, alpha2)
    where alpha1 = alpha ++ "1"; alpha2 = alpha ++ "2"
          addons = [alpha .== TVar alpha1 :-> TVar alpha2, EVar alpha1, EVar alpha2]

instance UnderContext Constraint where
    (|-) :: Context -> Constraint -> Maybe Context
    context |- constraint = case constraint of
        alpha :=< typevar | TVar beta <- typevar -> do
            alphaIndex <- elemIndex (EVar alpha) context
            betaIndex <- elemIndex (EVar beta) context
            if alphaIndex < betaIndex then
                return $ replace context alpha [alpha .== TVar beta]
            else return $ replace context beta [beta .== TVar alpha]

        alpha :=< ForAll beta b -> do
            newContext <- EBasic beta : context |- alpha :=< b
            return $ tail $ dropWhile (/= EBasic beta) newContext

        alpha :=< a1 :-> a2 -> do
            let (addons, alpha1, alpha2) = articulate alpha
            newContext <- replace context alpha addons |- alpha1 :>= a1
            newContext |- alpha2 :=< applyContext newContext a2

        alpha :=< a | context |- a -> Just $ replace context alpha [alpha .== a]

        alpha :>= ForAll beta b -> do
            newContext <- EVar beta : EMark beta : context |- alpha :>= (beta --> TVar beta) b
            return $ tail $ dropWhile (/= EMark beta) newContext

        alpha :>= a1 :-> a2 -> do
            let (addons, alpha1, alpha2) = articulate alpha
            newContext <- replace context alpha addons |- alpha1 :=< a1
            newContext |- alpha2 :>= applyContext newContext a2

        alpha :>= a | context |- a ->
            Just $ replace context alpha [alpha .== a]

        _ -> Nothing


instance UnderContext SubtypeJudgement where
    (|-) :: Context -> SubtypeJudgement -> Maybe Context
    context |- judgement = case judgement of
        TUnit :< TUnit -> Just context
        Basic var :< Basic var' | var == var' -> Just context
        TVar alpha :< TVar alpha' | alpha == alpha' -> Just context
        a1 :-> a2 :< b1 :-> b2 -> do
            newContext <- context |- b1 :< a1
            newContext |- ((:<) `on` applyContext newContext) a2 b2
        ForAll alpha a :< b -> do
            newContext <- EVar alpha : EMark alpha : context |- (alpha --> TVar alpha) a :< b
            return $ tail $ dropWhile (/= EMark alpha) newContext
        a :< ForAll beta b -> do
            newContext <- EBasic beta : context |- a :< b
            return $ tail $ dropWhile (/= EBasic beta) newContext

        TVar alpha :< b | not $ isFreeVar alpha b -> context |- alpha :=< b
        a :< TVar beta | not $ isFreeVar beta a -> context |- beta :>= a

        _ -> Nothing


instance UnderContext TypeChecking where
    (|-) :: Context -> TypeChecking -> Maybe Context
    context |- typeChecking = case typeChecking of
        Unit :<= TUnit -> Just context

        term :<= ForAll alpha a -> takeAllBefore (EBasic alpha) <$> do -- to avoid parens
            EBasic alpha : context |- term :<= a

        Lam x term :<= a :-> b -> takeAllBefore (EAnno x a) <$> do
            EAnno x a : context |- term :<= b

        -- last
        term :<= b -> do
            (a, newContext) <- context |- Synthesys term
            newContext |- ((:<) `on` applyContext newContext) a b

instance UnderContext Synthesys where
    type ContextResult Synthesys = Maybe (Type, Context)

    (|-) :: Context -> Synthesys -> Maybe (Type, Context)
    context |- (Synthesys term) = case term of
        Unit -> Just (TUnit, context)
        Var x -> (,context) <$> typeFor x context
        Anno term' a | context |- a -> (a,) <$> do context |- term' :<= a
        Lam x term' -> do
            let alpha = x ++ "-alpha"; beta = x ++ "-beta"
            let extended = EAnno x (TVar alpha) : EVar beta : EVar alpha : context
            newContext <- extended |- term' :<= TVar beta
            return (TVar alpha :-> TVar beta, takeAllBefore (EAnno x $ TVar alpha) newContext)
        App term1 term2 -> do
            (a, newContext) <- context |- Synthesys term1
            newContext |- applyContext newContext a :*: term2
        _ -> Nothing

instance UnderContext AppSynthesys where
    type ContextResult AppSynthesys = Maybe (Type, Context)

    (|-) :: Context -> AppSynthesys -> Maybe (Type, Context)
    context |- appSynthesys = case appSynthesys of
        ForAll alpha a :*: term -> EVar alpha : context |- (alpha --> TVar alpha) a :*: term

        a :-> b :*: term -> (b,) <$> do context |- term :<= a

        TVar alpha :*: term -> do
            let (addons, alpha1, alpha2) = articulate alpha
            newContext <- replace context alpha addons |- term :<= TVar alpha1
            return (TVar alpha2, newContext)

        _ -> Nothing

infer :: Term -> Maybe Type
infer = inferWith []

inferWith :: Context -> Term -> Maybe Type
inferWith context term = if isWellFormed context 
    then do
        (a, outContext) <- context |- Synthesys term
        return $ cleanupVars $ applyContext outContext a
    else Nothing
