module Constraints (module Constraints) where

import Data.List
import Data.Function
import Control.Monad.State
import Data.Bifunctor

import Syntax
import TypeUtils
import ContextUtils
import Debug.Trace (trace)

infix 3 :=<
infix 3 :>=
data Constraint = Var Type :=< Type | Var Type :>= Type
deriving instance Show Constraint
deriving instance Eq Constraint

infix 3 :<
data SubtypeJudgement = Type :< Type

type Derivation a = StateT (Context, Fresh) Maybe a

tvar :: Fresh -> Var Type
tvar fresh = "a" ++ show fresh
-- tvar2 :: Int -> [Char]
-- tvar2 fresh = singleton $ ['a'..] !! fresh

modifyContext :: (Context -> Context) -> Derivation ()
modifyContext f = modify $ first f

setContext :: Context -> Derivation ()
setContext context = modifyContext $ const context

instance InContext Constraint where
    (|-) :: Context -> Constraint -> Maybe Context
    context |- constraint = do
        (context, _) <- execStateT (constraintCompute constraint) (context, 0)
        return context
        

instance InContext SubtypeJudgement where
    (|-) :: Context -> SubtypeJudgement -> Maybe Context
    context |- judgement = do
        (context', _) <- execStateT (subtypeCompute judgement) (context, 0)
        return context'
    
constraintCompute :: Constraint -> Derivation ()
constraintCompute constr = do
    (context, fresh) <- get
    case constr of
        alpha :=< TVar beta -> do
            alphaIndex <- lift $ elemIndex (EVar alpha) context
            betaIndex <- lift $ elemIndex (EVar beta) context
            if alphaIndex < betaIndex then
                setContext $ replace context alpha [alpha .== TVar beta]
            else setContext $ replace context beta [beta .== TVar alpha]

        alpha :=< ForAll beta b -> do
            modifyContext (EBasic beta :)
            constraintCompute $ alpha :=< b
            modifyContext $ takeAllBefore (EBasic beta)

        alpha :=< a1 :-> a2 -> do
            let (addons, alpha1, alpha2) = articulate alpha fresh
            put (replace context alpha addons, fresh + 2)
            constraintCompute $ alpha1 :>= a1
            (context', _) <- get
            constraintCompute $ alpha2 :=< applyContext context' a2

        alpha :=< a | context |- a -> setContext $ replace context alpha [alpha .== a]

        alpha :>= ForAll beta b -> do
            let beta' = tvar fresh
            put (EVar beta' : EMark beta' : context, fresh + 1)
            constraintCompute $ alpha :>= (beta --> TVar beta') b
            modifyContext $ takeAllBefore (EMark beta')

        alpha :>= a1 :-> a2 -> do
            let (addons, alpha1, alpha2) = articulate alpha fresh
            put (replace context alpha addons, fresh + 2)
            constraintCompute $ alpha1 :=< a1
            (context', _) <- get
            constraintCompute $ alpha2 :>= applyContext context' a2

        alpha :>= a | context |- a ->
            setContext $ replace context alpha [alpha .== a]

        _ -> lift Nothing

subtypeCompute :: SubtypeJudgement -> Derivation ()
subtypeCompute judgement = do
    (context, fresh) <- get
    case judgement of
        TUnit :< TUnit -> return ()
        Basic var :< Basic var' | var == var' -> return ()
        TVar alpha :< TVar alpha' | alpha == alpha' -> return ()
        a1 :-> a2 :< b1 :-> b2 -> do
            subtypeCompute (b1 :< a1)
            (context', _) <- get
            subtypeCompute $ ((:<) `on` applyContext context') a2 b2
        ForAll alpha a :< b -> do 
            let alpha' = tvar fresh
            put (EVar alpha' : EMark alpha' : context, fresh + 1)
            subtypeCompute $ (alpha --> TVar alpha') a :< b
            modifyContext $ takeAllBefore (EMark alpha')
        a :< ForAll beta b -> do
            modifyContext (EBasic beta :)
            subtypeCompute $ a :< b
            modifyContext $ takeAllBefore (EBasic beta)
        TVar alpha :< b | not $ isFreeVar alpha b ->
            constraintCompute $ alpha :=< b
        a :< TVar beta | not $ isFreeVar beta a -> 
            constraintCompute $ beta :>= a
        _ -> lift Nothing
