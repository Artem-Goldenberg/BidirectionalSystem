module Inference (module Inference, (|-)) where

import Data.List
import Data.Function
import Control.Monad.State

import Syntax
import ContextUtils
import TypeUtils
import Constraints
import Debug.Trace

infix 3 :<=
infix 3 :*:
data TypeChecking = Term :<= Type
data AppSynthesys = Type :*: Term
newtype Synthesys = Synthesys Term


instance InContext TypeChecking where
    (|-) :: Context -> TypeChecking -> Maybe Context
    context |- typeCheck = fst <$> execStateT (checkCompute typeCheck) (context, 0)

instance InContext Synthesys where
    type ContextResult Synthesys = Maybe (Type, Context)

    (|-) :: Context -> Synthesys -> Maybe (Type, Context)
    context |- syn = do
        (a, (context', _)) <- runStateT (synCompute syn) (context, 0)
        return (a, context')

instance InContext AppSynthesys where
    type ContextResult AppSynthesys = Maybe (Type, Context)

    (|-) :: Context -> AppSynthesys -> Maybe (Type, Context)
    context |- appSynthesys = do
        (a, (context', _)) <- runStateT (appSynCompute appSynthesys) (context, 0)
        return (a, context')


checkCompute :: TypeChecking -> Derivation ()
checkCompute check = do
    (context, fresh) <- get
    case check of
        Unit :<= TUnit -> return ()
        term :<= ForAll alpha a ->  do
            modifyContext (EBasic alpha :)
            checkCompute (term :<= a)
            modifyContext $ takeAllBefore (EBasic alpha)
        Lam x term :<= a :-> b -> do
            modifyContext (EAnno x a :)
            checkCompute (term :<= b)
            modifyContext $ takeAllBefore (EAnno x a)
        term :<= b -> do
            a <- synCompute (Synthesys term)
            (context', _) <- get
            subtypeCompute $ ((:<) `on` applyContext context') a b

synCompute :: Synthesys -> Derivation Type
synCompute (Synthesys term) = do
    (context, fresh) <- get
    case term of
        Unit -> return TUnit
        Var x -> lift $ typeFor x context
        Anno term' a | context |- a -> do
            checkCompute $ term' :<= a
            return a
        Lam x term' -> do
            let (alpha, beta) = (tvar fresh, tvar $ fresh + 1)
            let extendedContext = EAnno x (TVar alpha) : EVar beta : EVar alpha : context
            put (extendedContext, fresh + 2)
            checkCompute (term' :<= TVar beta)
            modifyContext $ takeAllBefore (EAnno x $ TVar alpha)
            return $ TVar alpha :-> TVar beta
        App term1 term2 -> do
            a <- synCompute (Synthesys term1)
            (context', _) <- get
            appSynCompute $ applyContext context' a :*: term2
        _ -> lift Nothing

appSynCompute :: AppSynthesys -> Derivation Type
appSynCompute (a :*: term) = do
    (context, fresh) <- get
    case a of
        TVar alpha -> do 
            let (addons, alpha1, alpha2) = articulate alpha fresh
            put (replace context alpha addons, fresh + 2)
            checkCompute $ term :<= TVar alpha1
            return $ TVar alpha2
        a :-> b -> do
            checkCompute $ term :<= a
            return b
        ForAll alpha a -> do
            let alpha' = tvar fresh
            put (EVar alpha' : context, fresh + 1)
            appSynCompute $ (alpha --> TVar alpha') a :*: term
        _ -> lift Nothing


infer :: Term -> Maybe Type
infer = inferWith []

inferWith :: Context -> Term -> Maybe Type
inferWith context term = if isWellFormed context
    then do
        (a, outContext) <- context |- Synthesys term
        -- return $ applyContext outContext a
        return $ cleanupVars $ applyContext outContext a
    else Nothing
