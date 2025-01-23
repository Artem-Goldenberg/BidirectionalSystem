module Inference2 (module Inference2) where

import Data.Maybe
import Control.Monad.State
import Data.Function

import Syntax
import Constraints
import ContextUtils
import TypeUtils
import Inference

tvar2 :: Fresh -> Var Type
tvar2 fresh = "c" ++ show fresh

checkCompute2 :: TypeChecking -> Derivation ()
checkCompute2 check = do
    (context, fresh) <- get
    case check of
        Unit :<= TUnit -> return ()
        term :<= ForAll alpha a ->  do
            modifyContext (EBasic alpha :)
            checkCompute2 (term :<= a)
            modifyContext $ takeAllBefore (EBasic alpha)
        Lam x term :<= a :-> b -> do
            modifyContext (EAnno x a :)
            checkCompute2 (term :<= b)
            modifyContext $ takeAllBefore (EAnno x a)
        term :<= b -> do
            a <- synCompute (Synthesys term)
            (context', _) <- get
            subtypeCompute $ ((:<) `on` applyContext context') a b


synCompute2 :: Synthesys -> Derivation Type
synCompute2 (Synthesys term) = do
    (context, fresh) <- get
    case term of
        Unit -> return TUnit
        Var x -> lift $ typeFor x context
        Anno term' a | context |- a -> do
            checkCompute2 $ term' :<= a
            return a
        Lam x term' -> do
            let (alpha, beta) = (tvar fresh, tvar $ fresh + 1)
            let extendedContext = EAnno x (TVar alpha) : EVar beta : EVar alpha : EMark alpha : context
            put (extendedContext, fresh + 2)
            checkCompute2 (term' :<= TVar beta)
            (context', fresh') <- get
            let tau = TVar alpha :-> TVar beta
            let tau' = applyContext (takeWhile (/= EMark alpha) context') tau
            let (tau'', fresh'') = lambdaType context' fresh' tau'
            put (takeAllBefore (EAnno x $ TVar alpha) context', fresh'')
            return tau''
        App term1 term2 -> do
            a <- synCompute2 (Synthesys term1)
            (context', _) <- get
            appSynCompute2 $ applyContext context' a :*: term2
        _ -> lift Nothing

lambdaType :: Context -> Fresh -> Type -> (Type, Fresh)
lambdaType context fresh tau = (foralled, fresh')
    where
        foralled = foldr ForAll substituted freshVars
        substituted = foldr (\(alpha, var) r -> (alpha `sub2` Basic var) r) tau $ zip alphas freshVars
        fresh' = fresh + length alphas
        freshVars = [tvar2 (fresh + i) | i <- [0..(length alphas - 1)]]
        alphas = unsolved context

unsolved :: Context -> [Var Type]
unsolved = mapMaybe toVar
    where
        toVar (EVar v) = Just v
        toVar _ = Nothing

sub2 :: Var Type -> Type -> Type -> Type
sub2 var newType = \case
    TVar var' | var' == var -> newType
    -- Basic var' | var' == var -> newType
    ForAll var' a | var' /= var -> ForAll var' $ var `sub2` newType $ a
    a1 :-> a2 -> ((:->) `on` var `sub2` newType) a1 a2
    other -> other


appSynCompute2 :: AppSynthesys -> Derivation Type
appSynCompute2 (a :*: term) = do
    (context, fresh) <- get
    case a of
        TVar alpha -> do
            let (addons, alpha1, alpha2) = articulate alpha fresh
            put (replace context alpha addons, fresh + 2)
            checkCompute2 $ term :<= TVar alpha1
            return $ TVar alpha2
        a :-> b -> do
            checkCompute2 $ term :<= a
            return b
        ForAll alpha a -> do
            let alpha' = tvar fresh
            put (EVar alpha' : context, fresh + 1)
            appSynCompute2 $ (alpha --> TVar alpha') a :*: term
        _ -> lift Nothing

infer2 :: Term -> Maybe Type
infer2 = inferWith2 []

inferWith2 :: Context -> Term -> Maybe Type
inferWith2 context term = if isWellFormed context
    then do
        (a, (outContext, _)) <- runStateT (synCompute2 $ Synthesys term) (context, 0)
        -- return $ applyContext outContext a
        return $ cleanupVars $ applyContext outContext a
    else Nothing
