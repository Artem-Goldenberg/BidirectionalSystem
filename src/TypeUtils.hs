module TypeUtils (module TypeUtils) where

import Data.Function
import Control.Monad.State

import Syntax

infix 3 .==

(.==) :: Var Type -> Type -> ContextEntry
(.==) = EEq


infix 5 -->

(-->) :: Var Type -> Type -> Type -> Type
var --> newType = \case
    Basic var' | var' == var -> newType
    ForAll var' a | var' /= var -> ForAll var' $ var --> newType $ a
    a1 :-> a2 -> ((:->) `on` var --> newType) a1 a2
    other -> other

isFreeVar :: Var Type -> Type -> Bool
isFreeVar var = \case
    TVar var' | var' == var -> True
    ForAll var' a | var' /= var -> isFreeVar var a
    a1 :-> a2 -> ((||) `on` isFreeVar var) a1 a2
    _ -> False


type RenameContext = [(Var Type, Var Type)]

cleanupVars :: Type -> Type
cleanupVars a = foldr (\fresh a -> ForAll (vars !! fresh) a) renamed [0 .. fresh - 1]
    where 
        vars = (:[]) <$> ['a'..]
        (_, (_, fresh, renamed)) = runState rename ([], 0, a)

        rename :: State (RenameContext, Int, Type) ()
        rename = do
            (context, fresh, a) <- get
            case a of 
                Basic alpha -> return ()
                TVar alpha -> case lookup alpha context of
                    Just var -> put (context, fresh, Basic var)
                    Nothing -> put ((alpha, newVar):context, fresh + 1, Basic newVar)
                        where newVar = vars !! fresh
                ForAll alpha a -> put (context, fresh, a)
                a :-> b -> do
                    let (context1, fresh1, a') = execState rename (context, fresh, a)
                    let (context2, fresh2, b') = execState rename (context1, fresh1, b)
                    put (context2, fresh2, a' :-> b')
 