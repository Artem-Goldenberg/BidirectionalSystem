module Terms (module Terms) where

import Syntax
import Inference
import Data.Maybe
import ContextUtils
import Control.Monad.State
import Debug.Trace

a :: Term
a = Var "a"
b :: Term
b = Var "b"
x :: Term
x = Var "x"
y :: Term
y = Var "y"
z :: Term
z = Var "z"

id1 :: Term
id1 = Lam "x" $ Var "x"
id2 :: Term
id2 = Lam "y" $ Var "y"
id3 :: Term
id3 = Lam "z" $ Var "z"
idType :: Type
idType = ForAll "a" $ Basic "a" :-> Basic "a"

-- K = ^xy.x
k :: Term
k = Lam "x" $ Lam "y" $ Var "x"
kType :: Type
kType = ForAll "a" $ ForAll "b" $ Basic "a"

-- S = ^xyz.x z (y z)
s :: Term
s = Lam "x" $ Lam "y" $ Lam "z" $ x `App` z `App` (y `App` z)
    where x = Var "x"; y = Var "y"; z = Var "z"
sType :: Type -- (a -> b -> c) -> (a -> b) -> a -> c
sType = ForAll "a" $ ForAll "b" $ ForAll "c" $ (a :-> b :-> c) :-> (a :-> b) :-> a :-> c
    where a = Basic "a"; b = Basic "b"; c = Basic "c"

omega :: Term
omega = Lam "f" $ App (Var "f") (Var "f")

-- top :: Type
-- top = ForAll "a" (Basic "a") :-> ForAll "a" (Basic "a")

top :: Type
top = ForAll "a" (Basic "a" :-> Basic "a")
bot :: Type
bot = ForAll "a" $ Basic "a"

annomega :: Term -- annotated omega
annomega = Anno omega top -- (ForAll "a" $ top :-> Basic "a")


untyped :: Term
untyped = Lam "a" $ Lam "b" $ Anno (Lam "x" $ App (App x a) (App x b)) $ ForAll "a" (Basic "a" :-> Basic "a") :-> Basic "c"

untyped2 :: Term
untyped2 = Lam "b" $ Anno (Lam "x" $ App (App x a) (App x b)) $ ForAll "v" (Basic "v" :-> Basic "v") :-> Basic "c"

context2 :: Context
context2 = [
    -- EAnno "a" $ Basic "a",
    EBasic "a",
    EBasic "c"
    ]

term1 :: Term
term1 =  Lam "x" $ Lam "a" $ Lam "b" $ App (App x a) (App x b)

term2 :: Term
term2 = Lam "a" $ Lam "b" $ App (App x a) (App x b)

term3 :: Term
term3 = App (App x a) (App x b)

context :: Context
context = [
    EAnno "x" $ ForAll "a" $ Basic "a" :-> Basic "a",
    EAnno "a" $ Basic "b" :-> Basic "c",
    EAnno "b" $ Basic "b",
    EBasic "b",
    EBasic "c"
    ]


p :: (Type, Context)
p = fromJust $ context |- Synthesys (App x a)
context' :: Context
context' = snd p
a' :: Type
a' = fst p
a'' :: Type
a'' = applyContext context' a'

result :: Maybe (Type, (Context, Fresh))
result = runStateT (appSynCompute $ a'' :*: App x a) (context', 5)

term :: Term
term = Lam "x" $ Lam "y" $ App x y
