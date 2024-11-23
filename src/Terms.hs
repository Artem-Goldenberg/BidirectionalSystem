module Terms (module Terms) where

import Syntax

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

top :: Type
top = ForAll "a" (Basic "a") :-> ForAll "a" (Basic "a")

annomega :: Term -- annotated omega
annomega = Anno omega top
