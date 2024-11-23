module Embedding ((.==)) where

import Syntax

infix 3 .==

(.==) :: Var Type -> Type -> ContextEntry
(.==) = EEq
