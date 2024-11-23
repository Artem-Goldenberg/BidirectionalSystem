module Syntax (module Syntax) where

type family Var level where
    Var level = String

type BasicType = String

data Term =
      Var (Var Term)
    | Unit
    | Lam (Var Term) Term
    | App Term Term
    | Anno Term Type
    deriving Eq

instance Show Term where
    show :: Term -> String
    show = \case
        Unit -> "()"
        Var x -> x
        Lam x term -> "\\" ++ x ++ "." ++ show term
        App term term' -> "(" ++ show term ++ " " ++ show term' ++ ")"
        Anno term a -> "(" ++ show term ++ ": " ++ show a ++ ")"
  
infixr 4 :->

data Type =
      TUnit
    | Basic (Var Type)
    | TVar (Var Type)
    | ForAll (Var Type) Type
    | Type :-> Type
    deriving Eq

instance Show Type where
    show :: Type -> String
    show = \case
        TUnit -> "()"
        Basic alpha -> alpha
        TVar alpha -> "^" ++ alpha
        ForAll alpha a -> "∀" ++ alpha ++ "." ++ show a
        a :-> b -> "(" ++ show a ++ " -> " ++ show b ++ ")"

data MonoType =
      MUnit
    | MBasic (Var Type)
    | MVar (Var Type)
    | MArrow MonoType MonoType
    deriving (Show, Eq)

data ContextEntry =
      EBasic (Var Type)
    | EAnno (Var Term) Type
    | EVar (Var Type)
    | EEq (Var Type) Type
    | EMark (Var Type)
    deriving Eq
  
instance Show ContextEntry where
    show :: ContextEntry -> String
    show = \case
        EBasic var -> var
        EAnno x a -> x ++ ": " ++ show a
        EVar alpha -> "^" ++ alpha
        EEq alpha a -> "^" ++ alpha ++ " = " ++ show a
        EMark alpha -> ">" ++ alpha
  
type Context = [ContextEntry]
