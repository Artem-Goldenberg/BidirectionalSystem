module LambdaParser (module LambdaParser) where

import Text.Parsec
import Text.Parsec.String hiding (Parser)
import Control.Monad

import Syntax
import Data.Maybe

type Parser a = Parsec String () a

lang :: Parser Term
lang = spacing *> term <* eof

spacing :: Parser ()
spacing = void $ many $ oneOf " \n\t"

identifier :: Parser String
identifier = (:) <$> letter <*> many (alphaNum <|> oneOf ['_', '\'', '&'])

inParens :: Parser a -> Parser a
inParens p = between (char '(') (char ')') p <* spacing

inBrackets :: Parser a -> Parser a
inBrackets p = between (char '[') (char ']') p <* spacing

either' :: Parser a -> Parser b -> Parser (Either a b)
either' pa pb = (Left <$> pa) <|> (Right <$> pb)

manyMaybeUntil :: Parser a -> Parser a -> Parser [a]
manyMaybeUntil p end = stop <|> next
    where
        next = do
            a <- optionMaybe p
            case a of
                Just a -> (a:) <$> manyMaybeUntil p end
                Nothing -> return []
        -- next = (:) <$> p <*> manyUntil p end
        stop = (:[]) <$> end

many1MaybeUntil :: Parser a -> Parser a -> Parser [a]
many1MaybeUntil p end = (:[]) <$> end <|> (:) <$> p <*> manyMaybeUntil p end

term :: Parser Term
term = try annotated <|> termSeq

termSeq :: Parser Term
termSeq = foldl1 App <$> many1MaybeUntil basicTerm lambda

annotated :: Parser Term
annotated = Anno <$> basicTerm <*> anno

basicTerm :: Parser Term
basicTerm = try unit <|> var <|> inParens term

var :: Parser Term
var = Var <$> identifier <* spacing

-- ground :: Parser Term
-- ground = try unit <|> inParens term <|> lambda <|> ground'

unit :: Parser Term
unit = Unit <$ string "()" <* spacing

lambda :: Parser Term
lambda = do
    void $ char '\\'
    spacing
    id <- identifier
    spacing
    void $ char '.'
    spacing
    Lam id <$> term

ground' :: Parser Term
ground' = do
    t1 <- try (Var <$> identifier)
    spacing
    (Anno t1 <$> anno) <|> (App t1 <$> term) <|> t1 <$ spacing

anno :: Parser Type
anno = char ':' *> spacing *> type'

type' :: Parser Type
type' = forall' <|> try arrow <|> basicType

basicType :: Parser Type
basicType = try tunit <|> tvar <|> inParens type'

tunit :: Parser Type
tunit = TUnit <$ string "()" <* spacing

forall' :: Parser Type
forall' = do
    id <- inBrackets identifier
    spacing
    ForAll id <$> type'

arrow :: Parser Type
arrow = do
    a <- basicType
    spacing
    void $ string "->"
    spacing
    b <- type'
    return $ a :-> b

tvar :: Parser Type
tvar = Basic <$> identifier <* spacing
