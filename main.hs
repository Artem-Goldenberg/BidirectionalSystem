module Main (main) where

import Data.Maybe
import Control.Monad.State
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Syntax
import Terms
import Inference
import TypeUtils
import ContextUtils
import LambdaParser (lang)

-- Just an example of inferring a type
-- main :: IO ()
-- main = do
--     let term = App (App k id3) annomega  -- K id om = id
--     putStrLn $ "Inferring type for: " ++ show term
--     let (a, context) = fromJust $ [] |- Synthesys term
--     putStrLn ""
--     putStrLn $ "Final context: " ++ show context
--     putStrLn ""
--     let a' = applyContext context a
--     putStrLn $ "Dirty type: " ++ show a'
--     putStrLn ""
--     putStrLn $ "Cleaned type: " ++ show (cleanupVars a')

main :: IO ()
main = do
    (Right term) <- parseFromFile lang "example.term"
    putStrLn $ "Inferring type for : " ++ show term
    putStrLn ""
    let pair = [] |- Synthesys term
    case pair of
        Just (a, context) -> do
            putStrLn $ "Final context: " ++ show context
            putStrLn ""
            let a' = applyContext context a
            putStrLn $ "Dirty type: " ++ show a'
            putStrLn ""
            putStrLn $ "Cleaned type: " ++ show (cleanupVars a')
        Nothing -> putStrLn "Type Not Inferred"
