module Main (main) where

import Data.Maybe

import Syntax
import Terms
import Inference
import TypeUtils
import ContextUtils


-- Just an example of inferring a type
main :: IO ()
main = do
    let term = App (App k id3) annomega  -- K id om = id
    putStrLn $ "Inferring type for: " ++ show term
    let (a, context) = fromJust $ [] |- Synthesys term
    putStrLn ""
    putStrLn $ "Final context: " ++ show context
    putStrLn ""
    let a' = applyContext context a
    putStrLn $ "Dirty term: " ++ show a'
    putStrLn ""
    putStrLn $ "Cleaned term: " ++ show (cleanupVars a')

