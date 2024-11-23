module Main (main) where

import Prelude hiding (id)
import Data.Maybe

import Terms
import Inference
import ContextUtils (applyContext)

main :: IO ()
main = do
    let (a, context) = fromJust $ [] |- Synthesys (App (App k id) om)
    print a
    print context
    print $ applyContext context a
    putStrLn ""
    print $ fromJust $ infer s
