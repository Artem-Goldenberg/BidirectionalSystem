module Lib (module Lib) where

allWithSlices :: ([a] -> a -> Bool) -> [a] -> Bool
allWithSlices _ [] = True
allWithSlices f (a:as) = f as a && allWithSlices f as
