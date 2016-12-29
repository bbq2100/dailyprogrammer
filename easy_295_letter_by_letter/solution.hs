module Main where

import           Control.Monad (forever)
import           Data.List     (nub)

replace :: Eq e => [e] -> [e] -> [[e]]
replace source target =
  let
    split = span (uncurry (==)) (zip source target)
    replace' (s,t:ts) = (s ++ [(snd t, snd t)], ts)
    replace' (s,[])   = (s,[])
    combine acc bs
      | null $ snd bs = acc ++ [bs]
      | otherwise = combine (acc ++ [bs]) $ replace' bs
    mkList (ss, ts) = (++) (fst <$> ss) (fst <$> ts)
  in nub [mkList x | x <- combine [] split]

main :: IO ()
main = forever $ do
    putStrLn "\nPlease enter the source and target word."
    source <- getLine
    target <- getLine
    mapM_ print $ replace source target
    return ()
