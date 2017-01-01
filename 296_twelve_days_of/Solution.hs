module Main where

import           Data.Char
import           Data.Map.Strict as M

main :: IO ()
main = readFile "song" >>= xchange >>= putStrLn

xchange :: String -> IO String
xchange [] = return ""
xchange (x:xs)
  | isDigit x = do
    xss <- xchange xs
    return $ swap x ++ " " ++ xss
  | otherwise = do
    xss <- xchange xs
    return $ x : xss
  where swap k = M.findWithDefault "" k $ M.fromList[('1', "one"), ('2', "two"), ('3', "three"), ('4', "four")] --yada yada
