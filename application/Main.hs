--

module Main where

import Ziphil.Language.UntypedLambda


main :: IO ()
main = do
  putStrLn . show . makeTerm $ "x y"