--

module Main where

import Ziphil.Lang.UntypedLambda


main :: IO ()
main = putStrLn . show . evaluate $ makeTerm program
  where
    program = unwords
      [ "let plus = λm. λn. λf. λs. m f (n f s) in"
      , "let two = λf. λs. f (f s) in"
      , "let one = λf. λs. f s in"
      , "plus two one"
      , ":f :s"
      ]