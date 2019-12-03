--

module Main where

import Ziphil.Language.UntypedLambda


main :: IO ()
main = do
  putTextLn . show . evaluate . makeTerm $ "((λs. (s s)) (λt. t))"
  putTextLn . show . evaluate . makeTerm $ "((λy. (λx. (y (y x)))) (λx. x))"