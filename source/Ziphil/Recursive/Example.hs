{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ziphil.Recursive.Example
  ( FList
  , algPlusEx
  , algRepEx
  )
where

import Ziphil.Recursive.Base


data FList v a = Nil | Cons v a

instance Functor (FList v) where
  fmap func Nil = Nil
  fmap func (Cons val rest) = Cons val (func rest)

instance Fix (FList v) [v] where
  inF Nil = []
  inF (Cons val rest) = val : rest
  outF [] = Nil
  outF (val : rest) = Cons val rest

instance Fix (FList ()) Int where
  inF Nil = 0
  inF (Cons _ num) = num + 1
  outF 0 = Nil
  outF num = Cons () (num - 1)

algPlus :: Algebra (FList Int) Int
algPlus Nil = 0
algPlus (Cons val rest) = val + rest

algRep :: (Int -> Int) -> Algebra (FList ()) Int
algRep func Nil = 1
algRep func (Cons _ num) = func num

algPlusEx :: Int
algPlusEx = cata algPlus [8, 7, 4, 6, 9]

algRepEx :: Int
algRepEx = cata (algRep (* 2)) (10 :: Int)