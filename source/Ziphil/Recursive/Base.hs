{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ziphil.Recursive.Base
  ( Algebra
  , Coalgebra
  , Fix (..)
  , cata
  , ana
  , hylo
  , hylo'
  , meta
  , meta'
  )
where


type Algebra f a = f a -> a
type Coalgebra f c = c -> f c

class Functor f => Fix f s where
  inF :: f s -> s
  outF :: s -> f s

cata :: Fix f s => Algebra f a -> s -> a
cata alg = alg . fmap (cata alg) . outF
 
ana :: Fix f t => Coalgebra f c -> c -> t
ana coalg = inF . fmap (ana coalg) . coalg

hylo :: forall f s a c. Fix f s => Algebra f a -> Coalgebra f c -> c -> a
hylo alg coalg = cata alg . (ana @f @s coalg)

hylo' :: forall f s a c. Fix f s => Algebra f a -> Coalgebra f c -> c -> a
hylo' alg coalg = alg . fmap (hylo @f @s alg coalg) . coalg

meta :: Fix f s => Algebra f a -> Coalgebra f a -> s -> s
meta alg coalg = ana coalg . cata alg

meta' :: forall f s a. Fix f s => Algebra f a -> Coalgebra f a -> s -> s
meta' alg coalg = inF . fmap @f (meta' alg coalg) . outF