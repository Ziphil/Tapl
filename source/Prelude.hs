--

module Prelude
  ( module Relude
  , (!!?)
  )
where

import Relude


-- バージョン 0.6.0 以降の Relude で提供されている演算子です。
-- これ未満のバージョンがインストールされている場合でも使えるように再定義してあります。
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if index < 0
    then Nothing
    else go list index
  where
    go [] _ = Nothing
    go (val : _) 0 = Just val
    go (_ : rest) j = go rest (j - 1)