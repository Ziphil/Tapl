{-# LANGUAGE NegativeLiterals #-}

module Ziphil.Language.UntypedLambda.Base
  ( Index
  , VarName
  , Info (..)
  , Term (..)
  , Context
  , WithContext (..)
  , shift
  , shift'
  , substitute
  , evaluateOnce
  , evaluate
  , fetchIndex
  , fetchName
  )
where

import Data.List (elemIndex)


type Index = Int
type VarName = Text

data Info = Info

-- ラムダ計算の項を表す型です。
-- 変項は de Bruijn インデックスによって表現します。
data Term = Var Info Index | Abs Info VarName Term | App Info Term Term

-- 文脈を表します。
-- 最も左 (インデックスが小さい) ものほど階層の深い位置での変項に対応します。
type Context = [VarName]

infix 9 :-
data WithContext a = Context :- a

shift :: Int -> Term -> Term
shift = shift' 0

shift' :: Int -> Int -> Term -> Term
shift' thresh num (Var info index)
  | index >= thresh = Var info (index + num)
  | otherwise = Var info index
shift' thresh num (Abs info name contTerm) = Abs info name (shift' (thresh + 1) num contTerm)
shift' thresh num (App info funcTerm valTerm) = App info (shift' thresh num funcTerm) (shift' thresh num valTerm)

substitute :: Index -> Term -> Term -> Term
substitute tarIndex tarTerm (Var info index)
  | index == tarIndex = tarTerm
  | otherwise = Var info index
substitute tarIndex tarTerm (Abs info name contTerm) = Abs info name (substitute (tarIndex + 1) (shift 1 tarTerm) contTerm)
substitute tarIndex tarTerm (App info funcTerm valTerm) = App info (substitute tarIndex tarTerm funcTerm) (substitute tarIndex tarTerm valTerm)

evaluateOnce :: WithContext Term -> Maybe Term
evaluateOnce (context :- App info (Abs _ name contTerm) valTerm)
  | isValue context valTerm = Just $ shift -1 (substitute 0 (shift 1 valTerm) contTerm)
evaluateOnce (context :- App info funcTerm valTerm)
  | isValue context funcTerm = (\newTerm -> App info funcTerm newTerm) <$> evaluateOnce (context :- valTerm)
  | otherwise = (\newTerm -> App info newTerm valTerm) <$> evaluateOnce (context :- funcTerm)
evaluateOnce (context :- _) = Nothing

evaluate :: WithContext Term -> Term
evaluate (context :- term) =
  case evaluateOnce (context :- term) of
    Just newTerm -> evaluate (context :- newTerm)
    Nothing -> term

isValue :: Context -> Term -> Bool
isValue context (Abs _ _ _) = True
isValue context _ = False

fetchIndex :: Context -> VarName -> Maybe Index
fetchIndex context name = elemIndex name context

fetchName :: Context -> Index -> Maybe VarName
fetchName context index = context !!? index