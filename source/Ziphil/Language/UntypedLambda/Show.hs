{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ziphil.Language.UntypedLambda.Show
  (
  )
where

import qualified Text.Show as Show
import Ziphil.Language.UntypedLambda.Base


showRawTerm :: Term -> Text
showRawTerm (Var info index) = show index
showRawTerm (Abs info name contTerm) = "(λ. " <> showRawTerm contTerm <> ")"
showRawTerm (App info funcTerm valTerm) = "(" <> showRawTerm funcTerm <> " " <> showRawTerm valTerm <> ")"

showTerm :: WithContext Term -> Text
showTerm wterm@(_ :- Var _ _) = showVarTerm wterm
showTerm wterm@(_ :- Abs _ _ _) = showAbsTerm wterm
showTerm wterm@(_ :- App _ _ _) = showAppTerm wterm

showVarTerm :: WithContext Term -> Text
showVarTerm (context :- Var info index) = 
  case fetchName context index of
    Just name -> name
    Nothing -> "[" <> show (index - length context) <> "]"

showAbsTerm :: WithContext Term -> Text
showAbsTerm (context :- Abs info name contTerm) = "(λ" <> name' <> ". " <> showTerm (context' :- contTerm) <> ")"
  where
    (context', name') = pickFreshName context name

showAppTerm :: WithContext Term -> Text
showAppTerm (context :- App info funcTerm valTerm) = "(" <> showTerm (context :- funcTerm) <> " " <> showTerm (context :- valTerm) <> ")"

pickFreshName :: Context -> VarName -> (Context, VarName)
pickFreshName context name =
  if isNameBound context name
    then pickFreshName context (name <> "'")
    else (name : context, name)

isNameBound :: Context -> VarName -> Bool
isNameBound [] name = False
isNameBound (headName : rest) name =
  if name == headName
    then True
    else isNameBound rest name

instance Show.Show Term where
  show = toString . showTerm . ([] :-)

instance Show.Show (WithContext Term) where
  show = toString . showTerm