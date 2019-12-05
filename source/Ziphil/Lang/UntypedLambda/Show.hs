{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ziphil.Lang.UntypedLambda.Show
  (
  )
where

import Ziphil.Lang.UntypedLambda.Base


showRawTerm :: Term -> String
showRawTerm (Var info index) = show index
showRawTerm (Abs info name contTerm) = "(λ. " <> showRawTerm contTerm <> ")"
showRawTerm (App info funcTerm valTerm) = "(" <> showRawTerm funcTerm <> " " <> showRawTerm valTerm <> ")"
showRawTerm (Sym info name) = ":" <> name

showTerm :: WithContext Term -> String
showTerm wterm@(_ :- Var _ _) = showVarTerm wterm
showTerm wterm@(_ :- Abs _ _ _) = showAbsTerm wterm
showTerm wterm@(_ :- App _ _ _) = showAppTerm wterm
showTerm wterm@(_ :- Sym _ _) = showSymTerm wterm

showVarTerm :: WithContext Term -> String
showVarTerm (context :- Var info index) = 
  case fetchName context index of
    Just name -> name
    Nothing -> "[" <> show (index - length context) <> "]"

showAbsTerm :: WithContext Term -> String
showAbsTerm (context :- Abs info name contTerm) = "(λ" <> name' <> ". " <> showTerm (context' :- contTerm) <> ")"
  where
    (context', name') = pickFreshName context name

showAppTerm :: WithContext Term -> String
showAppTerm (context :- App info funcTerm valTerm) = "(" <> showTerm (context :- funcTerm) <> " " <> showTerm (context :- valTerm) <> ")"

showSymTerm :: WithContext Term -> String
showSymTerm (context :- Sym info name) = ":" <> name

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

instance Show Term where
  show = showTerm . ([] :-)

instance Show (WithContext Term) where
  show = showTerm