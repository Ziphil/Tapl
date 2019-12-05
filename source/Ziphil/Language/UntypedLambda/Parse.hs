--

module Ziphil.Language.UntypedLambda.Parse
  ( parseTerm
  , parseTerm'
  , makeTerm
  , makeTerm'
  )
where

import Prelude hiding ((<|>))
import Text.Parsec
import Text.Parsec.String
import Ziphil.Language.UntypedLambda.Base


parseTerm :: String -> Either ParseError (WithContext Term)
parseTerm text = parse (getTerm []) "" text

parseTerm' :: String -> Either ParseError Term
parseTerm' text = (\(context :- term) -> term) <$> parse (getTerm []) "" text

makeTerm :: String -> WithContext Term
makeTerm = fromRight (["?"] :- Var Info 0) . parseTerm

makeTerm' :: String -> Term
makeTerm' = fromRight (Var Info 0) . parseTerm'

getTerm :: Context -> Parser (WithContext Term)
getTerm context = try (getVarTerm context) <|> try (getAbsTerm context) <|> try (getAppTerm context)

getVarTerm :: Context -> Parser (WithContext Term)
getVarTerm context = do
  name <- getVarName
  case fetchIndex context name of
    Just index -> return (context :- Var Info index)
    Nothing -> return ((context ++ [name]) :- Var Info (length context))

getAbsTerm :: Context -> Parser (WithContext Term)
getAbsTerm context = do
  char '('
  getSpaces
  char 'λ'
  getSpaces
  name <- getVarName
  getSpaces
  char '.'
  getSpaces
  contContext :- contTerm <- getTerm (name : context)
  getSpaces
  char ')'
  case viaNonEmpty tail contContext of
    Just finalContext -> return (finalContext :- Abs Info name contTerm)
    Nothing -> fail "weird"

getAppTerm :: Context -> Parser (WithContext Term)
getAppTerm context = do
  char '('
  funcContext :- funcTerm <- getTerm context
  getSpaces
  valContext :- valTerm <- getTerm funcContext
  getSpaces
  char ')'
  return (valContext :- App Info funcTerm valTerm)

getVarName :: Parser VarName
getVarName = do
  name <- many1 lower
  return $ toText name

getSpaces :: Parser ()
getSpaces = skipMany space

getSpaces1 :: Parser ()
getSpaces1 = skipMany1 space