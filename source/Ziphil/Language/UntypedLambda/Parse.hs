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
getTerm context = getParenedTerm context <|> getNakedTerm context

getNonappTerm :: Context -> Parser (WithContext Term)
getNonappTerm context = getParenedTerm context <|> getNakedNonappTerm context

getNakedTerm :: Context -> Parser (WithContext Term)
getNakedTerm context = try (getAbsTerm context) <|> try (getAppTerm context) <|> getVarTerm context

getNakedNonappTerm :: Context -> Parser (WithContext Term)
getNakedNonappTerm context = try (getAbsTerm context) <|> getVarTerm context

getParenedTerm :: Context -> Parser (WithContext Term)
getParenedTerm context = do
  _ <- char '(' >> getSpaces
  newContext :- term <- getTerm context
  _ <- getSpaces >> char ')'
  return (newContext :- term)

getVarTerm :: Context -> Parser (WithContext Term)
getVarTerm context = do
  name <- getVarName
  case fetchIndex context name of
    Just index -> return (context :- Var Info index)
    Nothing -> return ((context ++ [name]) :- Var Info (length context))

getAbsTerm :: Context -> Parser (WithContext Term)
getAbsTerm context = do
  _ <- char 'λ' >> getSpaces
  name <- getVarName
  _ <- getSpaces >> char '.' >> getSpaces
  contContext :- contTerm <- getTerm (name : context)
  case viaNonEmpty tail contContext of
    Just finalContext -> return (finalContext :- Abs Info name contTerm)
    Nothing -> error "weird"

getAppTerm :: Context -> Parser (WithContext Term)
getAppTerm context = do
  newContext :- terms <- getTermSequence context []
  case reverse terms of
    headTerm : restTerms -> do
      let newTerm = foldl' (\prevTerm curTerm -> App Info prevTerm curTerm) headTerm restTerms
      return (newContext :- newTerm)
    [] -> error "weird"

getTermSequence :: Context -> [Term] -> Parser (WithContext [Term])
getTermSequence context prevTerms = do
  headContext :- headTerm <- getNonappTerm context
  try (getSpaces >> getTermSequence headContext (headTerm : prevTerms)) <|> return (headContext :- (headTerm : prevTerms))

getVarName :: Parser VarName
getVarName = do
  name <- many1 lower
  return $ toText name

getSpaces :: Parser ()
getSpaces = skipMany space

getSpaces1 :: Parser ()
getSpaces1 = skipMany1 space