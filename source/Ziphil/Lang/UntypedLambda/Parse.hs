--

module Ziphil.Lang.UntypedLambda.Parse
  ( parseTerm
  , parseTerm'
  , makeTerm
  , makeTerm'
  )
where

import Data.Either
import Text.Parsec
import Text.Parsec.String
import Ziphil.Lang.UntypedLambda.Base


parseTerm :: String -> Either ParseError (WithContext Term)
parseTerm text = parse getTermEof "" text

parseTerm' :: String -> Either ParseError Term
parseTerm' text = (\(context :- term) -> term) <$> parse getTermEof "" text

makeTerm :: String -> WithContext Term
makeTerm = fromRight (["?"] :- Var Info 0) . parseTerm

makeTerm' :: String -> Term
makeTerm' = fromRight (Var Info 0) . parseTerm'

getTermEof :: Parser (WithContext Term)
getTermEof = do
  wterm <- getTerm []
  _ <- eof
  return wterm

getTerm :: Context -> Parser (WithContext Term)
getTerm context = getNakedTerm context <|> getParenedTerm context

getNonappTerm :: Context -> Parser (WithContext Term)
getNonappTerm context = getParenedTerm context <|> getNakedNonappTerm context

getParenedTerm :: Context -> Parser (WithContext Term)
getParenedTerm context = do
  _ <- char '(' >> getSpaces
  newContext :- term <- getTerm context
  _ <- getSpaces >> char ')'
  return $ newContext :- term

getNakedTerm :: Context -> Parser (WithContext Term)
getNakedTerm context = try (getAppTerm context) <|> getNakedNonappTerm context

getNakedNonappTerm :: Context -> Parser (WithContext Term)
getNakedNonappTerm context = try (getAbsTerm context) <|> getSymTerm context <|> getVarTerm context

getVarTerm :: Context -> Parser (WithContext Term)
getVarTerm context = do
  name <- getVarName
  case fetchIndex context name of
    Just index -> return $ context :- Var Info index
    Nothing -> return $ (context ++ [name]) :- Var Info (length context)

getAbsTerm :: Context -> Parser (WithContext Term)
getAbsTerm context = do
  _ <- char 'λ' >> getSpaces
  name <- getVarName
  _ <- getSpaces >> char '.' >> getSpaces
  contContext :- contTerm <- getTerm $ name : context
  return $ tail contContext :- Abs Info name contTerm

getAppTerm :: Context -> Parser (WithContext Term)
getAppTerm context = do
  newContext :- terms <- getTermSequence context []
  return $ newContext :- foldl1 (\prevTerm curTerm -> App Info prevTerm curTerm) (reverse terms)

getTermSequence :: Context -> [Term] -> Parser (WithContext [Term])
getTermSequence context prevTerms = do
  headContext :- headTerm <- getNonappTerm context
  try (getSpaceTermSequence headContext (headTerm : prevTerms)) <|> do
    return $ headContext :- (headTerm : prevTerms)

getSpaceTermSequence :: Context -> [Term] -> Parser (WithContext [Term])
getSpaceTermSequence context prevTerms = do
  _ <- getSpaces
  getTermSequence context prevTerms

getSymTerm :: Context -> Parser (WithContext Term)
getSymTerm context = do
  _ <- char ':'
  name <- getVarName
  return $ context :- Sym Info name

getVarName :: Parser VarName
getVarName = do
  name <- many1 lower
  return name

getSpaces :: Parser ()
getSpaces = skipMany space

getSpaces1 :: Parser ()
getSpaces1 = skipMany1 space