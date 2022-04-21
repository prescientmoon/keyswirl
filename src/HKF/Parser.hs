{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HKF.Parser where

import qualified Data.Text as T
import Data.Void
import GHC.IO (throwIO)
import GHC.Unicode (isAlpha)
import qualified HKF.Ast as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

type Parser = Parsec Void T.Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc =
  L.space
    (void $ takeWhile1P (Just "space") isSpace)
    lineComment
    empty
  where
    isSpace c = c == ' ' || c == '\t'

parens :: Parser () -> Parser a -> Parser a
parens sc' = between (L.symbol sc' "(") ")"

stringLiteral :: Parser () -> Parser T.Text
stringLiteral sc' = go <?> "string literal"
  where
    go = do
      r <- L.symbol sc' "\"" *> manyTill L.charLiteral "\""
      pure (T.pack r)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseName_ :: Parser Text
parseName_ = takeWhile1P (Just "name") \c ->
  isAlpha c
    || c == '-'
    || c == '_'

parseName :: Parser Text
parseName = lexeme parseName_

expression :: Parser () -> Parser A.Expression
expression sc' = do
  f <- atom sc' <?> "function"
  a <- many ((try sc' *> atom sc') <?> "function argument")
  pure case (a, f) of
    ([], _) -> f
    (_, A.Call f ia) -> A.Call f $ ia ++ a
    _ -> A.Call f a

atom :: Parser () -> Parser A.Expression
atom sc' = parseKey <|> parseParenthesis <|> parseVar
  where
    parseKey = A.Key <$> stringLiteral sc'
    parseVar = A.Variable <$> (parseName_ <?> "variable")
    parseParenthesis = parens sc' (expression sc')

    lm = L.lexeme sc'

parseBinder :: Parser A.Binder
parseBinder = (wildcard <|> named) <?> "binder"
  where
    named = (A.Named <$> parseName_) <?> "named binder"
    wildcard = ("_" $> A.Wildcard) <?> "wildcard"

patternMatchBranch :: Parser (A.PatternMatchBranch, A.Expression)
patternMatchBranch = L.lineFold scn \sc' -> do
  let lm = L.lexeme sc'
  L.symbol sc' "|"
  keycodes <- many (lm $ stringLiteral sc')
  vars <- many (lm parseBinder)
  rest <- optional (lm ("*" *> parseBinder) <?> "rest binder")
  L.symbol sc' "=>"
  e <- expression sc'
  scn
  pure (A.MkPatternMatchBranch keycodes vars rest, e)

toplevel :: Parser (T.Text, A.ToplevelEntry)
toplevel = L.nonIndented scn (tlTemplate <|> tlAlias <|> tLayer)
  where
    entryWithContinuation ::
      Parser a ->
      (Parser () -> Parser (Parser A.ToplevelEntry)) ->
      Parser (T.Text, A.ToplevelEntry)
    entryWithContinuation kind parser = do
      (name, continuation) <- L.lineFold scn \sc' -> do
        L.lexeme sc' kind <?> "declaration kind"
        name <- L.lexeme (try sc') parseName_ <?> "declaration name"
        continuation <- parser sc'
        scn
        pure (name, continuation)
      result <- continuation
      pure (name, result)

    entry ::
      Parser a ->
      (Parser () -> Parser A.ToplevelEntry) ->
      Parser (T.Text, A.ToplevelEntry)
    entry kind parser =
      entryWithContinuation
        kind
        (fmap pure . parser)

    tlAlias = entry "alias" \sc' -> do
      L.symbol sc' "="
      r <- expression sc'
      pure $ A.Alias r

    tlTemplate = entry "template" \sc' -> do
      L.symbol sc' ":"
      names <- parseName_ `sepBy1` try sc'
      pure $ A.LayerTemplate $ A.MkLayerTemplate names

    tLayer = entryWithContinuation "layer" \sc' -> do
      static <-
        (L.symbol sc' "using" $> True)
          <|> (":" $> False)
      if static
        then do
          templateName <- parseName_
          L.symbol sc' ":"
          contents <-
            (wildcard <|> A.ExpressionEntry <$> atom sc')
              `sepBy1` try sc'
          pure $
            pure $
              A.Layer $
                A.StaticLayer $ A.MkStaticLayer templateName contents
        else pure do
          branches <- some $ L.nonIndented scn patternMatchBranch
          pure $ A.Layer $ A.ComputeLayer $ A.MkComputeLayer branches

    wildcard = A.WildcardEntry <$ "_"

parseConfig :: Parser A.Config
parseConfig = A.MkConfig <$> some toplevel
