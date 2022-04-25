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

spanned :: Parser a -> Parser (A.Spanned a)
spanned p = do
  start <- getSourcePos
  result <- p
  end <- getSourcePos
  pure $ A.Spanned (A.Span start end) result

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

customParens :: T.Text -> T.Text -> Parser () -> Parser a -> Parser a
customParens l r sc' = between (L.symbol sc' l) (string r)

parens :: Parser () -> Parser a -> Parser a
parens = customParens "(" ")"

squareBraces :: Parser () -> Parser a -> Parser a
squareBraces = customParens "[" "]"

curlyBraces :: Parser () -> Parser a -> Parser a
curlyBraces = customParens "{" "}"

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
  f <- atom sc'
  a <- many ((try sc' *> atom sc') <?> "function argument")
  case (a, f) of
    ([], _) -> pure f
    (_, A.Spanned _ (A.Call f ia)) -> spanned $ pure $ A.Call f $ ia ++ a
    _ -> spanned $ pure $ A.Call f a

atom :: Parser () -> Parser A.Expression
atom sc' = key <|> parseParenthesis <|> var <|> chord <|> sequence
  where
    key = spanned $ A.Key <$> spanned (stringLiteral sc')
    var = spanned $ A.Variable <$> (spanned parseName_ <?> "variable")
    parseParenthesis = parens sc' (expression sc')
    chord = spanned $ A.Chord <$> curlyBraces sc' (sepBy (expression sc') $ lm ",")
    sequence = spanned $ A.Sequence <$> squareBraces sc' (sepBy (expression sc') $ lm ",")

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

toplevel :: Parser A.ConfigEntry
toplevel = L.nonIndented scn (tlTemplate <|> tInput <|> tOutput <|> tlAlias <|> tLayer)
  where
    namedDeclarationWithContinuation ::
      Parser a ->
      (Parser () -> Parser (Parser A.ToplevelDeclaration)) ->
      Parser A.ConfigEntry
    namedDeclarationWithContinuation kind parser = do
      (name, continuation) <- L.lineFold scn \sc' -> do
        L.lexeme sc' kind <?> "declaration kind"
        name <- spanned (L.lexeme (try sc') parseName_ <?> "declaration name")
        continuation <- spanned (parser sc')
        scn
        pure (name, continuation)
      result <- spanned (A.unspan continuation)
      pure $ A.NamedConfigEntry name (A.Spanned (A.spanOf result <> A.spanOf continuation) $ A.unspan result)

    namedDeclaration ::
      Parser a ->
      (Parser () -> Parser A.ToplevelDeclaration) ->
      Parser A.ConfigEntry
    namedDeclaration kind parser =
      namedDeclarationWithContinuation
        kind
        (fmap pure . parser)

    unnamedDeclaration ::
      Parser a ->
      (Parser () -> Parser A.UnnamedConfigEntry) ->
      Parser A.ConfigEntry
    unnamedDeclaration kind parser =
      A.UnnamedConfigEntry <$> L.lineFold scn \sc' -> do
        L.lexeme sc' kind <?> "declaration kind"
        parser sc' <* scn

    tlAlias = namedDeclaration "alias" \sc' -> do
      L.symbol sc' "="
      r <- expression sc'
      pure $ A.Alias r

    tlTemplate = namedDeclaration "template" \sc' -> do
      L.symbol sc' ":"
      names <- spanned parseName_ `sepBy1` try sc'
      pure $ A.LayerTemplate $ A.MkLayerTemplate names

    tOutput = unnamedDeclaration "output" \sc' -> do
      value <- stringLiteral sc'
      pure $ A.Output value

    tInput = unnamedDeclaration "input" \sc' -> do
      let sym = L.symbol sc'
      let inputByName =
            sym "name" $> A.InputByName
      let inputByPath =
            sym "path" $> A.InputByPath
      kind <- (inputByName <|> inputByPath) <?> "input kind"
      value <- stringLiteral sc'
      pure (A.Input $ kind value)

    tLayer = namedDeclarationWithContinuation "layer" \sc' -> do
      static <-
        (L.symbol sc' "using" $> True)
          <|> (":" $> False)
      if static
        then do
          templateName <- spanned parseName_
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
parseConfig = A.MkConfig <$> some (spanned toplevel)
