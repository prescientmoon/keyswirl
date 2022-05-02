module HKF.Parser where

import Data.Char (isAlphaNum)
import Data.Foldable
import qualified Data.Text as T
import Data.Void
import GHC.IO (throwIO)
import GHC.Unicode (isAlpha)
import qualified HKF.Ast as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

newtype ParsingContext = MkParsingContext {danger :: Bool}

type Parser = ParsecT Void Text (Reader ParsingContext)

isUnsafe :: Parser Bool
isUnsafe = asks danger

spanned :: Parser a -> Parser (A.Spanned a)
spanned p = do
  (SourcePos filename l0 c0) <- getSourcePos
  result <- p
  (SourcePos _ l1 c1) <- getSourcePos
  pure $ A.Spanned (A.span (both unPos (l0, c0)) (both unPos (l1, c1)) filename) result
  where
    both :: (a -> b) -> (a, a) -> (b, b)
    both f (x, y) = (f x, f y)

extendSpan :: A.Span -> Parser (A.Spanned a) -> Parser (A.Spanned a)
extendSpan extra parser = do
  A.Spanned span result <- parser
  pure $ A.Spanned (fromMaybe span (A.mergeSpans span extra)) result

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

slimArrow :: Parser () -> Parser Text
slimArrow sc' = L.symbol sc' "->" <|> L.symbol sc' "→"

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
  isAlphaNum c
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

parseLambdaHead :: Bool -> Parser () -> Parser (A.Expression -> A.Expression)
parseLambdaHead required sc' = do
  arguments <- (if required then some else many) $
    spanned . lm $ parens sc' do
      name <- lm $ spanned parseName_
      L.symbol sc' ":"
      ty <- lm $ spanned (etype sc')
      pure (name, ty)
  annotation <- optional do
    L.symbol sc' ":"
    lm $ spanned $ etype sc'
  pure \inner ->
    let annotated = case annotation of
          Nothing -> inner
          Just ty ->
            A.Spanned
              (A.mergeSpans' (A.spanOf inner) (A.spanOf ty))
              $ A.Annotation ty inner
        buildLambda (A.Spanned span (name, ty)) body =
          A.Spanned (A.mergeSpans' span $ A.spanOf body) $
            A.Lambda name ty body
     in foldr buildLambda annotated arguments
  where
    lm = L.lexeme sc'

atom :: Parser () -> Parser A.Expression
atom sc' = key <|> parseParenthesis <|> lambda <|> var <|> chord <|> sequence
  where
    -- TODO: annotations
    key = spanned $ A.Key <$> spanned (stringLiteral sc')
    var = spanned $ A.Variable <$> (spanned parseName_ <?> "variable")
    parseParenthesis = parens sc' (expression sc')
    chord = spanned $ A.Chord <$> curlyBraces sc' (sepBy (expression sc') $ lm ",")
    sequence = spanned $ A.Sequence <$> squareBraces sc' (sepBy (expression sc') $ lm ",")
    lambda = do
      A.Spanned span _ <- spanned (L.symbol sc' "fun" <|> L.symbol sc' "λ")
      buildLambda <- parseLambdaHead True sc'
      L.symbol sc' "=>"
      inner <- expression sc'
      extendSpan span $ pure $ buildLambda inner

    lm = L.lexeme sc'

parseBinder :: Parser A.Binder
parseBinder = (wildcard <|> named) <?> "binder"
  where
    named = (A.Named <$> parseName_) <?> "named binder"
    wildcard = ("_" $> A.Wildcard) <?> "wildcard"

patternMatchBranch :: Parser (A.Spanned A.PatternMatchBranch, A.Expression)
patternMatchBranch = L.lineFold scn \sc' -> do
  let lm = L.lexeme sc'
  L.symbol sc' "|"
  A.Spanned span (keycodes, vars, rest) <- spanned do
    keycodes <- many (lm $ spanned $ stringLiteral sc')
    vars <- many (lm $ spanned parseBinder)
    rest <- optional (lm ("*" *> spanned parseBinder) <?> "rest binder")
    pure (keycodes, vars, rest)
  L.symbol sc' "=>"
  e <- expression sc'
  scn
  pure (A.Spanned span $ A.MkPatternMatchBranch keycodes vars rest, e)

etype :: Parser () -> Parser A.EType
etype sc' = do
  first <- tAtom
  other <- many (try (sc' *> slimArrow sc') *> tAtom)
  pure $ foldr1 A.TArrow (first : other)
  where
    tAtom = chord <|> template <|> sequence <|> layer <|> keycode <|> broken <|> parseParenthesis
    parseParenthesis = parens sc' (etype sc')

    broken = do
      unsafe <- isUnsafe
      if unsafe then tconst "Broken" A.TBroken else empty
    chord = tconst "Chord" A.TChord
    layer = tconst "Layer" (A.TArrow A.TChord A.TSequence)
    sequence = tconst "Sequence" A.TSequence
    keycode = tconst "Keycode" A.TKeycode
    template = tconst "LayerTemplate" A.TTemplate

    tconst name value = string name $> value

toplevel :: Parser (A.Spanned A.ConfigEntry)
toplevel = L.nonIndented scn (tlTemplate <|> tInput <|> tOutput <|> tlAlias <|> tlLayer <|> tlAssumption)
  where
    namedDeclarationWithContinuation ::
      Parser a ->
      (Parser () -> Parser (Parser (A.Spanned A.ToplevelDeclaration))) ->
      Parser (A.Spanned A.ConfigEntry)
    namedDeclarationWithContinuation kind parser = do
      A.Spanned span (name, continuation) <- L.lineFold scn \sc' -> do
        let withSpan = do
              L.lexeme sc' kind <?> "declaration kind"
              name <- spanned (L.lexeme (try sc') parseName_ <?> "declaration name")
              continuation <- parser sc'
              pure (name, continuation)
        spanned withSpan <* scn
      A.Spanned span . A.NamedConfigEntry name <$> continuation

    namedDeclaration ::
      Parser a ->
      (Parser () -> Parser A.ToplevelDeclaration) ->
      Parser (A.Spanned A.ConfigEntry)
    namedDeclaration kind parser =
      namedDeclarationWithContinuation
        kind
        (fmap pure . spanned . parser)

    unnamedDeclaration ::
      Parser a ->
      (Parser () -> Parser A.UnnamedConfigEntry) ->
      Parser (A.Spanned A.ConfigEntry)
    unnamedDeclaration kind parser =
      fmap A.UnnamedConfigEntry <$> L.lineFold scn \sc' -> do
        let parseKind = L.lexeme sc' kind <?> "declaration kind"
        spanned (parseKind *> parser sc') <* scn

    tlAssumption = do
      unsafe <- isUnsafe
      if not unsafe
        then empty
        else namedDeclaration "assume" \sc' -> do
          L.symbol sc' ":"
          ty <- spanned $ etype sc'
          pure $ A.Assumption ty

    tlAlias = namedDeclaration ("alias" <|> "def") \sc' -> do
      buildLambda <- parseLambdaHead False sc'
      L.symbol sc' "="
      r <- expression sc'
      pure $ A.Alias $ buildLambda r

    tlTemplate = namedDeclaration "template" \sc' -> do
      L.symbol sc' ":"
      names <- spanned (spanned parseName_ `sepBy1` try sc')
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

    tlLayer = namedDeclarationWithContinuation "layer" \sc' -> do
      A.Spanned startingSpan static <-
        spanned $
          (L.symbol sc' "using" $> True)
            <|> (":" $> False)
      extendSpan startingSpan
        <$> if static
          then (fmap pure . spanned) do
            templateName <- spanned parseName_
            L.symbol sc' ":"
            let entry = wildcard <|> A.ExpressionEntry <$> atom sc'
            contents <- spanned $ many ((try sc' *> entry) <?> "layer entry")
            pure $
              A.Layer $
                A.StaticLayer $ A.MkStaticLayer templateName contents
          else pure $ spanned do
            branches <- some $ L.nonIndented scn patternMatchBranch
            pure $ A.Layer $ A.ComputeLayer $ A.MkComputeLayer branches

    wildcard :: Parser A.StaticLayerEntry
    wildcard = A.WildcardEntry <$ "_"

parseConfig :: Parser A.Config
parseConfig = A.MkConfig <$> some toplevel
