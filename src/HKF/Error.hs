module HKF.Error where

import Data.Foldable (maximumBy)
import qualified Data.Text as T
import HKF.Ast
import qualified HKF.Check as C
import Prettyprinter hiding (indent)
import qualified Prettyprinter as P
import Prettyprinter.Render.Terminal
import qualified Prettyprinter.Render.Text as RT
import Text.Megaparsec (SourcePos (SourcePos))
import Text.Megaparsec.Pos (unPos)

data PrettyAnnotation
  = ANatural
  | AOperator
  | AKeyword
  | AVariable
  | AKeycode
  | APunctuation

indentation :: Int
indentation = 2

indent :: Doc a -> Doc a
indent x = flatAlt (P.indent indentation x) x

var :: Text -> Doc PrettyAnnotation
var = annotate AVariable . pretty

keyword :: Text -> Doc PrettyAnnotation
keyword = annotate AKeyword . pretty

punctuation :: Text -> Doc PrettyAnnotation
punctuation = annotate APunctuation . pretty

natural :: Natural -> Doc PrettyAnnotation
natural = annotate ANatural . pretty

operator :: Text -> Doc PrettyAnnotation
operator = annotate AOperator . pretty

keycode :: Text -> Doc PrettyAnnotation
keycode code =
  annotate AKeycode $
    qm <> pretty code <> qm
  where
    qm = "\""

withParenthesis :: Doc PrettyAnnotation -> Doc PrettyAnnotation
withParenthesis = surround (punctuation "(") (punctuation ")")

sometimesParenthesis :: Bool -> Doc PrettyAnnotation -> Doc PrettyAnnotation
sometimesParenthesis True = withParenthesis
sometimesParenthesis False = id

prettyPrintType :: C.EType -> Doc PrettyAnnotation
prettyPrintType C.TBroken = operator "?"
prettyPrintType C.TKeycode = keyword "Keycode"
prettyPrintType C.TSequence = keyword "Sequence"
prettyPrintType C.TChord = keyword "Chord"
prettyPrintType C.TTemplate = keyword "LayerTemplate"
prettyPrintType (C.TArrow from to) =
  align $ sep [left, hsep [arrow, right]]
  where
    right = prettyPrintType to
    left = sometimesParenthesis (needsParens from) (prettyPrintType from)
    needsParens (C.TArrow _ _) = True
    needsParens _ = False
    arrow = operator "->"

prettyPrintCheckError :: C.CheckError -> Spanned (Doc PrettyAnnotation)
prettyPrintCheckError (C.VarNotInScope (Spanned span name)) =
  -- TODO: did you mean ...
  Spanned span $
    hsep ["Variable", var name, "is not in scope!"]
prettyPrintCheckError (C.InvalidKeycode (Spanned span code)) =
  -- TODO: did you mean ...
  -- TODO: include list with all valid keycodes
  Spanned span $
    hsep [keycode code, "is not a valid keycode!"]
prettyPrintCheckError (C.DuplicateTemplateKeycode (Spanned span _) code) =
  Spanned span $
    hsep [keycode code, "appears more than once in this template!"]
prettyPrintCheckError (C.WrongTemplateLength (Spanned span _) templateName expected actual) =
  Spanned span $
    sep
      [ "Wrong static layer length!",
        "I was expecting" <+> natural expected
          <+> "expressions,",
        "as specified by the"
          <+> var (unspan templateName)
          <+> "template,",
        "but got"
          <+> natural actual
          <+> "expressions instead."
      ]
prettyPrintCheckError (C.AlreadyInScope (Spanned span name)) =
  Spanned span $
    hsep
      [ "The name",
        var name,
        "is already in scope!"
      ]
prettyPrintCheckError (C.NotCallable func arg tyFunc tyArg) =
  Spanned (spanOf func) $
    sep
      [ "Expression cannot be called, as it has type",
        indent $ prettyPrintType tyFunc -- Consider abstracting over this
      ]
prettyPrintCheckError (C.WrongArgument func arg expected actual contradictions) =
  Spanned (spanOf arg) $
    sep
      [ "Wrong argument type! Expecting",
        indent $ prettyPrintType expected,
        "but got",
        indent $ prettyPrintType actual,
        "instead!"
      ]
prettyPrintCheckError (C.WrongType expected actual expr contradictions reason) =
  Spanned (spanOf expr) $
    sep
      [ basic,
        "I got stuck on the following contradictions:",
        indent $ align $ vsep $ showContradiction <$> contradictions
      ]
  where
    showContradiction (left, right) =
      hsep
        [ prettyPrintType left,
          punctuation "<:",
          prettyPrintType right
        ]

    basic = case reason of
      C.MustSatisfyChord _ _ -> mustSatisfyList "chord"
      C.MustSatisfySequence _ _ -> mustSatisfyList "sequence"
      C.StaticLayerMember _ -> mustSatisfyList "static layer"
      C.StaticLayersRequireTemplate _ _ ->
        sep
          [ "Only values of type",
            indent $ prettyPrintType expected,
            "can be used as a static layer template! Got",
            indent $ prettyPrintType actual,
            "instead."
          ]

    mustSatisfyList :: Text -> Doc PrettyAnnotation
    mustSatisfyList kind =
      sep
        [ hsep
            [ "All elements inside a",
              pretty kind,
              "must have type"
            ],
          indent $ prettyPrintType expected,
          "but this one has type",
          indent $ prettyPrintType actual,
          "instead."
        ]

resolveAnnotation :: PrettyAnnotation -> AnsiStyle
resolveAnnotation ANatural = color Blue
resolveAnnotation AKeyword = bold <> color Yellow
resolveAnnotation AKeycode = bold <> color Cyan
resolveAnnotation AVariable = bold <> color Magenta
resolveAnnotation APunctuation = colorDull White
resolveAnnotation AOperator = colorDull White

getWidth :: LayoutOptions -> Doc a -> Int
getWidth layoutOptions =
  foldr max 0
    . fmap T.length
    . lines
    . RT.renderStrict
    . layoutSmart layoutOptions

errorToText :: Text -> [Text] -> C.CheckError -> Text
errorToText file lines =
  renderStrict . showSpan
    . fmap (reAnnotate resolveAnnotation)
    . prettyPrintCheckError
  where
    showSpan :: Spanned (Doc AnsiStyle) -> SimpleDocStream AnsiStyle
    showSpan (Spanned (Span (SourcePos _ l0 c0, SourcePos _ l1 _) (start, end)) doc) =
      layoutSmart options $
        vsep
          [ source,
            P.indent defaultIndentation "^",
            error
              & P.indent
                if errorWidth + defaultIndentation >= sourceWidth
                  then defaultIndentation - errorWidth + 2
                  else defaultIndentation,
            -- hsep $ pretty <$> [sectionStart, sectionEnd, start, end, errorWidth, sourceWidth],
            "\n"
          ]
      where
        options = LayoutOptions $ AvailablePerLine (max 80 sourceWidth) 1

        errorWidth = getWidth options (P.indent defaultIndentation error) - defaultIndentation
        sourceWidth = getWidth (LayoutOptions Unbounded) source
        defaultIndentation = unPos c0 - 1
        error = annotate (color Red) doc
        source =
          hcat
            [ pretty prefix,
              annotate (bold <> underlined) $ pretty remaining,
              pretty suffix
            ]

        substr from to = T.take (to - from) . T.drop from

        prefix = substr sectionStart start file
        suffix = substr end sectionEnd file
        remaining = substr start end file

        countLength x = x + sum (T.length <$> take x lines)
        sectionStart = countLength (unPos l0 - 1)
        sectionEnd = countLength (unPos l1) - 1
