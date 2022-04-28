{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HKF.Error where

import Data.Foldable (foldl, maximumBy)
import qualified Data.Text as T
import Error.Diagnose (Report, def, err)
import Error.Diagnose.Diagnostic (Diagnostic, addReport, printDiagnostic)
import Error.Diagnose.Position (Position (Position))
import Error.Diagnose.Report (Marker (This, Where))
import HKF.Ast
import qualified HKF.Check as C
import Prettyprinter (Pretty)
import Prettyprinter hiding (indent)
import qualified Prettyprinter as P
import Prettyprinter.Render.Terminal
import qualified Prettyprinter.Render.Text as RT
import Text.Megaparsec (SourcePos (SourcePos))
import Text.Megaparsec.Pos (unPos)

type MyDoc = Doc PrettyAnnotation

type DiagnosticAnnotation = (Position, Marker MyDoc)

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

var :: Text -> MyDoc
var = annotate AVariable . pretty

keyword :: Text -> MyDoc
keyword = annotate AKeyword . pretty

punctuation :: Text -> MyDoc
punctuation = annotate APunctuation . pretty

natural :: Natural -> MyDoc
natural = annotate ANatural . pretty

operator :: Text -> MyDoc
operator = annotate AOperator . pretty

keycode :: Text -> MyDoc
keycode code =
  annotate AKeycode $
    qm <> pretty code <> qm
  where
    qm = "\""

withParenthesis :: MyDoc -> MyDoc
withParenthesis = surround (punctuation "(") (punctuation ")")

sometimesParenthesis :: Bool -> MyDoc -> MyDoc
sometimesParenthesis True = withParenthesis
sometimesParenthesis False = id

prettyPrintType :: C.EType -> MyDoc
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

prettyPrintCheckError ::
  (Maybe MyDoc -> MyDoc -> [DiagnosticAnnotation] -> [MyDoc] -> Report MyDoc) ->
  C.CheckErrorDetails ->
  Report MyDoc
prettyPrintCheckError err (C.VarNotInScope (Spanned span name)) =
  err
    (Just "VarNotInScope")
    "Variable not in scope"
    [(span, This $ hsep ["Variable", var name, "is not in scope!"])]
    [] -- TODO: did you mean hint
prettyPrintCheckError err (C.InvalidKeycode (Spanned span code)) =
  err
    (Just "InvalidKeycode")
    "Invalid keycode"
    [(span, This $ hsep [keycode code, "is not a valid keycode!"])]
    -- TODO: did you mean ...
    -- TODO: include list with all valid keycodes
    [hsep ["For a list of all the available keycodes, check ..."]]
prettyPrintCheckError err (C.DuplicateTemplateKeycode (Spanned span _) duplicates code) =
  err
    (Just "DuplicateKeycode")
    (hsep ["Keycode", keycode code, "appears in template more than once"])
    (reverse duplicateMarkers)
    ["Try removing one of the highlighted occurences"]
  where
    duplicateMarkers =
      zip [0 ..] (toList duplicates) <&> uncurry
        \i (Spanned span d) -> (span, This $ hsep ["Here is the", pretty (nth i), "occurence of this keycode!"])

    nth :: Int -> Text
    nth 0 = "first"
    nth 1 = "second"
    nth 2 = "third"
    nth 3 = "fourth"
    nth 4 = "fifth"
    nth n = show n <> "th"
prettyPrintCheckError glm _ = err Nothing "not implemented" [] []

-- prettyPrintCheckError (C.WrongTemplateLength (Spanned span _) templateName expected actual) =
--   Spanned span $
--     sep
--       [ "Wrong static layer length!",
--         "I was expecting" <+> natural expected
--           <+> "expressions,",
--         "as specified by the"
--           <+> var (unspan templateName)
--           <+> "template,",
--         "but got"
--           <+> natural actual
--           <+> "expressions instead."
--       ]
-- prettyPrintCheckError (C.AlreadyInScope (Spanned span name)) =
--   Spanned span $
--     hsep
--       [ "The name",
--         var name,
--         "is already in scope!"
--       ]
-- prettyPrintCheckError (C.NotCallable func arg tyFunc tyArg) =
--   Spanned (spanOf func) $
--     sep
--       [ "Expression cannot be called, as it has type",
--         indent $ prettyPrintType tyFunc -- Consider abstracting over this
--       ]
-- prettyPrintCheckError (C.WrongArgument func arg expected actual contradictions) =
--   Spanned (spanOf arg) $
--     sep
--       [ "Wrong argument type! Expecting",
--         indent $ prettyPrintType expected,
--         "but got",
--         indent $ prettyPrintType actual,
--         "instead!"
--       ]
-- prettyPrintCheckError (C.WrongType expected actual expr contradictions reason) =
--   Spanned (spanOf expr) $
--     sep
--       [ basic,
--         "I got stuck on the following contradictions:",
--         indent $ align $ vsep $ showContradiction <$> contradictions
--       ]
--   where
--     showContradiction (left, right) =
--       hsep
--         [ prettyPrintType left,
--           punctuation "<:",
--           prettyPrintType right
--         ]
--
--     basic = case reason of
--       C.MustSatisfyChord _ _ -> mustSatisfyList "chord"
--       C.MustSatisfySequence _ _ -> mustSatisfyList "sequence"
--       C.StaticLayerMember _ -> mustSatisfyList "static layer"
--       C.StaticLayersRequireTemplate _ _ ->
--         sep
--           [ "Only values of type",
--             indent $ prettyPrintType expected,
--             "can be used as a static layer template! Got",
--             indent $ prettyPrintType actual,
--             "instead."
--           ]
--
--     mustSatisfyList :: Text -> MyDoc
--     mustSatisfyList kind =
--       sep
--         [ hsep
--             [ "All elements inside a",
--               pretty kind,
--               "must have type"
--             ],
--           indent $ prettyPrintType expected,
--           "but this one has type",
--           indent $ prettyPrintType actual,
--           "instead."
--         ]
--
resolveAnnotation :: PrettyAnnotation -> AnsiStyle
resolveAnnotation ANatural = color Blue
resolveAnnotation AKeyword = bold <> color Yellow
resolveAnnotation AKeycode = bold <> color Cyan
resolveAnnotation AVariable = bold <> color Magenta
resolveAnnotation APunctuation = colorDull White
resolveAnnotation AOperator = colorDull White

instance Pretty MyDoc where
  pretty = unAnnotate

printErrors :: (Diagnostic MyDoc -> IO ()) -> [C.CheckError] -> IO ()
printErrors print =
  print
    -- renderStrict . showSpan
    -- . fmap (reAnnotate resolveAnnotation)
    . foldl addReport def
    . map \(C.MkCheckError generalLocation details) ->
      let generalLocationMarker Nothing = []
          generalLocationMarker (Just (Spanned span location)) = [(span, Where message)]
            where
              message = case location of
                C.WhileCheckingAlias -> "while checking this alias"
                C.WhileCheckingStaticLayer -> "while checking this static layer"
                C.WhileCheckingLayerTemplate -> "while checking this layer template"

          extraMarkers = generalLocationMarker generalLocation
          myErr code message markers = err code message (markers <> extraMarkers)
       in prettyPrintCheckError myErr details
