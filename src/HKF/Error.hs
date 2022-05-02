{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HKF.Error where

import Data.Foldable (foldl, maximumBy)
import qualified Data.Text as T
import Error.Diagnose (Report, def, err)
import Error.Diagnose.Diagnostic (Diagnostic, addReport, printDiagnostic)
import Error.Diagnose.Position (Position (Position))
import Error.Diagnose.Report (Marker (Maybe, This, Where))
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

quoted :: MyDoc -> MyDoc
quoted a = surround a qm qm
  where
    qm = "\""

keycode :: Text -> MyDoc
keycode = annotate AKeycode . quoted . pretty

withParenthesis :: MyDoc -> MyDoc
withParenthesis = surround (punctuation "(") (punctuation ")")

sometimesParenthesis :: Bool -> MyDoc -> MyDoc
sometimesParenthesis True = withParenthesis
sometimesParenthesis False = id

duplicateMarkers :: Text -> NonEmpty (Spanned a) -> [DiagnosticAnnotation]
duplicateMarkers what duplicates =
  reverse $
    zip [0 ..] (toList duplicates) <&> uncurry
      \i (Spanned span _) -> (span, This $ hsep ["the", pretty (nth i), "occurence of this", pretty what])
  where
    nth :: Int -> Text
    nth 0 = "first"
    nth 1 = "second"
    nth 2 = "third"
    nth 3 = "fourth"
    nth 4 = "fifth"
    nth n = show n <> "th"

prettyPrintType :: EType -> MyDoc
prettyPrintType TBroken = operator "?"
prettyPrintType TKeycode = keyword "Keycode"
prettyPrintType TSequence = keyword "Sequence"
prettyPrintType TChord = keyword "Chord"
prettyPrintType TTemplate = keyword "LayerTemplate"
prettyPrintType (TArrow from to) =
  align $ sep [left, hsep [arrow, right]]
  where
    right = prettyPrintType to
    left = sometimesParenthesis (needsParens from) (prettyPrintType from)
    needsParens (TArrow _ _) = True
    needsParens _ = False
    arrow = operator "->"

prettyPrintCheckError ::
  (Maybe MyDoc -> MyDoc -> [DiagnosticAnnotation] -> [MyDoc] -> Report MyDoc) ->
  C.CheckErrorDetails ->
  Report MyDoc
prettyPrintCheckError err (C.VarNotInScope (Spanned span name) _ (Just (Spanned futureSpan _))) =
  err
    (Just "VarNotInScope")
    (hsep ["Variable", quoted (var name), "used before it's declaration"])
    [(span, This $ hsep ["Variable", quoted (var name), "hasn't been defined at this point"]), futureMarker]
    [futureHint]
  where
    futureHint = hsep ["Try moving this declaration under the place", quoted (var name), "is defiend"]
    futureMarker = (futureSpan, Maybe "The variable is defined here")
prettyPrintCheckError err (C.VarNotInScope (Spanned span name) similar Nothing) =
  err
    (Just "VarNotInScope")
    "Undefined variable"
    ((span, This $ hsep ["Undefined variable", var name]) : similarityMarker)
    similarityHint
  where
    similarityMarker = case similar of
      [] -> []
      (Spanned span target : _) -> pure (span, Maybe $ hsep ["Were you refering to this?"])
    similarityHint = case similar of
      [] -> []
      _ -> [hsep ["You might be referring to one of the following:", hcat $ intersperse ", " (fmap (pretty . unspan) similar)]] -- TODO: did you mean hint
prettyPrintCheckError err (C.InvalidKeycode (Spanned span code)) =
  err
    (Just "InvalidKeycode")
    "Invalid keycode"
    [(span, This $ hsep [keycode code, "is not a valid keycode!"])]
    -- TODO: did you mean ...
    -- TODO: include list with all valid keycodes
    [hsep ["For a list of all the available keycodes, check ..."]]
prettyPrintCheckError err (C.DuplicatePatternKeycode branch instances code) =
  err
    (Just "DuplicatePatternKeycode")
    (hsep ["Keycode", keycode code, "appears more than once in a single layer branch"])
    (duplicateMarkers "keycode" instances)
    ["Try removing all but one of the highlighted occurences"]
prettyPrintCheckError err (C.DuplicatePatternBinder branch binders name) =
  err
    (Just "DuplicatePatternBinder")
    (hsep ["Name", quoted (var name), "is bound more than once in the same layer branch"])
    (duplicateMarkers "binder" binders)
    ["Try removing all but one of the highlighted occurences"]
prettyPrintCheckError err (C.DuplicateTemplateKeycode (Spanned span _) duplicates code) =
  err
    (Just "DuplicateTemplateKeycode")
    (hsep ["Keycode", keycode code, "appears in template more than once"])
    (duplicateMarkers "keycode" duplicates)
    ["Try removing all but one of the highlighted occurences"]
prettyPrintCheckError err (C.WrongTemplateLength (Spanned span _) template templateName expected actual) =
  err
    (Just "WrongStaticLayerLength")
    "The length of the static layer does not match the length of the template it uses"
    [ (span, This $ hsep ["...but this layer", if expected > actual then "only has" else "", natural actual, "members"]),
      (spanOf templateName, Where "This is the template being used"),
      (spanOf template, This $ hsep ["This template has", natural expected, "elements"])
    ]
    [hint]
  where
    mainError =
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
    hint =
      if expected > actual
        then hsep ["Try adding", natural (expected - actual), "more members to this layer"]
        else hsep ["Try removing", natural (actual - expected), "members from this layer"]
prettyPrintCheckError err (C.NotCallable func arg tyFunc tyArg) =
  err
    (Just "NotCallable")
    "Expression cannot be called"
    [ (spanOf arg, argError),
      (spanOf func, funcError)
    ]
    [hint]
  where
    hint = "Try removing the argument from this expression"
    funcError = This $ hsep ["but expressions of type", prettyPrintType tyFunc, "are not callable"]
    argError = Where $ hsep ["You've tried calling an expression with an argument of type", prettyPrintType tyArg]
prettyPrintCheckError err (C.WrongArgument func arg expected actual contradictions) =
  err
    (Just "WrongArgument")
    "Argument does not have the expected type."
    [ (spanOf arg, argError),
      (spanOf func, funcError)
    ]
    []
  where
    funcError = This $ hsep ["...but this function expects arguments of type", prettyPrintType expected, "instead"]
    argError = This $ hsep ["This expression has type", prettyPrintType actual]
-- prettyPrintCheckError err e = err Nothing "not implemented" [] [pretty (show e :: Text)]
prettyPrintCheckError err (C.WrongType expected actual expr contradictions reason) =
  err
    (Just "WrongType")
    "Cannot match types"
    messages
    []
  where
    -- sep
    --   [ basic,
    --     "I got stuck on the following contradictions:",
    --     indent $ align $ vsep $ showContradiction <$> contradictions
    --   ]
    -- showContradiction (left, right) =
    --   hsep
    --     [ prettyPrintType left,
    --       punctuation "<:",
    --       prettyPrintType right
    --     ]

    messages = case reason of
      C.MustSatisfyChord _ _ -> mustSatisfyList "chord"
      C.MustSatisfySequence _ _ -> mustSatisfyList "sequence"
      C.StaticLayerMember _ -> mustSatisfyList "static layer"
      C.ComputeLayerMember _ -> mustSatisfyList "compute layer"
      C.AnnotationSaidSo _ expr ty ->
        [ (spanOf expr, This $ hsep ["...but this expression has type", prettyPrintType actual, "instead"]),
          (spanOf ty, This "I was expecting this type")
        ]
      C.StaticLayersRequireTemplate _ _ ->
        pure . (spanOf expr,) . This $
          sep
            [ "Only values of type",
              indent $ prettyPrintType expected,
              "can be used as a static layer template! This variable has type",
              indent $ prettyPrintType actual,
              "instead."
            ]

    mustSatisfyList :: Text -> [DiagnosticAnnotation]
    mustSatisfyList kind =
      pure . (spanOf expr,) . This $
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

instance Pretty MyDoc where
  pretty = unAnnotate

printErrors :: (Diagnostic MyDoc -> IO ()) -> [C.CheckError] -> IO ()
printErrors print =
  print
    -- renderStrict . showSpan
    -- . fmap (reAnnotate resolveAnnotation)
    . foldr (flip addReport) def
    . fmap \(C.MkCheckError generalLocation details) ->
      let generalLocationMarker Nothing = []
          generalLocationMarker (Just (Spanned span location)) = [(span, Where message)]
            where
              message = case location of
                C.WhileCheckingAlias _ -> "while checking this alias"
                C.WhileCheckingStaticLayer _ -> "while checking this static layer"
                C.WhileCheckingComputeLayer _ -> "while checking this compute layer"
                C.WhileCheckingLayerTemplate _ -> "while checking this layer template"

          extraMarkers = generalLocationMarker generalLocation
          myErr code message markers = err code message (markers <> extraMarkers)
       in prettyPrintCheckError myErr details
