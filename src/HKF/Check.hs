{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module HKF.Check where

import Data.Bifunctor
import qualified Data.List as L
import Data.Validation
import GHC.Num (intToNatural)
import qualified HKF.Ast as A
import Relude.Extra (StaticMap (member))
import qualified Relude.String.Reexport as T

-- No type variables to worry about. Hooray!
data DeclarationType
  = Broken -- eg: we can't infer a type because the underlying declaration errors out somewhere else
  -- Can be either:
  --   Chord -> Chord
  --   Key -> Key
  --   KeySequence -> KeySequence
  -- depending on context.
  -- When applied to a seuqnece, the layer maps each element in that sequence
  | Layer
  | Keycode -- in a way, this is a chord with one element
  | Chord -- in a way, this is a sequence with one element
  | Sequence -- [Chord], more or less
  | Arrow DeclarationType DeclarationType

newtype Context = MkContext
  { scope :: [(T.Text, A.ToplevelDeclaration)]
  }

data StaticLayerError
  = NotATemplate T.Text A.ToplevelDeclaration
  | CannotPickTemplate T.Text [A.ToplevelDeclaration]
  | WrongTemplateLength T.Text Natural Natural
  | NoTemplateInScope T.Text

data TemplateCheckError
  = DuplicateTemplateKeycode T.Text
  | InvalidTemplateKeycode Natural T.Text

data CheckError
  = SameName T.Text
  | TemplateCheckError Natural TemplateCheckError
  | StaticLayerError Natural StaticLayerError

type CheckM a = Context -> Validation [CheckError] a

---------- Helpers
lookupScope :: T.Text -> Context -> [A.ToplevelDeclaration]
lookupScope name ctx = [snd v | v <- scope ctx, fst v == name]

lookupTemplate :: T.Text -> Context -> Validation [StaticLayerError] A.LayerTemplate
lookupTemplate name ctx = case lookupScope name ctx of
  [] -> Failure [NoTemplateInScope name]
  [A.LayerTemplate t] -> pure t
  [somethingElse] -> Failure [NotATemplate name somethingElse]
  multiple -> Failure [CannotPickTemplate name multiple]

validKeyCode :: T.Text -> Bool
validKeyCode "wrong" = False
validKeyCode _ = True

---------- Validators
-- Validates a list and errors out on every duplicate found
checkForDuplicates :: Eq a => (a -> e) -> [a] -> Validation [e] ()
checkForDuplicates mkE arr = case duplicates of
  [] -> Success ()
  e -> Failure $ mkE <$> e
  where
    duplicates = L.nub (arr `sub` L.nub arr)
    sub x y = [a | a <- x, a `notElem` y]

-- Runs all the checks on a given template
checkLayerTemplate ::
  A.LayerTemplate ->
  Context ->
  Validation [TemplateCheckError] ()
checkLayerTemplate template ctx = noDuplicates *> keycodeCheck
  where
    keycodeCheck = for_
      (zip [0 ..] $ A.templateKeycodes template)
      \(i, keycode) ->
        if validKeyCode keycode
          then Success ()
          else Failure [InvalidTemplateKeycode i keycode]

    noDuplicates =
      checkForDuplicates
        DuplicateTemplateKeycode
        (A.templateKeycodes template)

checkStaticLayer ::
  A.StaticLayer ->
  Context ->
  Validation [StaticLayerError] ()
checkStaticLayer layer ctx = lengthCheck
  where
    lengthCheck = case lookupTemplate templateName ctx of
      Failure e -> Failure e
      Success template ->
        if templateLength == layerLength
          then Success ()
          else
            Failure
              [ WrongTemplateLength
                  templateName
                  (intToNatural templateLength)
                  (intToNatural layerLength)
              ]
        where
          templateLength = length (A.templateKeycodes template)
          layerLength = length (A.staticLayerContents layer)
      where
        templateName = A.staticLayerTemplate layer

checkDeclarations ::
  Context ->
  Validation [CheckError] Context
checkDeclarations ctx =
  ctx <$ traverse (uncurry mainCheck) indexedDeclarations
  where
    indexedDeclarations :: [(Natural, (T.Text, A.ToplevelDeclaration))]
    indexedDeclarations = zip [0 ..] (scope ctx)

    mainCheck ::
      Natural ->
      (T.Text, A.ToplevelDeclaration) ->
      Validation [CheckError] ()
    mainCheck i (name, declaration) =
      case declaration of
        A.LayerTemplate template ->
          first (map $ TemplateCheckError i) $
            checkLayerTemplate template ctx
        A.Layer (A.StaticLayer layer) ->
          first (map $ StaticLayerError i) $
            checkStaticLayer layer ctx
        _ -> pure ()
