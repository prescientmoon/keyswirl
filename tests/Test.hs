module Main where

import qualified Data.Text as T
import Error.Diagnose (prettyDiagnostic)
import Error.Diagnose.Diagnostic (printDiagnostic)
import MyLib (runChecker)
import Prettyprinter (unAnnotate)
import System.FilePath (replaceExtension)
import System.FilePath.Posix (takeBaseName)
import System.FilePath.Windows (makeRelative)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit

main = tests >>= defaultMain

tests :: IO TestTree
tests = goldenTests

goldenTests :: IO TestTree
goldenTests = do
  examplesAbsolute <- findByExtension [".hkf"] "./tests/examples"
  let examples = map (makeRelative "./tests/examples") examplesAbsolute
  pure $
    testGroup
      "Language output golden tests"
      [ goldenVsString (takeBaseName hkfFile) outFile printOutput
        | hkfFile <- examples,
          let outFile = "./tests/examples/" <> replaceExtension hkfFile ".out",
          let printOutput = do
                err <- runChecker (T.unpack $ T.intercalate "." (T.split (== '/') (T.dropEnd 4 $ T.pack hkfFile)))
                let doc = prettyDiagnostic False 2 err
                pure (show $ unAnnotate doc)
      ]
