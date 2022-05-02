{-# LANGUAGE MultiParamTypeClasses #-}

module MyLib (someFunc) where

import Control.Monad.Writer (execWriter, runWriter)
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Error.Diagnose.Diagnostic (printDiagnostic)
import HKF.Ast
import HKF.Check
import HKF.Error (printErrors)
import HKF.Parser (ParsingContext (MkParsingContext), parseConfig)
import Text.Megaparsec (MonadParsec (eof), parseMaybe, parseTest, runParser, runParserT)

instance HasHints Void Text where
  hints = absurd

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let path = case args of
        h : _ -> h
        [] -> "./examples/test.bkf"
  contents <- T.pack <$> readFile path
  let parser = parseConfig <* eof
  let withFile report = addFile report path (T.unpack contents)
  case flip runReader (MkParsingContext True) $ runParserT parser path contents of
    Left bundle -> do
      let report :: Diagnostic Text
          report = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
      printDiagnostic stderr True True 4 $ withFile report
    Right (MkConfig config) ->
      let declarations = flip mapMaybe config $ traverse \case
            NamedConfigEntry name d ->
              Just (name, d)
            _ -> Nothing
          (ctx, errors) =
            runWriter $
              checkConfig declarations $
                MkContext
                  { scope = mempty,
                    types = mempty,
                    nameSpans = mempty,
                    generalLocation = Nothing
                  }
       in do
            printErrors (printDiagnostic stderr True True 4 . withFile) errors

-- T.putStrLn "========== Context:"
-- for_ (H.toList $ types ctx) \(k, v) ->
--   T.putStr $ k <> ": " <> show v <> "\n"
