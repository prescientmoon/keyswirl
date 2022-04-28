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
import HKF.Parser (parseConfig)
import Text.Megaparsec (MonadParsec (eof), parseMaybe, parseTest, runParser)

path :: [Char]
path = "./examples/test.bkf"

instance HasHints Void Text where
  hints = absurd

someFunc :: IO ()
someFunc = do
  contents <- T.pack <$> readFile path
  let parser = parseConfig <* eof
  let withFile report = addFile report path (T.unpack contents)
  case runParser parser path contents of
    Left bundle -> do
      let report :: Diagnostic Text
          report = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
      printDiagnostic stderr True True 4 $ withFile report
    Right (MkConfig config) ->
      let declarations = flip mapMaybe config \case
            Spanned _ (NamedConfigEntry name d) ->
              Just (name, d)
            _ -> Nothing
          (ctx, errors) =
            runWriter $
              checkConfig declarations $
                MkContext
                  { scope = mempty,
                    types = mempty
                  }
       in do
            let splitContent = lines contents
            T.putStrLn "========== Errors:"
            printErrors (printDiagnostic stderr True True 4 . withFile) errors

-- T.putStrLn "========== Context:"
-- for_ (H.toList $ types ctx) \(k, v) ->
--   T.putStr $ k <> ": " <> show v <> "\n"
