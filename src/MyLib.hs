module MyLib (someFunc) where

import Control.Monad.Writer (execWriter, runWriter)
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import HKF.Ast
import HKF.Check
import HKF.Error (errorToText)
import HKF.Parser (parseConfig)
import Text.Megaparsec (MonadParsec (eof), parseMaybe, parseTest)

someFunc :: IO ()
someFunc = do
  contents <- T.pack <$> readFile "./examples/test.bkf"
  let parser = parseConfig <* eof
  parseTest parser contents
  case parseMaybe parser contents of
    Nothing -> pure ()
    Just (MkConfig config) ->
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
            for_ errors (T.putStrLn . errorToText contents splitContent)
            T.putStrLn "========== Context:"
            for_ (H.toList $ types ctx) \(k, v) ->
              T.putStr $ k <> ": " <> show v <> "\n"
