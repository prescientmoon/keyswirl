{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module MyLib (someFunc) where

import qualified Data.Text as T
import HKF.Ast
import HKF.Check
import HKF.Parser (parseConfig)
import qualified Relude.Lifted as T
import Text.Megaparsec (MonadParsec (eof), parseMaybe, parseTest)

someFunc :: IO ()
someFunc = do
  contents <- T.pack <$> readFile "./examples/test.bkf"
  let parser = parseConfig <* eof
  parseTest parser contents
  case parseMaybe parser contents of
    Nothing -> pure ()
    Just (MkConfig config) ->
      T.putStr $
        show $
          checkDeclarations $
            MkContext
              { scope = flip mapMaybe config \case
                  Spanned _ (NamedConfigEntry name d) ->
                    Just (name, d)
                  _ -> Nothing,
                types = mempty
              }
