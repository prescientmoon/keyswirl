module MyLib (someFunc) where

import qualified Data.Text as T
import HKF.Parser (parseConfig)
import Text.Megaparsec (MonadParsec (eof), parseTest)

someFunc :: IO ()
someFunc = do
  contents <- T.pack <$> readFile "./examples/test.bkf"
  parseTest (parseConfig <* eof) contents
