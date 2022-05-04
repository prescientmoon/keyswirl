-- Not the best module name.
-- This module takes care of the actual following of the imports in each file
module HKF.Crawl where

import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (hints), errorDiagnosticFromBundle)
import HKF.Ast (CompleteConfig, Module, Spanned (..), unspan)
import qualified HKF.Ast as A
import HKF.Check hiding (Module)
import HKF.Error (MyDoc, printErrors)
import HKF.Parser (Parser, ParsingContext (MkParsingContext), parseConfigModule)
import Prettyprinter (Pretty (pretty))
import Relude.Monad.Reexport
import Text.Megaparsec (MonadParsec (eof), runParserT)

instance HasHints Void MyDoc where
  hints = absurd

data CrawlState = MkCrawlState
  { config :: [(A.ModuleName, A.Module)],
    diagnostics :: Diagnostic MyDoc
  }

type Crawl = StateT CrawlState IO

parser :: Parser Module
parser = parseConfigModule <* eof

addDiagnostic :: Diagnostic MyDoc -> Crawl ()
addDiagnostic diagnostic = modify \ctx ->
  ctx {diagnostics = diagnostics ctx <> diagnostic}

loadFile :: A.ModuleName -> Text -> Crawl ()
loadFile entry file = modify \ctx ->
  ctx
    { diagnostics = addFile (diagnostics ctx) (T.unpack entry) (T.unpack file)
    }

addModule :: A.ModuleName -> Module -> Crawl ()
addModule path module_ = modify \ctx ->
  ctx
    { config = (path, module_) : config ctx
    }

prefix = "./examples/"

buildFullConfig :: A.ModuleName -> Crawl ()
buildFullConfig entry = do
  let filename = prefix <> T.intercalate "/" (T.split (== '.') entry) <> ".bkf"
  file <- liftIO $ T.readFile $ T.unpack filename
  loadFile entry file
  case flip runReader (MkParsingContext True) $ runParserT parser (T.unpack entry) file of
    Left bundle -> do
      let report :: Diagnostic MyDoc
          report = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
      addDiagnostic report
    Right _module@(A.MkModule (A.MkHeader isUnsafe exports imports) inner) -> do
      addModule entry _module
      for_ imports \(Spanned _ import_) -> do
        buildFullConfig (unspan $ A.importPath import_)

initialCrawlState :: CrawlState
initialCrawlState = MkCrawlState [] def

emptyContext :: Context
emptyContext = MkContext {moduleScopes = mempty, importedModules = mempty, currentScope = MkScope mempty, generalLocation = Nothing}

runChecker :: FilePath -> IO ()
runChecker entry = do
  MkCrawlState config diagnostics <- execStateT (buildFullConfig $ T.pack entry) initialCrawlState
  let checked = foldlM (\ctx (name, module_) -> checkModule name module_ ctx) emptyContext config
  let (ctx, errors) = runWriter checked
  printErrors (\d -> printDiagnostic stderr True True 4 (diagnostics <> d)) errors
