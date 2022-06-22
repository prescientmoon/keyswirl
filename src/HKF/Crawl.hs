-- Not the best module name.
-- This module takes care of the actual following of the imports in each file
module HKF.Crawl where

import Control.Exception.Base (IOException)
import Control.Monad.Writer (runWriter)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (hints), errorDiagnosticFromBundle)
import GHC.IO (catch)
import HKF.Ast (CompleteConfig, Module, Span, Spanned (..), spanOf, unspan)
import qualified HKF.Ast as A
import HKF.Check hiding (Module)
import HKF.Error (MyDoc, printErrors)
import HKF.Parser (Parser, ParsingContext (MkParsingContext), parseConfigModule)
import Prettyprinter (Pretty (pretty))
import Relude (Either (Right))
import Relude.Monad.Reexport
import Text.Megaparsec (MonadParsec (eof), runParserT)

instance HasHints Void MyDoc where
  hints = absurd

data CrawlState = MkCrawlState
  { config :: [(A.ModuleName, A.Module)],
    diagnostics :: Diagnostic MyDoc
  }

data CrawlContext = MkCrawlContext
  { rootDir :: Text,
    crawlStack :: [A.ModuleName]
  }

type Crawl = ReaderT CrawlContext (StateT CrawlState IO)

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

hasModule :: A.ModuleName -> Crawl Bool
hasModule name = asks crawlStack <&> elem name

extendStack :: A.ModuleName -> Crawl a -> Crawl a
extendStack with = local (\ctx -> ctx {crawlStack = with : crawlStack ctx})

buildFullConfig :: (Maybe Span, A.ModuleName) -> Crawl ()
buildFullConfig (mbSpan, entry) =
  hasModule entry >>= \cycle ->
    if cycle
      then for_ mbSpan \span -> do
        addDiagnostic $ printErrors [MkCheckError Nothing $ CircularImport (Spanned span entry)]
      else extendStack entry do
        prefix <- asks rootDir
        let filename = prefix <> T.intercalate "/" (T.split (== '.') entry) <> ".hkf"
        file <- liftIO $ catch (fmap Just $ T.readFile $ T.unpack filename) (const (pure Nothing) :: IOException -> IO (Maybe Text))
        case file of
          Nothing -> pure ()
          Just file -> do
            loadFile entry file
            case flip runReader (MkParsingContext True) $ runParserT parser (T.unpack entry) file of
              Left bundle -> do
                let report :: Diagnostic MyDoc
                    report = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
                addDiagnostic report
              Right _module@(A.MkModule (A.MkHeader isUnsafe exports imports) inner) -> do
                addModule entry _module
                for_ imports \(Spanned _ import_) -> do
                  buildFullConfig (Just $ spanOf $ A.importPath import_, unspan $ A.importPath import_)

initialCrawlState :: CrawlState
initialCrawlState = MkCrawlState [] def

emptyContext :: Context
emptyContext = MkContext {moduleScopes = mempty, importedModules = mempty, currentScope = MkScope mempty, generalLocation = Nothing}

runChecker :: FilePath -> A.ModuleName -> IO (Diagnostic MyDoc)
runChecker root entry = do
  let context = MkCrawlContext (T.pack root) []
  MkCrawlState config diagnostics <- flip execStateT initialCrawlState $ runReaderT (buildFullConfig (Nothing, entry)) context
  let checked = foldlM (\ctx (name, module_) -> checkModule name module_ ctx) emptyContext config
  let (ctx, errors) = runWriter checked
  let extra = printErrors errors
  pure (diagnostics <> extra)
