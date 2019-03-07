module App where

import System.Environment (getArgs)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Fail
import Boggle
import qualified Data.Trie as T
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Char8 as BS

newtype AppT m a = AppT
  { runAppT :: ExceptT AppException (ReaderT Cfg m) a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadReader Cfg, MonadError AppException)

data AppException = GeneralException String deriving (Eq, Show)

data Cfg = Cfg
  { _cfgWordTrie  :: T.Trie ()
  , _configUIData :: BC.ByteString
  }

raiseAppException :: (Monad m, MonadFail m) => Either AppException a -> m a
raiseAppException = \case
  Left err -> Control.Monad.Fail.fail . show $ err
  Right a -> return a

runApp :: (Monad m, MonadFail m) => Cfg -> AppT m a -> m a
runApp cfg =
  (>>= raiseAppException) . flip runReaderT cfg . runExceptT . runAppT

getCfg :: IO Cfg
getCfg = do
  args <- getArgs
  when (length args < 2) $
    ioError $ userError "error: missing filename as command line arg"
  let (wordFilePath:uiFilePath:_) = args
  wordData <- BS.lines <$> BS.readFile wordFilePath
  uiData <- BC.readFile uiFilePath
  return $ Cfg (dictTrie wordData) uiData
