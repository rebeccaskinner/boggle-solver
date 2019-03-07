{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE BangPatterns    #-}
module Server
    ( startApp
    , app
    ) where

import App

import Data.Aeson
import Data.Aeson.TH
import System.Environment (getArgs)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Media.MediaType
import Servant
import Boggle
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Fail
import Data.Maybe
import GHC.Generics
import Text.Printf
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map.Strict as Map

type API = "boggle" :> ReqBody '[JSON] BoggleRequest :> Get '[JSON] [String]
           :<|> Get '[HTML] String

type AppServer a = ServerT a (AppT IO)

data BoggleRequest = BoggleRequest
  { reqSize :: Int
  , reqData :: String
  } deriving (Eq, Show, Generic)

instance ToJSON BoggleRequest
instance FromJSON BoggleRequest

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML String where
  mimeRender _ a = BC.pack a

appToHandler :: Cfg -> AppT IO a -> Handler a
appToHandler c app = liftIO $ runApp c app

startApp :: IO ()
startApp = do
  cfg <- getCfg
  run 8080 (app cfg)

app :: Cfg -> Application
app cfg = serve api (server cfg)

api :: Proxy API
api = Proxy

server :: Cfg -> Server API
server cfg = hoistServer api (appToHandler cfg) appTserver

appTserver :: AppServer API
appTserver = playBoggle :<|> showRoot
  where

playBoggle :: BoggleRequest -> AppT IO [String]
playBoggle (BoggleRequest boardSize boardData) =
   case mkTour boardSize boardData of
     Nothing -> throwError $ GeneralException "failed to generate board graph, are the dimensions incorrect?"
     Just tourTrie -> do
       wordTrie <- _cfgWordTrie <$> ask
       let results = solveBoggle wordTrie tourTrie
       return results

showRoot :: AppT IO String
showRoot =  (BC.unpack . _configUIData) <$> ask
