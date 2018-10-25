{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
    ( serverFunc
    ) where

import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup)
import Data.Maybe
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Control.Applicative
import Data.Text (Text)

serverFunc :: IO ()
serverFunc = do
  port <- getPort
  config <- Y.decodeThrow configYaml
  putStr "start Server: http://localhost:"
  print port
  run port (appWithConfig config)

data Config =
    Config {
           shouldBeExists :: [String]
           } deriving(Eq, Show)

instance FromJSON Config where
    parseJSON (Y.Object v) =
      Config <$>
      v .: "shouldBeExists"
    parseJSON _ = fail "Expected Object for Config"

configYaml :: ByteString
configYaml = [r|
shouldBeExists:
  - app.log 
|]

appWithConfig :: Config -> Application
appWithConfig config req resp = do
    let file = head . shouldBeExists $ config
    isBootable <- doesFileExist $ file
    let packed = BC.pack $ file ++ " should be exists:" ++ (show isBootable)
    resp . responseLBS status200 [] $ packed 

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

