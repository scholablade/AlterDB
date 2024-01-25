{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.MeiliSearch where

{-In this file we build our own handler function to avoid having module cycle issues -}
-- import           Application                          (handler)
import           Control.Lens
import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as B
import qualified Data.Map.Lazy                        as Map
import           Data.Text                            as T
import           Data.Text.Encoding
import           Data.Text.Encoding                   as Encode
import           Database.Persist.Sqlite              (createSqlitePool,
                                                       runSqlPool, sqlDatabase,
                                                       sqlPoolSize)
import           Dhall
import           Handler.JSON                         (getJSONR)
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.HTTP
import           Network.HTTP.Client.OpenSSL
import           Network.HTTP.Client.TLS              (getGlobalManager)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           Network.Wreq
import           OpenSSL.Session                      (context)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)

sendJSONToMeiliSearch :: Handler (Network.Wreq.Response B.ByteString)
sendJSONToMeiliSearch = do
  x <- liftIO $ input auto "./config/env.dhall" :: Handler (Map Text Text)
  let meilisearchKey = Encode.encodeUtf8 $ x Map.! "meilisearchKey"
  txt <- (getUrlRender :: Handler (Route App -> Text))
  let homeRoute = txt HomeR
  let searchRoute = remove' $ T.unpack homeRoute <> "7700"
  let opts = manager .~ Left (opensslManagerSettings context) $ header hAuthorization .~ ["Bearer " <>  meilisearchKey] $ defaults & header hContentType .~ ["application/json"]
  json <- getJSONR
  liftIO $ withOpenSSL $ postWith opts (searchRoute `mappend` "/indexes/software/documents?primaryKey=id") json

remove' :: String -> String
remove' []                           = []
remove' ('3' : _ : _ : _ : '/' : xs) = remove' xs
remove' ('4' : '4' : '3' : '/' : xs) = remove' xs
remove' ('8' : '0' : '/' : xs)       = remove' xs
remove' ('o' : 'r' : 'g' : '/' : xs) = 'o' : 'r' : 'g' : ':' : remove' xs -- Production
remove' (x : xs)                     = x : remove' xs

