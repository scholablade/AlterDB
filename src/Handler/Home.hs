{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Data.Aeson
import           Data.Map             as Map
import           Data.SearchEngine
import           Data.Text            as T
import           Database.Esqueleto   ((^.))
import qualified Database.Esqueleto   as E
import           Database.Persist.Sql
import           Dhall
import           GHC.Generics
-- import           Handler.ClientJSON
-- import           Handler.MeiliSearch  (sendJSONToMeiliSearchf)
import           Handler.MeiliSearch  (sendJSONToMeiliSearch)
import           Handler.Search
import           Import
import           Text.Julius          (RawJS (..))
-- data SearchResult' = SearchResult {entry :: Text} deriving (Show, Generic)
-- inclined, or create a single monolithic file.

getHomeR :: Handler Html
getHomeR = do
    ((_, searchWidget), _) <- runFormGet searchForm
    softs <- runDB $ selectList [SoftwareTitle !=. ""] []
    let jsSofts =  fmap (T.unpack . softwareTitle . entityVal) softs
    txt <- (getUrlRender :: Handler (Route App -> Text))
    {- Hacky solution for production meilisearch -}
    let homeRoute = txt HomeR
    key <- keyToUse
    a <- getHome
    print a
    let searchRoute = remove' $ T.unpack homeRoute <> "7700"
    _ <- sendJSONToMeiliSearch
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
        -- $(widgetFile "homepage")

remove' :: String -> String
remove' []                           = []
remove' ('3' : _ : _ : _ : '/' : xs) = remove' xs
remove' ('4' : '4' : '3' : '/' : xs) = remove' xs
remove' ('o' : 'r' : 'g' : '/' : xs) = 'o' : 'r' : 'g' : ':' : remove' xs -- Production
remove' ('8' : '0' : '/' : xs)       = remove' xs
remove' (x : xs)                     = x : remove' xs
getHome :: Handler Text
getHome = do
  txt <- getUrlRender :: Handler (Route App -> Text)
  let homeRoute = txt HomeR
  return homeRoute
keyToUse :: Handler Text
keyToUse = do
  x <- liftIO $ input auto "./config/env.dhall" :: Handler (Map Text Text)
  let localKey  = x Map.! "localHostMeiliSearch"
  let serverKey = x Map.! "serverMeiliSearch"
  homeRoute <- getHome
  if T.elem 'g' homeRoute then return serverKey else return localKey

