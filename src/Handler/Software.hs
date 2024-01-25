{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Software where

import qualified Data.Text            as Text
import           Database.Esqueleto   ((^.))
import qualified Database.Esqueleto   as E
import           Database.Persist.Sql
import           Import
import           Text.Julius          (rawJS)

getSoftwareR :: SoftwareId -> Handler Html
getSoftwareR key = do
 contextData <- runDB $ get404 key
 let (contextTitle, contextDescription, contextFileName) = (softwareTitle contextData, softwareDescription contextData, softwareFilename contextData)
 softwares <- runDB $ E.select $ E.from $ \(software `E.InnerJoin` alternatives) -> do
              E.where_ (software ^. SoftwareTitle E.==. E.val contextTitle)
              E.on $ (software ^. SoftwareTitle  E.==. alternatives ^. AlternativesSame_title)
              return (alternatives ^. AlternativesAlternative)

 let fa = fromSqlKey key
 let f = map E.unValue softwares
 dt <- runDB $ selectList [SoftwareTitle <-. f] []
 let b = fmap (softwareDescription . entityVal) dt
 let filenames = fmap (softwareFilename . entityVal) dt
 let mainImage = imageFilePath $ Text.unpack $ contextFileName
 defaultLayout $ do
   setTitle "a"
   $(widgetFile "software")

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "/static"

