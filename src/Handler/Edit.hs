{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Edit where

import qualified Data.Text            as Text
import           Database.Esqueleto   ((^.))
import qualified Database.Esqueleto   as E
import           Database.Persist.Sql
import           Import
getEditR :: SoftwareId -> Handler Html
getEditR key = do
  soft <- runDB $ get404 key
  (widget, enctype) <- generateFormPost $ editForm soft
  defaultLayout [whamlet|
                <form method=post action=@{EditR key} enctype=#{enctype}>
                 ^{widget}
                 &nbsp
                 <button>Submit
                        |]

editForm :: Software -> Form SoftwareForm
editForm soft =
  renderBootstrap2 $ MakeSoftwareForm
    <$> areq textField "Name" (pure $ softwareTitle soft)
    <*> areq textareaField "Description of Software" (pure $ softwareDescription soft)
    <*> fileAFormOpt "Image file"
  where

data SoftwareForm = MakeSoftwareForm
  {
     softwareName            :: Text
  ,  formSoftwareDescription :: Textarea
  ,  fileInfo                :: Maybe FileInfo
    }
postEditR :: SoftwareId -> Handler Html
postEditR key = do
 soft <- runDB $ get404 key
 ((result, widget), enctype) <- runFormPost $ editForm soft
 case result of
   FormSuccess form -> do
     let (name, description, file) = (softwareName form, formSoftwareDescription form, fileInfo form)
     filename <- writeToServer file
     let newSoft = mkSoftware filename form
     runDB $ replace key $ newSoft
     redirect $ SoftwareR key
   _ -> defaultLayout [whamlet|
                       <form method=post action=@{SoftwareFormR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                       |]
mkSoftware :: String -> SoftwareForm -> Software
mkSoftware filename form = Software {softwareTitle = softwareName form, softwareDescription = formSoftwareDescription form, softwareFilename = Text.pack filename}

writeToServer :: Maybe FileInfo -> Handler FilePath
writeToServer (Just file) = do
    let filename = Text.unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename
writeToServer Nothing = return ""

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static"

