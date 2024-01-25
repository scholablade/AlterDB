{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SoftwareForm where

import qualified Data.Text as Text
import           Import
getSoftwareFormR :: Handler Html
getSoftwareFormR = do
 (widget, enctype) <- generateFormPost t

 defaultLayout [whamlet|
     <div .ui.container>
         <center> 
            <form method=post action=@{SoftwareFormR} enctype=#{enctype}>
                ^{widget}
                &nbsp
                <button>Submit
                |]
postSoftwareFormR :: Handler Html
postSoftwareFormR = do
  ((result, widget), enctype) <- runFormPost t
  case result of
        FormSuccess form -> do
          filename <- writeToServer (fileInfo form)
          let soft = Software {
                  softwareTitle = softwareName form
                , softwareDescription = formSoftwareDescription form
                , softwareFilename = Text.pack filename
                              }
          id <- runDB $ insert soft
          redirect $ SoftwareR id
          defaultLayout [whamlet|<p> kf|]
        _ -> defaultLayout
            [whamlet| 
                    <form method=post action=@{SoftwareFormR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit|]

descriptionError :: Textarea
descriptionError  = Textarea "Description should be at least 10 characters"

data SoftwareForm = MakeSoftwareForm
  {
     softwareName            :: Text
  ,  formSoftwareDescription :: Textarea
  ,  fileInfo                :: Maybe FileInfo
    }

t :: Form SoftwareForm
t = renderBootstrap2 $ MakeSoftwareForm
    <$> areq textField "Name" Nothing
    <*> areq g "Description of Software" Nothing
    <*> fileAFormOpt "Image file"
            where
            -- textareaField# = check validateLength $ textareaField
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

validateLength :: Textarea -> Either Textarea Textarea
validateLength y
               |  (< 10) $ Text.length (unTextarea y) = Left descriptionError
               | otherwise = Right y

g :: (Monad m, RenderMessage (HandlerSite m) Textarea,
      RenderMessage (HandlerSite m) FormMessage) =>
     Field m Textarea
g = check validateLength textareaField

instance RenderMessage App Textarea where
  renderMessage _ _ = unTextarea
