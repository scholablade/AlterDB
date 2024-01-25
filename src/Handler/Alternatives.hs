{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Alternatives where

import qualified Data.Text as Text
import           Import

getAlternativesR :: SoftwareId -> Handler Html
getAlternativesR xs = do
 (widget, enctype) <- generateFormPost alternativeAForm
 defaultLayout [whamlet|
             <form method=post action=@{AlternativesR xs} enctype=#{enctype}>
                  ^{widget}
                 <p>It also doesn't include the submit button.
                 <button>Submit|]
-----------
-- FORMS --
-----------
{--
docs:
https://www.yesodweb.com/book/forms
https://hackage.haskell.org/package/yesod-form-1.7.4/docs/Yesod-Form-Types.html
https://hackage.haskell.org/package/yesod-form-1.7.4/docs/Yesod-Form-Functions.html
--}

-- Applicative form
alternativeAForm :: Form Alternative_forms
alternativeAForm = renderBootstrap2 $ Alternative_forms
             <$> areq textField# "Alternative" Nothing
          where
            textField# = checkM validate $ textField

postAlternativesR :: SoftwareId -> Handler Html
postAlternativesR key = do
  ((result, widget), enctype) <- runFormPost alternativeAForm
  dt <- runDB $ get404 key
  let title = softwareTitle dt
  case result of
        FormSuccess post -> do
          let alternative = alternative_formsAlternative post
          let (goal1, goal2) = (Alternatives title alternative, Alternatives alternative title)
          _ <- runDB $ insert goal1
          _ <- runDB $ insert goal2
          redirect $ SoftwareR key
        _ -> defaultLayout
            [whamlet|
                <!-- <p>Invalid input, let's try again. -->
                <form method=post action=@{AlternativesR key} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

errorMessage :: Text
errorMessage  = "Too small of text, try again!"

validate :: Text -> Handler (Either Text Text)
validate xs = do
   t <- runDB (exists [SoftwareTitle ==. xs])
   if t then return $ Right xs else return (Left "The alternative is not in the database")
