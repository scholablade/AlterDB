{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# LANGUAGE TypeFamilies          #-}
module Handler.ProfileForm where

import qualified Data.Text as Text
import           Import

getProfileFormR ::  Handler Html
getProfileFormR = do
 (widget, enctype) <- generateFormPost postAForm
 (_, user) <- requireAuthPair
 defaultLayout [whamlet|
           <p>
             Your name currently is #{show user}, feel free to change it.
            <form method=post action=@{ProfileFormR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>
                   Submit|]
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
postAForm :: Form Name_forms
postAForm = renderBootstrap $ Name_forms
             <$> areq textField# "Post" Nothing
          where
            textField# = check validateLength textField
            validateLength y
               |  (< 10) $ Text.length y = Left errorMessage
               | otherwise = Right y

postProfileFormR :: Handler Html
postProfileFormR = do
  ((result, widget), enctype) <- runFormPost postAForm
  (authId, user) <- requireAuthPair
  let name = userName user
  -- runDB $ get404 authId
  case result of
    FormSuccess post -> do
      let text = name_formsName post
      runDB $ update authId [UserName =. Just text]
      defaultLayout [whamlet|<p> jalf|]
    FormFailure _ -> do
      defaultLayout [whamlet|<p> afjlaf|]



errorMessage :: Text
errorMessage  = "Too small of text, try again!"
