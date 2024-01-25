
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.JSON where
-- import           Application          (handler)
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T
import           Data.Text.Encoding
import           Import
import           Network.HTTP

import           Network.Wreq

getJSONR :: Handler Value
getJSONR = do
    softs <- runDB $ selectList [SoftwareTitle !=. ""] []
    let returned = toJSON softs
    return returned

