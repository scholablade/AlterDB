{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Search where

import           Data.Aeson
import           Data.Ix
import           Data.SearchEngine
import qualified Data.Text          as T
import qualified Database.Esqueleto as E
import           Import
import           NLP.Snowball
import           NLP.Tokenize.Text
softwareSearchEngine :: SearchEngine (Entity Software) (Import.Key Software) (TempFields) NoFeatures
softwareSearchEngine = initSearchEngine config searchRank

config :: SearchConfig (Entity Software) (Import.Key Software) TempFields NoFeatures
config = SearchConfig {
  documentKey = entityKey
  ,   extractDocumentTerms = extractTerms
  ,   transformQueryTerm = transform
  ,   documentFeatureValue = const noFeatures
                      }
         where
           extractTerms :: Entity Software -> TempFields -> [Term]
           extractTerms soft Title       = tokenize $ T.toLower $ softwareTitle  $ entityVal soft
           extractTerms soft Description = tokenize $ T.toLower $ unTextarea $ softwareDescription $ entityVal soft

           transform :: Term -> TempFields -> Term
           transform term _       = T.toLower term

data TempFields = Title | Description deriving (Eq, Ord, Enum, Bounded, Ix, Show)

getSearchR :: Handler Html
getSearchR = do
  ((formRes, searchWidget), _) <- runFormGet searchForm
  searchResults <-
        case formRes of
            FormSuccess qstring ->  getResults qstring
            -- FormFailure _       ->  return
  defaultLayout $ do
        [whamlet|
            <form method=get action=@{SearchR}>
                ^{searchWidget}
                <input type=submit value=Search>
            $if not $ null searchResults
                   <h1>Results
                   $forall (number, title ) <- searchResults
                    <p> <a href=#{"software/" ++ show number}> #{title}</a>
                    <p> #{show searchResults}

        |]
searchRank :: SearchRankParameters TempFields NoFeatures
searchRank =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = noFeatures,
      paramFeatureFunctions   = noFeatures,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400,
      paramAutosuggestPrefilterLimit  = 500,
      paramAutosuggestPostfilterLimit = 500
    }
    where
    paramK1 :: Float
    paramK1 = 1

    paramB :: TempFields -> Float
    paramB Title       = 0.5
    paramB Description = 0.5
    paramFieldWeights :: TempFields -> Float
    paramFieldWeights Title       = 5
    paramFieldWeights Description = 5

getResults :: Text -> Handler _
getResults input = do
  software <- runDB $ E.select $
              E.from $ \software -> do
              return software
  let val = fmap tokenize $ fmap softwareTitle $ fmap entityVal software -- Debug
  let completeSearchEngine = insertDocs software softwareSearchEngine
  print val -- Debug
  let queried = query completeSearchEngine [input]
  let num = fmap E.unSqlBackendKey $ fmap unSoftwareKey queried
  print queried -- Debug
  dt <- runDB $ mapM get404 queried
  let titles = fmap softwareTitle dt
  let returned = zip num titles
  return returned

-- getResults :: Text -> Handler _
-- getResults input = do
  -- software <- runDB $ E.select $
              -- E.from $ \software -> do
              -- return software
  -- let val = fmap tokenize $ fmap softwareTitle $ fmap entityVal software -- Debug
  -- let completeSearchEngine = insertDocs software softwareSearchEngine
  -- print val -- Debug
  -- let queried = query completeSearchEngine [input]
  -- let num = fmap E.unSqlBackendKey $ fmap unSoftwareKey queried
  -- print queried -- Debug
  -- dt <- runDB $ mapM get404 queried
  -- let titles = fmap softwareTitle dt
  -- let returned = zip queried titles
  -- return num
searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) "Query" Nothing
