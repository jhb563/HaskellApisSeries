{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Subscriber sql=subscribers
    email Text
    UniqueEmail email
    deriving Show Read Eq
|]

instance ToJSON Subscriber where
  toJSON sub = object
    [ "email" .= subscriberEmail sub
    ]

instance FromJSON Subscriber where
  parseJSON = withObject "Subscriber" $ \o -> do
    sEmail <- o .: "email"
    return Subscriber
      { subscriberEmail = sEmail
      }
