{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Eff.Database where

import Control.Monad (void)
import Control.Monad.Freer (Member, Eff, send, runNat)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Database.Persist (insert, selectList)
import Database.Persist.Sql (SqlPersistT, entityVal)
import Database.Persist.Postgresql (withPostgresqlConn, runMigration)
import System.Environment (getEnv)

import Schema (Subscriber(..), migrateAll)

data Database a where
  RegisterUser :: Text -> Database ()
  RetrieveSubscribers :: Database [Text]

registerUser :: (Member Database r) => Text -> Eff r ()
registerUser email = send (RegisterUser email)

retrieveSubscribers :: (Member Database r) => Eff r [Text]
retrieveSubscribers = send RetrieveSubscribers

runDatabase :: (Member IO r) => Eff (Database ': r) a -> Eff r a
runDatabase = runNat databaseToSql
  where
    databaseToSql :: Database a -> IO a
    databaseToSql (RegisterUser email) = void $ runPGAction $ insert (Subscriber email)
    databaseToSql RetrieveSubscribers = do
      allEntities <- (runPGAction $ selectList [] [])
      return $ (subscriberEmail . entityVal) <$> allEntities

runPGAction :: SqlPersistT (LoggingT IO) a -> IO a
runPGAction action = do
  pgConn <- pack <$> getEnv "DATABASE_URL"
  runStdoutLoggingT $ withPostgresqlConn pgConn $ \backend ->
    runReaderT action backend

migrateDB :: IO ()
migrateDB = runPGAction $ runMigration migrateAll
