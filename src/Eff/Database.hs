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
import Database.Persist (insert)
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Postgresql (withPostgresqlConn)
import System.Environment (getEnv)

import Schema (Subscriber(..))

data Database a where
  RegisterUser :: Text -> Database ()

registerUser :: (Member Database r) => Text -> Eff r ()
registerUser email = send (RegisterUser email)

runDatabase :: (Member IO r) => Eff (Database ': r) a -> Eff r a
runDatabase = runNat databaseToSql
  where
    databaseToSql :: Database a -> IO a
    databaseToSql (RegisterUser email) = void $ runPGAction $ insert (Subscriber email)

runPGAction :: SqlPersistT (LoggingT IO) a -> IO a
runPGAction action = do
  pgConn <- pack <$> getEnv "DATABASE_URL"
  runStdoutLoggingT $ withPostgresqlConn pgConn $ \backend ->
    runReaderT action backend
