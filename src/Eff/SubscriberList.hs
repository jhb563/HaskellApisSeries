{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Eff.SubscriberList where

import Control.Monad.Freer (Member, Eff, send, runNat)
import Data.Text (Text, pack)
import System.Environment (getEnv)

import Mailchimp (fetchMCListId, fetchMCListMembers, subscribeMCMember)
import Schema (Subscriber(..))

data SubscriberList a where
  FetchListId :: SubscriberList (Either String Text)
  FetchListMembers :: Text -> SubscriberList (Either String [Subscriber])
  SubscribeUser :: Text -> Subscriber -> SubscriberList (Either String ())

fetchListId :: (Member SubscriberList r) => Eff r (Either String Text)
fetchListId = send FetchListId

fetchListMembers :: (Member SubscriberList r) => Text -> Eff r (Either String [Subscriber])
fetchListMembers listId = send (FetchListMembers listId)

subscribeUser :: (Member SubscriberList r) => Text -> Subscriber -> Eff r (Either String ())
subscribeUser listId subscriber = send (SubscribeUser listId subscriber)

runSubscriberList :: (Member IO r) => Eff (SubscriberList ': r) a -> Eff r a
runSubscriberList = runNat subscriberListToIO
  where
    subscriberListToIO :: SubscriberList a -> IO a
    subscriberListToIO FetchListId = do
      listName <- pack <$> getEnv "MAILCHIMP_LIST_NAME"
      fetchMCListId listName
    subscriberListToIO (FetchListMembers listId) = do
      membersEither <- fetchMCListMembers listId
      case membersEither of
        Left e -> return $ Left e
        Right emails -> return $ Right (Subscriber <$> emails)
    subscriberListToIO (SubscribeUser listId (Subscriber email)) = subscribeMCMember listId email
