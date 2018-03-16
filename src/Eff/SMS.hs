{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Eff.SMS where

import Control.Monad.Freer (Member, Eff, send, runNat)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, splitOn, pack)
import System.Environment (getEnv)
import Twilio hiding (runTwilio)
import Twilio.Messages

-- Get Environment Variables

fetchSid :: IO String
fetchSid = (getEnv "TWILIO_ACCOUNT_SID")

fetchToken :: IO String
fetchToken = (getEnv "TWILIO_AUTH_TOKEN")

fetchTwilioNumber :: IO Text
fetchTwilioNumber = pack <$> getEnv "TWILIO_PHONE_NUMBER"

-- SMS Command

data SMSCommand = SubscribeCommand Text

messageToCommand :: Text -> Maybe SMSCommand
messageToCommand messageBody = case splitOn " " messageBody of
  ["subscribe", email] -> Just $ SubscribeCommand email
  _ -> Nothing

-- Eff

data SMS a where
  SendText :: Text -> Text -> SMS ()

sendText :: (Member SMS r) => Text -> Text -> Eff r ()
sendText toNum msg = send (SendText toNum msg)

runTwilio :: (Member IO r) => Eff (SMS ': r) a -> Eff r a
runTwilio = runNat smsToTwilio
  where
    smsToTwilio :: SMS a -> IO a
    smsToTwilio (SendText toNum msg) = runTwilio' fetchSid fetchToken $ do
      fromNum <- liftIO fetchTwilioNumber
      _ <- post (PostMessage toNum fromNum msg)
      return ()
