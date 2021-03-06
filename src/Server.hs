{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
    ( runServer
    ) where

import Control.Monad.Freer (Eff, Member, runM, runNat)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment (getEnv)
import Twilio hiding (runTwilio)
import Twilio.Messages
import Web.FormUrlEncoded (FromForm(..), Form(..))

import Eff.Email
import Eff.SMS
import Eff.SubscriberList
import Schema

-- Sending a Basic Message

sendMessage :: IO ()
sendMessage = runTwilio' fetchSid fetchToken $ do
  let msg = PostMessage "+15551231234" "+15559879876" "Hello Twilio!"
  _ <- post msg
  return ()

-- Message Data Type

data IncomingMessage = IncomingMessage
  { fromNumber :: Text
  , body :: Text
  }
  deriving (Show)

instance FromForm IncomingMessage where
  fromForm (Form form) = case lookupResults of
    Just ((fromNumber : _), (body : _)) -> Right $ IncomingMessage fromNumber body
    Just _ -> Left "Found the keys but no values"
    _ -> Left "Didn't find keys" 
    where
      lookupResults = do
        fromNumber <- HashMap.lookup "From" form
        body <- HashMap.lookup "Body" form
        return (fromNumber, body)

-- Server

type TwilioServerAPI = "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] ()

pingHandler :: Eff r String
pingHandler = return "Pong"

smsHandler :: (Member SMS r, Member Email r) => IncomingMessage -> Eff r ()
smsHandler msg = 
  case messageToCommand (body msg) of
    Nothing -> sendText (fromNumber msg) "Sorry, we didn't understand that request!"
    Just (SubscribeCommand email) -> do
      _ <- sendSubscribeEmail email
      return ()

subscribeHandler :: (Member SubscriberList r) => Text -> Eff r ()
subscribeHandler email = do
  listId <- fetchListId 
  case listId of
    Left _ -> error "Failed to find list ID!"
    Right listId' -> do
      _ <- subscribeUser listId' (Subscriber email)
      return ()

emailList :: (Member SubscriberList r, Member Email r) => (Text, ByteString, Maybe ByteString) -> Eff r ()
emailList content = do
  listId <- fetchListId 
  case listId of
    Left _ -> error "Failed to find list ID!"
    Right listId' -> do
      subscribers <- fetchListMembers listId'
      case subscribers of
        Left _ -> error "Failed to find subscribers!"
        Right subscribers' -> do
          _ <- sendEmailToList content (subscriberEmail <$> subscribers')
          return ()

transformToHandler :: (Eff '[SubscriberList, Email, SMS, IO]) :~> Handler
transformToHandler = NT $ \action -> do
  let ioAct = runM $ runTwilio (runEmail (runSubscriberList action))
  liftIO ioAct

twilioAPI :: Proxy TwilioServerAPI
twilioAPI = Proxy :: Proxy TwilioServerAPI

twilioServer :: Server TwilioServerAPI
twilioServer = enter transformToHandler (pingHandler :<|> smsHandler :<|> subscribeHandler)

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve twilioAPI twilioServer)
