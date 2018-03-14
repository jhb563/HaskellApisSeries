{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TwilioServer
    ( runServer
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment (getEnv)
import Twilio
import Twilio.Messages
import Web.FormUrlEncoded (FromForm(..), Form(..))

-- Get Environment Variables

fetchSid :: IO String
fetchSid = (getEnv "TWILIO_ACCOUNT_SID")

fetchToken :: IO String
fetchToken = (getEnv "TWILIO_AUTH_TOKEN")

twilioNum :: Text
twilioNum = "+15559879876"

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
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] ()

pingHandler :: Twilio String
pingHandler = return "Pong"

smsHandler :: IncomingMessage -> Twilio ()
smsHandler msg = do
  let newMessage = PostMessage (fromNumber msg) twilioNum (body msg)
  _ <- post newMessage
  return ()

transformToHandler :: Twilio :~> Handler
transformToHandler = NT $ \action -> liftIO $ runTwilio' fetchSid fetchToken action

twilioAPI :: Proxy TwilioServerAPI
twilioAPI = Proxy :: Proxy TwilioServerAPI

twilioServer :: Server TwilioServerAPI
twilioServer = enter transformToHandler (pingHandler :<|> smsHandler)

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve twilioAPI twilioServer)
