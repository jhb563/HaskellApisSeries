{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Eff.Email where

import Control.Monad.Freer (Member, Eff, send, runNat)
import Data.ByteString.Char8 (pack, ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Mail.Hailgun
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv)

data Email a where
  SendSubscribeEmail :: Text -> Email (Either String ())

sendSubscribeEmail :: (Member Email r)  => Text -> Eff r (Either String ())
sendSubscribeEmail email = send (SendSubscribeEmail email)

runEmail :: (Member IO r) => Eff (Email ': r) a -> Eff r a
runEmail = runNat emailToIO
  where
    emailToIO :: Email a -> IO a
    emailToIO (SendSubscribeEmail subscriberEmail) = do
      domain <- getEnv "MAILGUN_DOMAIN"
      apiKey <- getEnv "MAILGUN_API_KEY"
      replyEmail <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
      currentDir <- getCurrentDirectory
      let context = HailgunContext domain apiKey Nothing
      case mkSubscribeMessage replyEmail (encodeUtf8 subscriberEmail) currentDir of
        Left err -> return $ Left err
        Right msg -> do
          result <- sendEmail context msg
          case result of
            Left err -> return $ Left (show err)
            Right resp -> return $ Right ()

mkSubscribeMessage :: ByteString -> ByteString -> FilePath -> Either HailgunErrorMessage HailgunMessage
mkSubscribeMessage replyAddress subscriberAddress currentDir = hailgunMessage
  "Thanks for signing up!"
  content
  replyAddress 
  (emptyMessageRecipients { recipientsTo = [subscriberAddress] })
  [Attachment (rewardFilepath currentDir) (AttachmentBS "Your Reward")]
  where
    content = TextAndHTML 
      textOnly
      ("Here's your reward! To confirm your subscription, click " <> link <> "!")
    textOnly = "Here's your reward! To confirm your subscription, go to "
      <> "https://haskell-apis.herokuapp.com/api/subscribe/" <> subscriberAddress
      <> " and we'll sign you up!"
    link = "<a href=\"https://haskell-apis.herokuapp.com/api/subscribe/" 
      <> subscriberAddress <> "\">this link</a>"

rewardFilepath :: FilePath -> FilePath
rewardFilepath currentDir = currentDir ++ "/attachments/reward.txt"
