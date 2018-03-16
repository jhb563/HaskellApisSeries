{-# LANGUAGE OverloadedStrings #-}

module Email where

import Data.ByteString.Char8 (pack, ByteString)
import Mail.Hailgun
import System.Environment

-- Sending Email

data MailgunInfo = MailgunInfo
  { mailgunInfoDomain :: String
  , mailgunInfoAPIKey :: String
  , mailgunInfoReplyEmail :: ByteString
  }

sendMail :: IO ()
sendMail = do
  domain <- getEnv "MAILGUN_DOMAIN"
  apiKey <- getEnv "MAILGUN_API_KEY"
  print domain
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  let context = HailgunContext domain apiKey Nothing
  case mkMessage replyAddress of
    Left err -> putStrLn ("Making failed: " ++ show err)
    Right msg -> do
      result <- sendEmail context msg
      case result of
        Left err -> putStrLn ("Sending failed: " ++ show err)
        Right resp -> putStrLn ("Sending succeeded: " ++ show resp)
  where
    mkMessage replyAddress = hailgunMessage
      "Hello Mailgun!"
      (TextOnly "This is a test message being sent from our mailgun server.")
      replyAddress 
      (emptyMessageRecipients { recipientsTo = ["jhbowen047@gmail.com"] })
      []
