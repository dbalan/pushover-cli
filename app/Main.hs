{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Pushover
import Network.Pushover.Request
import Network.Pushover.Token
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment
import Data.Maybe()
import UnliftIO.Exception
import Options.Applicative

data CLI = CLI
  { cliMessage :: Text
  , cliTitle :: Text
  , cliDevices :: [Text]
  } deriving (Show, Eq)

cli :: Parser CLI
cli = CLI
  <$> strOption
      (  long "message"
      <> metavar "TARGET"
      <> help "Message string, takes in html" )
  <*> strOption
       (  long "title"
       <> metavar "TARGET"
       <> showDefault
       <> value "pushover-cli"
       <> help "Title of the message")
  <*> many (strOption
            (long "device"
            <> metavar "TARGET"
            <> help "devices to send to"))

main :: IO ()
main = do
  let opts = info (cli <**> helper)
               ( fullDesc
               <> progDesc ("Send message over pushover. The API Token and User Key should be passed "
                           <> "as envrionment variables PS_API_TOKEN and PS_USER_KEY")
               <> header "Pushover CLI Client" )

  c <- execParser opts
  at <- readAPIKey "PS_API_TOKEN"
  uk <- readAPIKey "PS_USER_KEY"

  pSendMessage at uk c

pSendMessage :: Text -> Text -> CLI -> IO ()
pSendMessage api user opts = do
  r <- sendRequest req
  putStrLn $ show $ status r
  where
    req = Request
      { requestToken      = makeTokenOrError api
      , requestUserKey    = makeTokenOrError user
      , requestMessage    = text $ cliMessage opts
      , devices           = cliDevices opts
      , title             = Just (cliTitle opts)
      , url               = Nothing
      , priority          = Nothing
      , timestamp         = Nothing
      , notificationSound = Nothing
      }

readAPIKey :: String -> IO Text
readAPIKey name = do
  val <- lookupEnv name
  case val of
    Just v -> pure $ T.pack v
    Nothing -> throwString ("env not set: " <> name)
