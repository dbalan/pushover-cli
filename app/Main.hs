{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Pushover
import Network.Pushover.Request
import Network.Pushover.Token
import qualified Data.Text as T
import Data.Maybe

testRequest = Request
  { requestToken      = makeTokenOrError "foo"
  , requestUserKey    = makeTokenOrError "bar"
  , requestMessage    = text "Backup of database \"example\" finished in 16 minutes."
  , devices           = ["iphone12"]
  , title             = Just "Backup finished - SQL1"
  , url               = Nothing
  , priority          = Nothing
  , timestamp         = Nothing
  , notificationSound = Nothing
  }

main :: IO ()
main = do
  r <- sendRequest testRequest

  putStrLn $ show $ status r
