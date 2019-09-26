{-# LANGUAGE OverloadedStrings #-}
module Main where

import SimpleServer.Types

import Data.IORef (readIORef, writeIORef, modifyIORef, newIORef, IORef)
import Network.HTTP.Simple
import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Environment
import qualified Data.Text as T
import Data.Aeson

main :: IO ()
main = do
    [addr, url] <- getArgs
    let newURL  = url ++ "?addr=" ++ addr
    let request = setRequestMethod "DELETE" . parseRequest_ $ newURL
    let address = T.pack addr
    state      <- newIORef $ Mail address "default" :: IO (IORef Mail)
    forever $ pollLoop state request

pollLoop state request = do
        response <- (eitherDecode . getResponseBody) `liftM` httpLBS request
        case response of
            Left err ->         putStrLn err
            Right mails
                | null mails -> return ()
                | otherwise  -> writeIORef state (last mails)
        lastMail <- readIORef state
        let print = T.unpack $ "Last message received: " `T.append` (message lastMail)
        putStrLn print
        threadDelay 1000000
