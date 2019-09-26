{-# LANGUAGE OverloadedStrings #-}
import SimpleServer.Types

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef (readIORef, writeIORef, modifyIORef, newIORef, IORef)
import Data.Maybe (fromJust)
import Data.List
import Data.Aeson
import System.Environment
import Control.Monad
import qualified Data.Text as T (toUpper)
import qualified Data.Text.Encoding as TE (decodeUtf8)

-- Augmented Application type, adding an IORef for tracking internal state
app :: IORef Mailbox -> Application
app mailbox request respond = do
    let requestClass = appRequest request
    response <- appResponse mailbox requestClass
    case requestClass of
        Send _ _ -> putStrLn . show $ request
        _ -> return ()
    respond response

-- Take an HTTP request and convert it into our app's self-defined types
appRequest :: Request -> AppRequest
appRequest request
    | reqMethod == methodGet = Hello
    | reqMethod == methodPost = Send (qlook "addr") (qlook "message")
    | reqMethod == methodDelete = Receive (qlook "addr")
    | otherwise = Invalid
    where qlook str =
            TE.decodeUtf8 . fromJust . fromJust . lookup str . queryString $ request
          reqMethod = requestMethod request

appResponse :: IORef Mailbox -> AppRequest -> IO Response
appResponse mailbox (Send addr message) = do
    modifyIORef mailbox $ (:) (Mail addr (T.toUpper message))
    mails <- readIORef mailbox
    return $ craftResponse mails

appResponse mailbox (Receive addr) = do
    (sent, rest) <- ( partition ((==) addr . address) ) `liftM` readIORef mailbox
    writeIORef mailbox rest
    return $ craftResponse sent

appResponse _ Hello = return (
    responseLBS status200 [(hContentType, "text/html")] "Hello"
    )
appResponse _ Invalid = return (
    responseLBS status400 [(hContentType, "text/html")] "Bad Request"
    )

craftResponse :: Mailbox -> Response
craftResponse mails =
    responseLBS status200 [(hContentType, "application/json")] jsonmails
    where jsonmails = encode mails

-- main just calls the augmented Application above with an IORef to the
-- internal state and a port number
main :: IO ()
main = do
    port <- (read . head ) `liftM` getArgs :: IO Int
    mailbox <- newIORef [] :: IO (IORef Mailbox)
    run port $ app mailbox
