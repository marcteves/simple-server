{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef (readIORef, writeIORef, modifyIORef, newIORef, IORef)
import Data.Maybe (fromJust)
import Data.List
import Data.Aeson
import System.Environment
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- These data types indicate the requests the web app can process
data AppRequest = Send T.Text T.Text
                | Receive T.Text
                | Hello
                | Invalid

type Mail = (T.Text, T.Text)
type Mailbox = [Mail]

-- Augmented Application type, adding an IORef for tracking internal state
app :: IORef Mailbox -> Application
app mailbox request respond = do
    response <- appResponse mailbox . appRequest $ request
    putStrLn . show . queryString $ request
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
    modifyIORef mailbox $ (:) (addr, message)
    mails <- readIORef mailbox
    return $ craftResponse mails

appResponse mailbox (Receive addr) = do
    (sent, rest) <- ( partition ((==) addr . fst) ) `liftM` readIORef mailbox
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
    args <- getArgs
    mailbox <- newIORef [] :: IO (IORef Mailbox)
    run (read . head $ args :: Int) $ app mailbox
