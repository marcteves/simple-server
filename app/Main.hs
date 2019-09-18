{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef (readIORef, writeIORef, modifyIORef, newIORef, IORef)
import Data.Maybe (fromJust)
import Data.List
import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data AppRequest = Send T.Text T.Text
                | Receive T.Text
                | Hello
                | Invalid

app :: IORef [(T.Text, T.Text)] -> Application
app mailbox request respond = do
    response <- appResponse mailbox . appRequest $ request
    putStrLn . show . queryString $ request
    respond response

appRequest :: Request -> AppRequest
appRequest request
    | reqMethod == methodGet = Hello
    | reqMethod == methodPost = Send (qlook "addr") (qlook "message")
    | reqMethod == methodDelete = Receive (qlook "addr")
    | otherwise = Invalid
    where qlook str =
            TE.decodeUtf8 . fromJust . fromJust . lookup str . queryString $ request
          reqMethod = requestMethod request

appResponse :: IORef [(T.Text, T.Text)] -> AppRequest -> IO Response
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

craftResponse :: [(T.Text, T.Text)] -> Response
craftResponse mails =
    responseLBS status200 [(hContentType, "application/json")] jsonmails
    where jsonmails = encode mails

main :: IO ()
main = do
    mailbox <- newIORef [] :: IO (IORef ([(T.Text, T.Text)]))
    putStrLn $ "http://localhost:8080/"
    run 8080 $ app mailbox
