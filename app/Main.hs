{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef (readIORef, modifyIORef, newIORef, IORef)
import Data.Maybe (fromJust)
import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map

data AppRequest = Send T.Text
                | Receive T.Text
                | Hello
                | Invalid

app :: IORef (Map.Map T.Text Bool) -> Application
app mailbox request respond = do
    response <- appResponse mailbox . appRequest $ request
    reqBody <- getRequestBodyChunk request
    putStrLn $ "Received request:\n" ++ show reqBody
    respond response

appRequest :: Request -> AppRequest
appRequest request
    | reqMethod == methodGet = Hello
    | reqMethod == methodPost = Send address
    | reqMethod == methodDelete = Receive address
    | otherwise = Invalid
    where address =
            TE.decodeUtf8 . fromJust . fromJust . lookup "addr" . queryString $ request
          reqMethod = requestMethod request

appResponse :: IORef (Map.Map T.Text Bool) -> AppRequest -> IO Response
appResponse mailbox (Send addr) = do
    modifyIORef mailbox $ Map.insert addr True
    mails <- readIORef mailbox
    return $ craftResponse mails

appResponse mailbox (Receive addr) = do
    modifyIORef mailbox $ Map.delete addr
    mails <- readIORef mailbox
    return $ craftResponse mails

appResponse _ Hello = return (
    responseLBS status200 [(hContentType, "text/html")] "Hello"
    )
appResponse _ Invalid = return (
    responseLBS status400 [(hContentType, "text/html")] "Bad Request"
    )

craftResponse :: Map.Map T.Text Bool -> Response
craftResponse mails =
    responseLBS status200 [(hContentType, "application/json")] jsonmails
    where jsonmails = encode . Map.toAscList $ mails

main :: IO ()
main = do
    mailbox <- newIORef Map.empty :: IO (IORef (Map.Map T.Text Bool))
    putStrLn $ "http://localhost:8080/"
    run 8080 $ app mailbox
