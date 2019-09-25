{-# LANGUAGE DeriveGeneric #-}

module SimpleServer.Types (
      AppRequest(..)
    , Mail(..)
    , Mailbox
    ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

-- These data types indicate the requests the web app can process
data AppRequest = Send T.Text T.Text
                | Receive T.Text
                | Hello
                | Invalid

data Mail = Mail {
      address :: T.Text
    , message :: T.Text
    } deriving (Generic)
type Mailbox = [Mail]

instance ToJSON Mail where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Mail
