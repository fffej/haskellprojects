{-# LANGUAGE OverloadedStrings #-}

http://hypernephelist.com/2014/09/16/sending-data-to-azure-event-hubs-from-nodejs.html

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

data EventHub = EventHub
                {
                  namespace :: ByteString
                , hubname :: ByteString
                , devicename :: ByteString
                } deriving (Show,Eq)

data AccessKey = AccessKey
                 {
                   keyName :: ByteString
                 , key :: ByteString
                 } deriving (Show,Eq)

type Token = ByteString

createSASToken :: URI -> AccessKey -> IO Token
createSASToken uri accessKey = do
  expiry <- round `fmap` getPOSIXTime
  let signingString = B.pack $ (uriToString id uri) "\n" ++ show expiry
      signature = hmacSha1 (key accessKey) signingString
      
  return (B.pack $ show signature)

main :: IO ()
main = do
  return ()
