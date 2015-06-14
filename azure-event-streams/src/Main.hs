{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Base64 as Base64
import Data.Maybe (fromJust, fromMaybe)
import Data.List (genericLength)

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client
import qualified Blaze.ByteString.Builder.ByteString as Builder

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

data AccessKey = AccessKey
                 {
                   keyName :: ByteString
                 , key :: ByteString
                 } deriving (Show,Eq)

type Token = ByteString


data Sample = Sample
              {
                name :: String,
                value :: Double
              } deriving (Show, Generic)

data Event = Event
              {
                device :: String
              , samples :: [Sample]
              } deriving (Show, Generic)

instance ToJSON Sample
instance ToJSON Event

              

namespace :: ByteString
namespace = "eventhubexample-ns"

hubName :: ByteString
hubName = "eventhubexample"

deviceName :: ByteString
deviceName = "computer"

encodeURI :: URI -> ByteString
encodeURI x = B.pack $ uriToString id x ""

-- This function validates against the JS thing
sign :: ByteString -> ByteString -> ByteString
sign key signingString = Base64.encode $ LB.toStrict $ bytestringDigest dig
  where
    strictKey = LB.fromStrict key
    strictString = LB.fromStrict signingString
    dig = hmacSha256 strictKey strictString

escape :: ByteString -> ByteString
escape = B.pack . escapeURIString isUnreserved . B.unpack

buildUri :: ByteString -> ByteString -> Integer -> ByteString -> ByteString
buildUri uri signature expiry keyName = B.concat [
        "SharedAccessSignature sr=",
        uri,
        "&sig=",
        escape signature,
        "&se=",
        B.pack $ show expiry,
        "&skn=",
        keyName
        ]

createSASToken :: URI -> AccessKey -> IO Token
createSASToken uri accessKey = do
  expiry <- (+ 3600) `fmap` round `fmap` getPOSIXTime
  let name = keyName accessKey
      encodedURI = escape $ B.pack $ show uri
      stringToSign = B.concat [
        encodedURI,
        "\n",
        B.pack $ show expiry
        ]                      
      signature = sign (key accessKey) stringToSign 

  return $ buildUri encodedURI signature expiry name

makeRequest :: ToJSON a => AccessKey -> a -> IO ()
makeRequest key obj = do
  token <- createSASToken url key

  let contentType = "application/atom+xml;type=entry;charset=utf-8"
      messageBody = encode obj
      q = buildRequest1 $ do
        http POST (B.pack $ show url) 
        setAccept "application/json"
        setContentType contentType
        setContentLength (fromIntegral $ LB.length messageBody)
        setHeader "Authorization" token

  c <- withConnection (establishConnection (B.pack $ show url)) $ (\c -> do
      sendRequest c q (\o -> Streams.write (Just (Builder.fromLazyByteString messageBody)) o)
      receiveResponse c debugHandler)

  return ()
  

url :: URI
url = fromJust $ parseURI $ B.unpack $ B.concat ["https://", namespace, ".servicebus.windows.net", "/", hubName, "/publishers/", deviceName, "/messages"]

main :: IO ()
main = do
  return ()
