{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Base64
import Data.Maybe (fromJust, fromMaybe)
import Data.List (genericLength)

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client

data AccessKey = AccessKey
                 {
                   keyName :: ByteString
                 , key :: ByteString
                 } deriving (Show,Eq)

type Token = ByteString

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
sign key signingString = encode $ LB.toStrict $ bytestringDigest dig
  where
    strictKey = LB.fromStrict key
    strictString = LB.fromStrict signingString
    dig = hmacSha256 strictKey strictString

escape :: ByteString -> ByteString
escape = B.pack . escapeURIString isUnreserved . B.unpack

buildUri :: ByteString -> ByteString -> Integer -> ByteString -> ByteString
buildUri uri signature expiry keyName = B.concat [
        "SharedAccessSignature sr=",
        escape url,
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

makeRequest :: AccessKey -> IO ()
makeRequest key = do
  --token <- createSASToken (fromJust $ parseURI $ B.unpack url) key

  c <- openConnection "requestb.in" 80

  let contentType = "application/atom+xml;type=entry;charset=utf-8"
      q = buildRequest1 $ do
        http POST "/rn25fprn"
        setAccept "application/json"
        setContentType contentType
        setContentLength 0
        setHeader "Authorization" "CUNT" -- token

  sendRequest c q emptyBody

  receiveResponse c (\p i -> do
                        putStr $ show p
                        x <- Streams.read i
                        B.putStr $ fromMaybe "" x)

  closeConnection c

  return ()
  

url :: ByteString
url = B.pack "requestb.in/rn25fprn" --B.concat ["https://", namespace, ".servicebus.windows.net", "/", hubName, "/publishers/", deviceName, "/messages"]

main :: IO ()
main = do
  --let ops = defaults & manager .~ Left (tlsManagerSettings context)
  --withtls $
  --x <-  makeRequest ops (AccessKey "" "") "[1,2,3]"
  return ()
