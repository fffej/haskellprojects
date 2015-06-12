{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Base64
import Data.Maybe (fromJust)
import Data.List (genericLength)

import Network.Wreq
import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (nth)
import OpenSSL.Session (context)
import Network.HTTP.Client.TLS

data AccessKey = AccessKey
                 {
                   keyName :: ByteString
                 , key :: ByteString
                 } deriving (Show,Eq)

type Token = ByteString

namespace :: ByteString
namespace = "https://eventhubexample-ns.servicebus.windows.net";

hubName :: ByteString
hubName = "hubname";

deviceName :: ByteString
deviceName = "computer";

encodeURI :: URI -> ByteString
encodeURI x = B.pack $ uriToString id x ""

sign :: ByteString -> ByteString -> ByteString
sign key signingString = encode $ LB.toStrict $ bytestringDigest dig
  where
    strictKey = LB.fromStrict key
    strictString = LB.fromStrict signingString
    dig = hmacSha1 strictString strictString

buildUri :: ByteString -> ByteString -> Integer -> ByteString -> ByteString
buildUri uri signature expiry keyName = B.concat [
        "SharedAccessSignature sr=",
        uri,
        "&sig=",
        signature,
        "&se=",
        B.pack $ show expiry,
        "&skn=",
        keyName
        ]

createSASToken :: URI -> AccessKey -> IO Token
createSASToken uri accessKey = do
  expiry <- round `fmap` getPOSIXTime
  let signingString = B.concat [encodeURI uri, "\n", B.pack $ show expiry]
      signature :: ByteString
      signature = sign (key accessKey) signingString
  return $ buildUri (encodeURI uri) signature expiry (keyName accessKey)

makeRequest :: Options -> AccessKey -> [a] -> IO ByteString
makeRequest ops key payload = do
  let contentType = "application/atom+xml;type=entry;charset=utf-8"
      url = B.concat [namespace, "/", hubName, "/publishers/", deviceName, "/messages"]
  token <- createSASToken (fromJust $ parseURI $ show url) key
  c <- withConnection (openConnection url 443) $ (\c -> do
    let q = buildRequest1 $ do
          http POST ""
          setHeader "Authorization" token
          setContentType contentType
          setContentLength (genericLength payload)

    sendRequest c q (\o -> Streams.write (Just (fromString payload)) o)
    receiveResponse c debugHandler    
    return "blah")
  return c

url :: ByteString
url = B.concat [namespace, "/", hubName, "/publishers/", deviceName, "/messages"]

main :: IO ()
main = do
  let ops = defaults & manager .~ Left (opensslManagerSettings context)
  withOpenSSL $
    makeRequest ops (AccessKey "" "") "[1,2,3]"
  return ()
