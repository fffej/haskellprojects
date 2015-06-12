{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Base64

data AccessKey = AccessKey
                 {
                   keyName :: ByteString
                 , key :: ByteString
                 } deriving (Show,Eq)

type Token = ByteString

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

main :: IO ()
main = do
  return ()
