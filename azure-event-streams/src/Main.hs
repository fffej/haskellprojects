{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Base64

data AccessKey = AccessKey
                 {
                   keyName :: B.ByteString
                 , key :: B.ByteString
                 } deriving (Show,Eq)

type Token = B.ByteString

encodeURI :: URI -> B.ByteString
encodeURI x = B.pack $ uriToString id x ""

sign :: B.ByteString -> B.ByteString -> B.ByteString
sign key signingString = encode $ LB.toStrict $ bytestringDigest dig
  where
    strictKey = LB.fromStrict key
    strictString = LB.fromStrict signingString
    dig = hmacSha1 strictString strictString

createSASToken :: URI -> AccessKey -> IO Token
createSASToken uri accessKey = do
  expiry <- round `fmap` getPOSIXTime
  let signingString = B.concat [encodeURI uri, "\n", B.pack $ show expiry]
      signature :: B.ByteString
      signature = sign (key accessKey) signingString
  return $ B.concat [
        "SharedAccessSignature sr=",
        encodeURI uri,
        "&sig=",
        signature,
        "&se=",
        B.pack $ show expiry,
        "&skn=",
        keyName accessKey
        ]  

main :: IO ()
main = do
  return ()
