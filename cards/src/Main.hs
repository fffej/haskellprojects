{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Request -> (Response -> t) -> t
app req respond = respond $
    case pathInfo req of
        ["yay"] -> yay
        x -> index x

yay :: Response
yay = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]

index :: Show a => a -> Response
index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]                    
