module Freebase where

import Text.JSON
import Network.HTTP
import Network.URI

import Control.Monad

import Data.Maybe (fromJust)

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

touch :: URI
touch = fromJust $ parseURI "http://api.freebase.com/api/service/touch" 

status :: URI
status = fromJust $ parseURI "http://api.freebase.com/api/status" 

version :: URI
version = fromJust $ parseURI "http://api.freebase.com/api/version" 

simpleService :: URI -> IO (Result JSValue)
simpleService s = liftM decode (simpleHTTP (mkRequest GET s) >>= getResponseBody)

mqlReadUri :: String
mqlReadUri = "http://api.freebase.com/api/service/mqlread"

makeQuery :: JSValue -> IO (Result JSValue)
makeQuery s = liftM decode (simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ urlEncode (encode s))) >>= getResponseBody) 

mkSimpleQuery :: [(String,JSValue)] -> JSValue
mkSimpleQuery x = JSObject $ toJSObject [("query", JSObject $ toJSObject x)]

getAlbumList :: String -> IO (Result [String])
getAlbumList artist = do
  response <- makeQuery $ mkSimpleQuery [("type",showJSON "/music/artist")
                                        ,("name",showJSON artist)
                                        ,("album", JSArray [])]
  let albums = (lookupValue (lookupValue response "result") "album")
  return (fmap (map (\(JSString x) -> fromJSString x)) albums)

getReleaseDate :: String -> IO (Result String)
getReleaseDate film = do
  response <- makeQuery $ mkSimpleQuery [("type", showJSON "/film/film")
                                        ,("name", showJSON film)
                                        ,("initial_release_date",  JSNull)]
  let releaseDate = (lookupValue (lookupValue response "result") "initial_release_date")
  return (fmap fromJSString releaseDate)
