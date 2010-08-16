{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Freebase
import Text.JSON

data AlbumLister = AlbumLister {
      ajaxStatic :: Static
}

staticFiles "static/"

mkYesod "AlbumLister" [$parseRoutes|
  /       HomeR   GET
  /static StaticR Static ajaxStatic
  /albums/#String AlbumsR GET
|]

instance Yesod AlbumLister where
    approot _ = ""

--    
getHomeR :: Handler AlbumLister RepHtml
getHomeR = hamletToRepHtml [$hamlet|
%html
  %head
    %title Album Lister
    %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
    %script!src=@StaticR.script_js@
  %body
    %h1 Album Lister
    %p Enter the name of a band:
    %input!type=text!onchange="listAlbums(this.value)"
    %hr
    #output
|]              

getAlbumsR :: String -> Handler AlbumLister RepJson
getAlbumsR band = do
  albumsResult <- liftIO $ getAlbumList band
  case albumsResult of
    (Ok albums) -> jsonToRepJson $ jsonMap [("name", jsonList $ map jsString albums)]
    (Error _)   -> jsonToRepJson $ jsonMap [("error", jsString "Unknown band")]

jsString :: String -> Json
jsString = jsonScalar . string

main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  basicHandler 3000 $ AlbumLister static