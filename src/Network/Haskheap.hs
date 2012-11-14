{-# LANGUAGE OverloadedStrings #-}
module Network.Haskheap
       ( Paste(..)
       , getPaste
       , createPaste
       , deletePaste
       )
where

import Control.Applicative  ((<*>), (<$>))
import Control.Monad        (liftM)
import Data.Time.Format     (parseTime)
import Data.Time.Clock      (UTCTime)
import System.Locale        (defaultTimeLocale)
import Network.URI          (URI, parseURI)
import Data.Aeson           ((.:), (.:?), (.!=), decode, FromJSON(..), Value(..))
import Network.HTTP.Conduit
import Network.HTTP.Types   (renderSimpleQuery, SimpleQuery, Method, methodGet, methodPost, methodDelete)
import Network              (withSocketsDo)
import Control.Arrow        ((***))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB

parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

type PasteID  = String
type Language = String
type Contents = String
type Query    = [(String, String)]
type Auth     = (String, String)

packQuery :: Query -> SimpleQuery
packQuery = map $ SB.pack *** SB.pack

composeAuth :: Auth -> Query
composeAuth (user, token) = [("username", user), ("token", token)]

data Paste = Paste { getLines    :: Integer
                   , getDate     :: Maybe UTCTime
                   , getID       :: PasteID
                   , getLanguage :: Language
                   , getPrivate  :: Bool
                   , getURL      :: Maybe URI
                   , getUser     :: Maybe String
                   , getBody     :: Contents
                   } deriving (Show)

data Error = Error String deriving (Show)

instance FromJSON Error where
  parseJSON (Object v) = Error <$> (v .: "error")

instance FromJSON Paste where
  parseJSON (Object v) =
    Paste <$>
    (v .: "lines")                    <*>
    (liftM parseRHTime (v .: "date")) <*>
    (v .: "paste-id")                 <*>
    (v .: "language")                 <*>
    (v .: "private")                  <*>
    (liftM parseURI (v .: "url"))     <*>
    (v .:? "user")                    <*>
    (v .: "contents")

refheap :: String
refheap = "https://www.refheap.com/api"

refheapReq :: Method -> String -> Maybe Query -> Maybe Query -> IO B.ByteString
refheapReq method path query body = do
  let queryStr = renderSimpleQuery True . packQuery <$> query
      url      = refheap ++ path
  req <- parseUrl $ case queryStr of
    Just s  -> url ++ SB.unpack s
    Nothing -> url
  withSocketsDo $ do
    let req'  = req { method = method, checkStatus = \_ _ -> Nothing }
        req'' = case body of
          Just b  -> urlEncodedBody (packQuery b) req'
          Nothing -> req'
    responseBody <$> withManager (httpLbs req'')

decodePaste :: B.ByteString -> Either (Maybe Error) Paste
decodePaste s =
  case decode s of
    (Just x) -> Right x
    Nothing  -> Left $ decode s

-- | Get a paste from refheap. Will return IO Nothing if the paste doesn't exist.
getPaste :: PasteID -> IO (Either (Maybe Error) Paste)
getPaste id =
  refheapReq methodGet ("/paste/" ++ id) Nothing Nothing >>=
  return . decodePaste

-- | Create a new paste.
createPaste :: Contents -> Bool -> Language -> Maybe Auth -> IO (Either (Maybe Error) Paste)
createPaste body private language auth =
  refheapReq methodPost "/paste" (composeAuth <$> auth) form >>=
  return . decodePaste
  where form = Just [("contents", body)
                    ,("private", show private)
                    ,("language", language)]

-- | Delete a paste. If it fails for some reason, will return
-- the error message from refheap's API wrapped in Maybe, otherwise Nothing.
deletePaste :: PasteID -> Auth -> IO (Either (Maybe Error) Paste)
deletePaste id auth =
  refheapReq methodDelete ("/paste/" ++ id) (Just $ composeAuth auth) Nothing >>=
  return . decodePaste

-- | Fork a paste.
forkPaste :: PasteID -> Auth -> IO (Either (Maybe Error) Paste) 
forkPaste id auth =
  refheapReq methodPost ("/paste/" ++ id ++ "/fork") (Just $ composeAuth auth) Nothing >>=
  return . decodePaste