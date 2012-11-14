{-# LANGUAGE OverloadedStrings #-}
module Network.Haskheap
       ( Paste(..)
       , getPaste
       )
where

import Control.Applicative ((<*>), (<$>))
import Control.Monad       (liftM)
import Data.Time.Format    (parseTime)
import Data.Time.Clock     (UTCTime)
import System.Locale       (defaultTimeLocale)
import Network.URI         (URI, parseURI)
import Data.Aeson          ((.:), (.:?), decode, FromJSON(..), Value(..))
import Network.HTTP.Conduit
import Network.HTTP.Types (renderSimpleQuery, SimpleQuery, Method, methodGet, methodPost)
import Network (withSocketsDo)
import Control.Arrow ((***))
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

decodePaste :: B.ByteString -> Maybe Paste
decodePaste s = decode s

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

-- | Get a paste from refheap. Will return IO Nothing if the paste doesn't exist.
getPaste :: PasteID -> IO (Maybe Paste)
getPaste id = do
  s <- refheapReq methodGet ("/paste/" ++ id) Nothing Nothing
  return $ decodePaste s

createPaste :: Contents -> Bool -> Language -> Maybe Auth -> IO (Maybe Paste)
createPaste body private language auth = do
  s <- refheapReq methodPost "/paste" (composeAuth <$> auth) (Just [("contents", body)
                                                                   ,("private", show private)
                                                                   ,("language", language)])
  return $ decodePaste s