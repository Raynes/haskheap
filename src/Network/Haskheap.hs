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
import Data.Aeson          ((.:), decode, FromJSON(..), Value(..))
import Network.HTTP.Conduit
import Network.HTTP.Types (renderSimpleQuery, SimpleQuery, Method, methodGet, methodPost)
import Network (withSocketsDo)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB

parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

data Paste = Paste { getLines    :: Integer
                   , getDate     :: Maybe UTCTime
                   , getID       :: String
                   , getLanguage :: String
                   , getPrivate  :: Bool
                   , getURL      :: Maybe URI
                   , getUser     :: String
                   , getBody     :: String
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
    (v .: "user")                     <*>
    (v .: "contents")

decodePaste :: B.ByteString -> Maybe Paste
decodePaste s = decode s

refheap :: String
refheap = "https://www.refheap.com/api"

refheapReq :: Method -> String -> Maybe SimpleQuery -> Maybe SimpleQuery -> IO B.ByteString
refheapReq method path query body = do
  let queryStr = renderSimpleQuery True <$> query
      url      = refheap ++ path
  req <- parseUrl $ case queryStr of
    Just s  -> url ++ SB.unpack s
    Nothing -> url
  withSocketsDo $ do
    let req'  = req { method = method }
        req'' = case body of
          Just b  -> urlEncodedBody b req'
          Nothing -> req'
    responseBody <$> withManager (httpLbs req'')

-- | Get a paste from refheap.
getPaste :: String -> IO Paste
getPaste id = do
  s <- refheapReq methodGet ("/paste/" ++ id) Nothing Nothing
  let (Just p) = decodePaste s
  return p