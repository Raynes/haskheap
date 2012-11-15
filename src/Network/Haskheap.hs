{-# LANGUAGE OverloadedStrings #-}
-- | A library for interfacing with the refheap (https://www.refheap.com) API.
module Network.Haskheap
       ( Success(..)
       , getPaste
       , createPaste
       , deletePaste
       , forkPaste
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

-- | Parse UTC time from the time string provided by refheap.
parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

-- Various type synonyms to make it clearer what they're used for in context

type PasteID  = String
type Language = String
type Contents = String
type Query    = [(String, String)]
type Auth     = (String, String)

-- | Turns a list of 2 element String tuples and packs the strings into bytestrings.
packQuery :: Query -> SimpleQuery
packQuery = map $ SB.pack *** SB.pack

-- | Takes a 2 tuple of strings and returns a query with username and token assigned
-- to the strings.
composeAuth :: Auth -> Query
composeAuth (user, token) = [("username", user), ("token", token)]

-- | Result of as successful request (either empty or a paste).
data Success = Paste { getLines    :: Integer       -- ^ Lines in the paste.
                     , getDate     :: Maybe UTCTime -- ^ Time the paste was created.
                     , getID       :: PasteID       -- ^ ID of the paste (may be numeric or hash or sorts).
                     , getLanguage :: Language      -- ^ The language of the paste.
                     , getPrivate  :: Bool          -- ^ Whether or not the paste is Private. True is yes.
                     , getURL      :: Maybe URI     -- ^ URL of the paste.
                     , getUser     :: Maybe String  -- ^ User who created the paste. Nothing indicates anonymous.
                     , getBody     :: Contents      -- ^ Body of the paste.
                     }
             | Empty -- ^ Operation was successful, but response is empty.
             deriving (Show)

-- | A simple error box so I can parse refheap error messages into something useful.
data Error = Error String deriving (Show)

instance FromJSON Error where
  parseJSON (Object v) = Error <$> (v .: "error")
  
instance FromJSON Success where
  parseJSON (Object v) =
    Paste <$>
    (v .: "lines")                    <*>
    liftM parseRHTime (v .: "date")   <*>
    (v .: "paste-id")                 <*>
    (v .: "language")                 <*>
    (v .: "private")                  <*>
    liftM parseURI (v .: "url")       <*>
    (v .:? "user")                    <*>
    (v .: "contents")

refheap :: String
refheap = "https://www.refheap.com/api"

-- | A convenience method for sending a request to refheap and getting the body.
-- This function could (and likely should) be made more general and put in a
-- separate library.
refheapReq :: Method          -- ^ The request method.
           -> String          -- ^ Path to append to the refheap API url.
           -> Maybe Query     -- ^ Query parameters.
           -> Maybe Query     -- ^ Form parameters.
           -> IO B.ByteString -- ^ The body of the response of the request.
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

-- | Decode a paste to either a Maybe Error or a Paste. It works by first
-- trying to decode the JSON as a Paste and if that fails, it tries to decode
-- it as an Error.
decodePaste :: B.ByteString -> Either Error Success
decodePaste s =
  case decode s of
    (Just x) -> Right x
    Nothing  -> case decode s of
      (Just x) -> Left x
      Nothing  -> Right Empty

-- | Get a paste from refheap. Will return IO Nothing if the paste doesn't exist.
getPaste :: PasteID -> IO (Either Error Success)
getPaste id =
  liftM decodePaste $ refheapReq methodGet ("/paste/" ++ id) Nothing Nothing

-- | Create a new paste.
createPaste :: Contents -> Bool -> Language -> Maybe Auth -> IO (Either Error Success)
createPaste body private language auth =
  liftM decodePaste $ refheapReq methodPost "/paste" (composeAuth <$> auth) form
  where form = Just [("contents", body)
                    ,("private", show private)
                    ,("language", language)]

-- | Delete a paste. If it fails for some reason, will return
-- the error message from refheap's API wrapped in Maybe, otherwise Nothing.
deletePaste :: PasteID -> Auth -> IO (Either Error Success)
deletePaste id auth =
  liftM decodePaste $
  refheapReq methodDelete ("/paste/" ++ id) (Just $ composeAuth auth) Nothing

-- | Fork a paste.
forkPaste :: PasteID -> Auth -> IO (Either Error Success) 
forkPaste id auth =
  liftM decodePaste $
  refheapReq methodPost ("/paste/" ++ id ++ "/fork") (Just $ composeAuth auth) Nothing
