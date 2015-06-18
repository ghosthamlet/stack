-- | The purpose of this module is to provide better failure messages
-- When parsing a key of an object, this makes sure the key itself will show up
module Data.Aeson.Extended (
    module Export
  , (.:)
  , (.:?)
  , decodeFileOrLoad
  , encodeFile
  ) where

import Data.Aeson as Export hiding ((.:), (.:?))
import qualified Data.Aeson as A

import Data.Aeson.Types hiding ((.:), (.:?))

import Data.Text (unpack, Text)
import Data.Monoid ((<>))
import Control.Exception.Enclosed (tryIO)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) o p = modifyFailure (("failed to parse field " <> unpack p <> ": ") <>) (o A..: p)
{-# INLINE (.:) #-}

(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) o p = modifyFailure (("failed to parse field " <> unpack p <> ": ") <>) (o A..:? p)
{-# INLINE (.:?) #-}

-- | Write to the given file
encodeFile :: (ToJSON a, MonadIO m) => FilePath -> a -> m ()
encodeFile fp x = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    L.writeFile fp $ A.encode x

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file.
decodeFileOrLoad :: (ToJSON a, FromJSON a, MonadIO m) => FilePath -> m a -> m a
decodeFileOrLoad fp mx = do
    eres <- liftIO $ tryIO $ do
        lbs <- L.readFile fp
        return $! A.decode' lbs
    case eres of
        Right (Just x) -> return x
        _ -> do
            x <- mx
            encodeFile fp x
            return x
