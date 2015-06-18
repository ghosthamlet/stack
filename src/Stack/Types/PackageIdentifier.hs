{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Package identifier (name-version).

module Stack.Types.PackageIdentifier
  (PackageIdentifier(..)
  ,toTuple
  ,fromTuple
  ,parsePackageIdentifier
  ,parsePackageIdentifierFromString
  ,packageIdentifierVersion
  ,packageIdentifierName
  ,packageIdentifierParser
  ,packageIdentifierString
  ,packageIdentifierText)
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (Exception)
import           Control.Monad (forM)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Aeson.Extended
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Prelude hiding (FilePath)
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | A parse fail.
data PackageIdentifierParseFail
  = PackageIdentifierParseFail ByteString
  deriving (Typeable)
instance Show PackageIdentifierParseFail where
    show (PackageIdentifierParseFail bs) = "Invalid package identifier: " ++ show bs
instance Exception PackageIdentifierParseFail

-- | A pkg-ver combination.
data PackageIdentifier =
  PackageIdentifier !PackageName
                    !Version
  deriving (Eq,Ord,Generic,Data,Typeable)

instance NFData PackageIdentifier where
  rnf (PackageIdentifier !p !v) =
      seq (rnf p) (rnf v)

instance Hashable PackageIdentifier
instance Binary PackageIdentifier

instance Show PackageIdentifier where
  show = show . packageIdentifierString

instance ToJSON PackageIdentifier where
  toJSON = toJSON . packageIdentifierString
instance FromJSON PackageIdentifier where
  parseJSON = withText "PackageIdentifier" $ \t ->
    case parsePackageIdentifier $ encodeUtf8 t of
      Left e -> fail $ show (e, t)
      Right x -> return x

instance ToJSON a => ToJSON (Map PackageIdentifier a) where
    toJSON = toJSON . Map.mapKeysWith const packageIdentifierString
instance FromJSON a => FromJSON (Map PackageIdentifier a) where
    parseJSON v = do
        m <- parseJSON v
        fmap Map.fromList $ forM (Map.toList m) $ \(x, y) ->
            case parsePackageIdentifier $ encodeUtf8 x of
                Left e -> fail $ show e
                Right x' -> return (x', y)

-- | Convert from a package identifier to a tuple.
toTuple :: PackageIdentifier -> (PackageName,Version)
toTuple (PackageIdentifier n v) = (n,v)

-- | Convert from a tuple to a package identifier.
fromTuple :: (PackageName,Version) -> PackageIdentifier
fromTuple (n,v) = PackageIdentifier n v

-- | Get the version part of the identifier.
packageIdentifierVersion :: PackageIdentifier -> Version
packageIdentifierVersion (PackageIdentifier _ ver) = ver

-- | Get the name part of the identifier.
packageIdentifierName :: PackageIdentifier -> PackageName
packageIdentifierName (PackageIdentifier name _) = name

-- | A parser for a package-version pair.
packageIdentifierParser :: Parser PackageIdentifier
packageIdentifierParser =
  do name <- packageNameParser
     char8 '-'
     version <- versionParser
     return (PackageIdentifier name version)

-- | Convenient way to parse a package identifier from a bytestring.
parsePackageIdentifier :: MonadThrow m => ByteString -> m PackageIdentifier
parsePackageIdentifier x = go x
  where go =
          either (const (throwM (PackageIdentifierParseFail x))) return .
          parseOnly (packageIdentifierParser <* endOfInput)

-- | Migration function.
parsePackageIdentifierFromString :: MonadThrow m => String -> m PackageIdentifier
parsePackageIdentifierFromString =
  parsePackageIdentifier . S8.pack

-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier n v) = show n ++ "-" ++ show v

-- | Get a Text representation of the package identifier; name-ver.
packageIdentifierText :: PackageIdentifier -> Text
packageIdentifierText = T.pack .  packageIdentifierString
