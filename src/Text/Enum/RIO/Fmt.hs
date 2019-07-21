{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Enum.RIO.Fmt
  (
  -- * Overview
  -- $overview

  -- * EnumText, Display Types
  -- $example

  -- * TBuilder
    TBuilder
  -- * UsingBuildable and UsingDisplay
  , UsingBuildable(..)
  , UsingDisplay(..)
  -- * Text.Enum.Text
  , module Text.Enum.Text
  -- * Fmt
  , module Fmt
  ) where

import           Fmt                      hiding (Builder)
import           Fmt.Internal.Core
import qualified Data.ByteString.Lazy     as LBS
import           Data.ByteString.Builder
import qualified Data.Text.Lazy.Builder   as T
import           RIO
import           Text.Enum.Text


{- $overview
If you want to use @fmt@ with @rio@, preparing 'Utf8Builder' log messages with
`fmt` and so forth then you can just import this module along side @RIO@.

See the demo program for a working example.
-}


-- | RIO export @Builder@ as the @ByteString@ builder so we have to hide it and
-- provide this in its stead.
type TBuilder = T.Builder

{- $example
To place an 'EnumText' type in 'Display' (with the 'Buildable' and
'TextParsable' instance reqiored by 'EnumText') you can do something like this:

@
{\-\# LANGUAGE DeriveAnyClass    #-\}
{\-\# LANGUAGE DerivingVia       #-\}
{\-\# LANGUAGE NoImplicitPrelude #-\}

import           RIO
import           Text.Enum.RIO.Fmt

data Foo = FOO_bar | FOO_bar_baz
  deriving (Bounded,Enum,EnumText,Eq,Ord,Show)
  deriving (Buildable,Display,TextParsable) via UsingEnumText  Foo
@
-}

-- | @derive@ @via@ this type if you have a `Buildable` and want to derive a
-- corresponding 'Display' instance
newtype UsingBuildable a = UsingBuildable { _UsingBuildable :: a }

-- | @derive@ @via@ this type if you have a `Display` type and want to derive a
-- corresponding 'Buildable' instance
newtype UsingDisplay   a = UsingDisplay   { _UsingDisplay   :: a }

instance Buildable a => Display (UsingBuildable a) where
  display (UsingBuildable x) =
    Utf8Builder
      $ byteString
      $ encodeUtf8
      $ fmt
      $ build x

instance Display a => Buildable (UsingDisplay a) where
  build (UsingDisplay x) =
    build
      $ decodeUtf8With lenientDecode
      $ LBS.toStrict
      $ toLazyByteString
      $ getUtf8Builder
      $ display x

instance EnumText a => Display (UsingEnumText a) where
  display (UsingEnumText x) =
    Utf8Builder
      $ byteString
      $ encodeUtf8
      $ fmt
      $ buildEnumText x

-- | With this instance we can use `fmt` to generate `Utf8Builder` strings
instance FromBuilder Utf8Builder where
  fromBuilder = Utf8Builder . byteString . fromBuilder
