-- Copyright (C) 2014, 2015, 2016  Fraser Tweedale
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.JOSE.Types.Orphans where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Network.URI (URI)
import Network.URI as Network (parseURI)
import Test.QuickCheck(Arbitrary(arbitrary), Gen, frequency)
import Data.Foldable (toList)
import qualified Data.Vector as V(fromList)

import Control.Lens(Rewrapped, Wrapped(_Wrapped', Unwrapped), _Wrapped, Getting, AReview, iso, view, review)
import Data.Aeson
import Data.Aeson.Types
import Data.Text(Text)

viewMaybe :: FromJSON a => Getting b a b -> Object -> Text -> Parser (Maybe b)
viewMaybe k o t = fmap (fmap (view k)) (o .:? t)

previewEqual :: (ToJSON v, KeyValue kv) => AReview v a -> Text -> a -> kv
previewEqual k t v = t .= review k v

gettingGen :: Arbitrary s => Getting a s a -> Gen a
gettingGen k = fmap (view k) arbitrary

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = frequency [(1, return Nothing), (3, fmap Just g)]

gettingGenMaybe :: Arbitrary s => Getting a s a -> Gen (Maybe a)
gettingGenMaybe k = genMaybe (fmap (view k) arbitrary)

newtype WrappedNonEmpty a =
  WrappedNonEmpty (NonEmpty a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance WrappedNonEmpty a ~ x => Rewrapped (WrappedNonEmpty a) x

instance Wrapped (WrappedNonEmpty a) where
  type Unwrapped (WrappedNonEmpty a) =
    NonEmpty a
  _Wrapped' =
    iso
      (\(WrappedNonEmpty x) -> x)
      WrappedNonEmpty

instance FromJSON a => FromJSON (WrappedNonEmpty a) where
  parseJSON =
    withArray "WrappedNonEmpty [a]" $ \v -> case toList v of
      [] -> fail "Wrapped Non-empty list required"
      (x:xs) -> mapM parseJSON (WrappedNonEmpty (x :| xs))

instance ToJSON a => ToJSON (WrappedNonEmpty a) where
  toJSON = Array . V.fromList . map toJSON . toList

instance Arbitrary a => Arbitrary (WrappedNonEmpty a) where
  arbitrary = (\h t -> WrappedNonEmpty (h :| t)) <$> arbitrary <*> arbitrary

kvNonEmpty :: (ToJSON a, KeyValue kv) => Text -> NonEmpty a -> kv
kvNonEmpty = previewEqual (_Wrapped :: AReview (WrappedNonEmpty a) (NonEmpty a))

parseNonEmpty :: FromJSON a => Object -> Text -> Parser (Maybe (NonEmpty a))
parseNonEmpty = viewMaybe (_Wrapped :: Getting (NonEmpty a) (WrappedNonEmpty a) (NonEmpty a))

gettingGenNonEmpty :: Arbitrary a => Gen (NonEmpty a)
gettingGenNonEmpty = gettingGen (_Wrapped :: Getting (NonEmpty a) (WrappedNonEmpty a) (NonEmpty a))

gettingGenMaybeNonEmpty :: Arbitrary a => Gen (Maybe (NonEmpty a))
gettingGenMaybeNonEmpty = genMaybe gettingGenNonEmpty

newtype WrappedURI =
  WrappedURI URI
  deriving (Eq, Ord, Show)

instance WrappedURI ~ x => Rewrapped WrappedURI x

instance Wrapped WrappedURI where
  type Unwrapped WrappedURI =
    URI
  _Wrapped' =
    iso
      (\(WrappedURI x) -> x)
      WrappedURI

instance FromJSON WrappedURI where
  parseJSON = withText "URI" $
    maybe (fail "not a URI") return . fmap WrappedURI . Network.parseURI . T.unpack

instance ToJSON WrappedURI where
  toJSON = String . T.pack . show . view _Wrapped

kvURI :: KeyValue kv => Text -> URI -> kv
kvURI = previewEqual (_Wrapped :: AReview WrappedURI URI)

parseURI :: Object -> Text -> Parser (Maybe URI)
parseURI = viewMaybe (_Wrapped :: Getting URI WrappedURI URI)
