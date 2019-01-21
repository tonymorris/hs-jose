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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crypto.JOSE.Types.Orphans where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Network.URI (URI, parseURI)
#if ! MIN_VERSION_QuickCheck(2,9,0)
import Test.QuickCheck(Arbitrary(arbitrary))
#endif
import Data.Foldable (toList)
import qualified Data.Vector as V(fromList)

import Control.Lens(Rewrapped, Wrapped(_Wrapped', Unwrapped), _Wrapped, Getting, AReview, iso, view, review)
import Data.Aeson
import Data.Aeson.Types
import Data.Text(Text)

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

viewMaybe :: FromJSON a => Getting b a b -> Object -> Text -> Parser (Maybe b)
viewMaybe k o t = fmap (fmap (view k)) (o .:? t)

(.:|?) :: FromJSON a => Object -> Text -> Parser (Maybe (NonEmpty a))
(.:|?) = viewMaybe (_Wrapped :: Getting (NonEmpty a) (WrappedNonEmpty a) (NonEmpty a))

infixl 9 .:|?

previewEqual :: (ToJSON v, KeyValue kv) => AReview v a -> Text -> a -> kv
previewEqual k t v = t .= review k v

(.|=) :: (ToJSON a, KeyValue kv) => Text -> NonEmpty a -> kv
(.|=) = previewEqual (_Wrapped :: AReview (WrappedNonEmpty a) (NonEmpty a))

infixr 8 .|=

instance FromJSON URI where
  parseJSON = withText "URI" $
    maybe (fail "not a URI") return . parseURI . T.unpack

instance ToJSON URI where
  toJSON = String . T.pack . show


#if ! MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
#endif
