module Composite.Aeson.Compat 
  ( CompositeFormat(..)
  ) where

import Composite.Aeson.Base (parseJsonWithFormat', toJsonWithFormat)
import Composite.Aeson.Formats.Default (DefaultJsonFormat(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

newtype CompositeFormat a = CompositeFormat {unCompositeFormat :: a}

instance DefaultJsonFormat a => FromJSON (CompositeFormat a) where
  parseJSON = fmap CompositeFormat . parseJsonWithFormat' defaultJsonFormat

instance DefaultJsonFormat a => ToJSON (CompositeFormat a) where
  toJSON = toJsonWithFormat defaultJsonFormat . unCompositeFormat
