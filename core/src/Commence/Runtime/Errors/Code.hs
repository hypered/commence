{-# LANGUAGE OverloadedStrings #-}
module Commence.Runtime.Errors.Code
  ( ErrCode(..)
  , showErrCode
  ) where

import           Commence.Logging
import qualified Data.Aeson                    as Aeson
import qualified Data.String                                   -- Required from the handrolled IsString instance. 
import qualified Data.Text                     as T
import qualified Text.Blaze.Html5              as H

newtype ErrCode = ErrCode [Text]
                deriving (Eq, Show, Monoid, Semigroup) via [Text]

instance TextShow ErrCode where
  showb = showb . showErrCode

-- | Reverse of the IsString instance (below)
showErrCode (ErrCode envs) = T.toUpper . T.intercalate "." $ envs

-- | Take any string; split at @/@; and use it as the ErrCode.
instance IsString ErrCode where
  fromString = ErrCode . T.splitOn "." . T.toUpper . T.pack

instance Aeson.ToJSON ErrCode where
  toJSON = Aeson.String . showErrCode
  {-# INLINE toJSON #-}

instance H.ToMarkup ErrCode where
  toMarkup = H.text . showErrCode
  {-# INLINE toMarkup #-}
