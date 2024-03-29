{-# LANGUAGE
    DeriveAnyClass
  , GeneralizedNewtypeDeriving
#-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Prototype.Types.NonEmptyText
Description: Safe texts guaranteed to be non-empty
-}
module Commence.Types.NonEmptyText
  ( NonEmptyText'(..)
  , NonEmptyText
  , unNonEmptyText
  -- * Constructors
  , nonEmptyText
  , nonEmptyTextCoerce
  -- * Deconstructors
  , nonEmptyToTextCoerce
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.String                    ( IsString(..) )
import qualified Data.Text                     as T
import           Protolude
import           Text.Blaze.Html                ( ToMarkup
                                                , ToValue
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..)
                                                , ToHttpApiData
                                                )

-- * NonEmptyText

-- ** Types and aliases

-- | `NonEmptyText` is `NonEmpty` specialised to `Text`.
newtype NonEmptyText' t = NonEmptyText { _unNonEmptyText :: t }
                        deriving stock (Eq, Generic, Ord)
                        deriving ( ToJSON
                                 , ToJSONKey
                                 , FromJSONKey
                                 , Hashable
                                 , ToHttpApiData
                                 , ToMarkup
                                 , ToValue
                                 , Show
                                 , Semigroup
                                 ) via t

type NonEmptyText = NonEmptyText' Text

makeLenses ''NonEmptyText'

instance FromHttpApiData NonEmptyText where
  parseUrlPiece = maybe (Left "Text cannot be empty") Right . nonEmptyText

-- | Safe constructor
-- Leading and trailing whitespace is stripped from a Just value.
nonEmptyText :: Text -> Maybe NonEmptyText
nonEmptyText t' | T.null t  = Nothing
                | otherwise = Just . NonEmptyText $ t' -- when the stripped text is non-empty; we'd like to keep the whitespaces; if any.
  where t = T.strip t'

-- | TODO: improve this with proper error messages.
instance FromJSON NonEmptyText where
  parseJSON = \case
    String t -> maybe mempty pure . nonEmptyText $ t
    _        -> mempty

-- | A more clever form of `nonEmptyText` for more convenient construction.
nonEmptyTextCoerce :: Coercible a NonEmptyText => Text -> Maybe a
nonEmptyTextCoerce = fmap convert . nonEmptyText where convert = view coerced

instance IsString NonEmptyText where
  fromString = fromMaybe def' . nonEmptyText . T.pack
    where def' = NonEmptyText "[EMPTY_TEXT]"

nonEmptyToTextCoerce :: Coercible a NonEmptyText => a -> Text
nonEmptyToTextCoerce = view $ coerced . unNonEmptyText

