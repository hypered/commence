{-# LANGUAGE OverloadedStrings #-}
module Commence.Runtime.Errors.Mode
  ( ErrMode(..)
  , modeContentType
  ) where

import           Network.HTTP.Types.Header

{- | An error mode indicates how we'd like to have a failure.

In some cases, we'd like to respond with JSON values in case of errors: eg. in a REST API response. Whereas for hosted HTML
interfaces, we'd like to respond with HTML. 
-}
data ErrMode = JsonErr | HtmlErr
             deriving (Eq, Show)

-- | Get the content-type header value 
modeContentType :: ErrMode -> [Header]
modeContentType = pure . (hContentType, ) . \case
  JsonErr -> "application/json"
  HtmlErr -> "text/html"
