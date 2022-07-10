{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{- |
Module: Commence.Runtime.Errors
Description: Custom errors and support for throwing these errors in some monad.

We introduce RuntimeErr's that can be used to wrap either errors that are instances of `IsRuntimeErr`; or exceptions that are otherwise not handled.
These errors then get mapped to Servant errors.

Consider the following example:

@
data AuthErr = LoginFailed Text | PermissionDenied Text
             deriving Show

instance IsRuntimeErr AuthErr where

  httpStatus = \\case
    LoginFailed{} -> HTTP.unauthorized401
    PermissionDenied{} -> HTTP.forbidden403

  userMessage = Just . \\case
    LoginFailed msg -> msg
    PermissionDenied msg -> msg

@

-}
module Commence.Runtime.Errors
  ( RuntimeErr(..)
  , IsRuntimeErr(..)
  , asServantError
  , defaultErrHtml
  -- * Exceptional re-exports for backwards compatibility in dependant packages. 
  , module Mode
  , module Code
  ) where

import           Commence.Runtime.Errors.Code  as Code
import           Commence.Runtime.Errors.Mode  as Mode
import           Control.Lens                  as L
                                         hiding ( (.=) )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=)
                                                , Value(String)
                                                ) -- for convenience, where qualified imports are overkill.
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified GHC.Show                      as Show
import           Network.HTTP.Types
import qualified Network.HTTP.Types            as HTTP
import           Servant.Server                 ( ServerError(..) )
import qualified Text.Blaze.Html               as B
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import qualified Text.Blaze.Html5              as H

-- brittany-disable-next-binding 
-- | A generalised error
data RuntimeErr where
  -- | Capture known error types.
  KnownErr ::IsRuntimeErr e => e -> RuntimeErr
  -- | Capture all exceptions
  RuntimeException ::Exception e => e -> RuntimeErr

deriving anyclass instance Exception RuntimeErr

-- | TODO: add common properties of errors.
class IsRuntimeErr e where

  errCode :: e -> Code.ErrCode

  -- | Construct a `RuntimeErr` from an instance value
  knownErr :: e -> RuntimeErr
  knownErr = KnownErr

  -- | What should the error status be in HTTP-terms?
  httpStatus :: e -> HTTP.Status

  -- | Throw an error created with an instance value
  throwError' :: MonadError RuntimeErr m => e -> m a
  throwError' = throwError . knownErr

  -- | Informative user message
  -- TODO: also add language code as a parameter for localisation (later)
  userMessage :: e -> Maybe Text

  displayErr :: e -> Text
  default displayErr :: Show e => e -> Text
  displayErr e = T.unwords [ "httpStatus =", show $ httpStatus e
                           , "userMessage =", fromMaybe "" (userMessage e)
                           ]

  -- | Header information to supply for returning errors over HTTP.
  httpHeaders :: e -> [Header]
  httpHeaders e = [("x-err-code", errCode e ^. coerced . L.to Code.showErrCode . L.to TE.encodeUtf8)]

  -- | Error specific HTML markup generation. Uses the crude `defaultErrHtml` by default. 
  htmlErr :: e -> B.Html
  htmlErr = defaultErrHtml

-- | Generate an HTML error: TODO improve the default implementation to have a proper design. 
-- For errors with very custom designs, this method may be overriden. 
defaultErrHtml :: forall e . IsRuntimeErr e => e -> B.Html
defaultErrHtml e = H.body $ do
  H.text "Sorry, we've encountered an error."
  H.hr
  maybe mempty addMessage (userMessage e)
  where addMessage txt = H.text txt *> H.hr

instance Show RuntimeErr where
  show = T.unpack . displayErr

instance IsRuntimeErr RuntimeErr where

  errCode = \case
    KnownErr e         -> errCode e
    RuntimeException{} -> "ERR.RUNTIME.EXCEPTION"

  knownErr   = identity

  httpStatus = \case
    KnownErr e         -> httpStatus e
    RuntimeException{} -> HTTP.internalServerError500

  throwError' = throwError

  userMessage = \case
    KnownErr e         -> userMessage e
    RuntimeException{} -> Just "Internal exception, we're sorry about that."

  displayErr = \case
    KnownErr e -> displayErr e
    RuntimeException e ->
      T.unwords ["RuntimeException", show e, T.pack $ displayException e]

-- | Map out a known error to a `ServerError` (from Servant)
asServantError :: IsRuntimeErr e => Mode.ErrMode -> e -> ServerError
asServantError mode e = ServerError
  { errReasonPhrase = T.unpack . TE.decodeUtf8 $ statusMessage
  , errHeaders      = httpHeaders e <> Mode.modeContentType mode
  , ..
  }
 where
  Status errHTTPCode statusMessage = httpStatus e
  -- depending on the mode, we'd like to output the right respose body.
  -- this in combination of the call to `Mode.modeContentType` above will ensure we're outputting the correct response type. 
  errBody                          = case mode of
    Mode.JsonErr -> Aeson.encode . knownErr $ e
    Mode.HtmlErr -> renderHtml $ htmlErr e

instance MonadError RuntimeErr (Either RuntimeErr) where
  throwError = Left
  catchError op' handler = either handler Right op'

instance Aeson.ToJSON RuntimeErr where
  toJSON = \case
    KnownErr e -> Aeson.object
      [ "errorCode" .= errCode e
      , "userMessage" .= userMessage e
      , "errorType" .= String "KNOWN_ERR" -- indicates that we know how to handle this error, and it is a user-defined error. 
      ]

    RuntimeException ex -> Aeson.object
      [ "errorType" .= String "RUNTIME_EXCEPTION"
      , "exception" .= String (show ex)
      ]

