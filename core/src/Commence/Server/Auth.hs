{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
#-}
{- |
Module: Commence.Server.Auth
Description: Authentication and authorization module.

-}
module Commence.Server.Auth
  ( UserAuthentication
  , PostAuthHeaders
  ) where

import           Commence.Types.NonEmptyText   as NE
import           Servant.API
import qualified Servant.Auth.Server           as SAuth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication user = SAuth.Auth '[SAuth.Cookie] user

-- | Headers that will be returned post a successful authentication.
type PostAuthHeaders
  = '[ Header "Location" NE.NonEmptyText
     , Header "Set-Cookie" SAuth.SetCookie
     , Header "Set-Cookie" SAuth.SetCookie
     ]

