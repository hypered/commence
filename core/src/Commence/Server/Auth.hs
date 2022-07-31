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
  , PostLogoutHeaders
  ) where

import           Commence.Types.NonEmptyText   as NE
import           Servant.API
import qualified Servant.Auth.Server           as SAuth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication user = SAuth.Auth '[SAuth.Cookie] user

-- | Headers that will be returned post a successful authentication.
type PostAuthHeaders
  = '[ Header "Location" NE.NonEmptyText -- redirect the client to some location.
     , Header "Set-Cookie" SAuth.SetCookie -- (this and below) set the cookie(s) for authentication.
     , Header "Set-Cookie" SAuth.SetCookie
     ]

-- | Logout headers are the same as login headers (`PostAuthHeaders` above).
type PostLogoutHeaders
  = '[ Header "Location" NE.NonEmptyText -- redirect the client to some location after logging out.
     , Header "Set-Cookie" SAuth.SetCookie -- (this and below) clear the cookie(s).
     , Header "Set-Cookie" SAuth.SetCookie
     ]
