module Commence.Server.Components.Navbar
  ( Navbar(..)
  , IsNavbarContent(..)
  ) where

import           Text.Blaze.Html5

-- | A very basic navbar: contains the HTML contents that make up the navbar.  
newtype Navbar = Navbar { getNavbarHtml :: Html }
               deriving ToMarkup via Html

class IsNavbarContent a where

  -- | Indicate how we can render something as HTML.
  navbarMarkup :: a -> Html

