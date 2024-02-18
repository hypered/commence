{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Commence.JSON.Pretty
Description: Modify JSON values on the fly.

Consider:

@

data Foo = Foo
  { _First :: Maybe Int
  , second :: Maybe Text
  }
  deriving Generic
  deriving anyclass ToJSON

foo0 = Foo (Just 1) (Just "hi")
foo1 = Foo Nothing Nothing
foo2 = Foo Nothing (Just "hi")

@

We can then have:

@

λ> putStrLn $ encode foo0 
{"_First":1,"second":"hi"}
*Commence.Types.NonEmptyText Data.Aeson Commence.JSON.Pretty
λ> putStrLn . encode . PrettyJSON @'[ 'CleanupFieldNames] $ foo0
{"first":1,"second":"hi"}
*Commence.Types.NonEmptyText Data.Aeson Commence.JSON.Pretty
λ> putStrLn . encode . PrettyJSON @'[ 'CleanupFieldNames, 'DropNulls ] $ foo0
{"first":1,"second":"hi"}
*Commence.Types.NonEmptyText Data.Aeson Commence.JSON.Pretty
λ> putStrLn . encode . PrettyJSON @'[ 'CleanupFieldNames, 'DropNulls ] $ foo1
{}
*Commence.Types.NonEmptyText Data.Aeson Commence.JSON.Pretty
λ> putStrLn . encode . PrettyJSON @'[ 'CleanupFieldNames, 'DropNulls ] $ foo2
{"second":"hi"}

@

-}
module Commence.JSON.Pretty
  ( Opt(..)
  , PrettyJSON(..)
  , ModifyJSON(..)
  , ModifyJSONs
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap             as KM
import           Data.Vector                   as V

-- | Options non how to modify JSONs.
data Opt = DropNulls
  deriving (Eq, Show)

-- | A newtype wrapper that prettifies the JSON output.
newtype PrettyJSON (opts :: [Opt]) a = PrettyJSON { _unPrettyJSON :: a }
                     deriving (Show, Eq, FromJSON) via a

-- | A class that indicates how we can modify JSONs. 
class ModifyJSON (opt :: Opt) where
  -- | Modify a JSON value for the given option.
  modifyJSON :: Value -> Value

type family ModifyJSONs (opts :: [Opt]) :: Constraint where
  ModifyJSONs '[] = ()
  ModifyJSONs (opt ': opts) = (ModifyJSON opt, ModifyJSONs opts)

-- brittany-disable-next-binding 
-- | Drop null values in arrays and objects. 
instance ModifyJSON 'DropNulls where
  modifyJSON = \case
    Object km -> Object . KM.map (modifyJSON @'DropNulls) . KM.filter (/= Null) $ km
    Array  vs -> Array . V.map (modifyJSON @'DropNulls) . V.filter (/= Null) $ vs
    other     -> other

instance ( ToJSON a
         , ToJSON (PrettyJSON opts a)
         , ModifyJSON opt
         , ModifyJSONs opts
         ) => ToJSON (PrettyJSON ( opt ': opts)  a) where
  toJSON (PrettyJSON a) =
    modifyJSON @opt -- apply the current option. 
      . toJSON
      $ PrettyJSON @opts a -- apply the rest of the options.

instance {-# OVERLAPPING #-} ToJSON a => ToJSON (PrettyJSON '[] a) where
  toJSON = toJSON . _unPrettyJSON

