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

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Vector                   as V

-- | Options non how to modify JSONs.
data Opt =
  CleanupFieldNames -- ^ Cleanup field names: drop leading non-alpha-num chars, use proper case for leading char. 
  | DropNulls
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
-- | Modifications when we want to clean up field names: non alpha-numeric leading characters can be removed. 
instance ModifyJSON 'CleanupFieldNames where
  modifyJSON = \case
    Object km ->
      -- since the new versions of Aeson give us an opaque Key datatype (for reasons that are beyond my understanding)
      -- we need to go via conversion to a Map Text Value and back to the new and "fancy" KeyValue. 
      Object
        . KM.fromMapText
        . Map.foldrWithKey' cleanupKey mempty
        $ KM.toMapText km
     where
      cleanupKey k v newKM =
        let cleanKey = over _head toLower . T.dropWhile (not . isAlphaNum) $ k
        in  Map.insert cleanKey (modifyJSON @'CleanupFieldNames v) newKM

    Array vs -> Array $ modifyJSON @'CleanupFieldNames <$> vs

    other -> other

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

