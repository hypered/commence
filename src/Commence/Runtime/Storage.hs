{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module: Prototype.Runtime.Storage
Description: Abstract Storage

The goal of these typeclasses is to abstract /how/ data is stored; and
only rely on some environment/monad @m@; for which we can implement
these instances.

The core functions here are just
- dbUpdate
- dbSelect

And these functions can be implemented more concretely with the
definition of the @m@ in which they operate.

-}
module Commence.Runtime.Storage
  ( DBIdentity(..)
  , DBStorage(..)
  , DBTransaction(..)
  , DBStorageOps(..)
  -- * Performing updates and getting values. 
  , gettingAffected
  , gettingAffectedFirstMaybe
  , gettingAffectedFirstErr
  ) where

import           Commence.Runtime.Errors       as Errs
import qualified Data.Text                     as T
import qualified Data.Typeable                 as Typeable
import qualified Network.HTTP.Types.Status     as Status
import           Protolude

-- | A class with the properties indicating that something has some notion of a unique ID in a storage layer.
-- Eg. for a user, this can be the username/namespace, etc.
class DBIdentity a where
  -- | The ID of the stored value, this value should be unique.
  type DBId a :: Type
  -- | Get the ID from a larger product type, for example.
  dbId :: a -> DBId a

-- | Operations that can be performed on users.
class DBIdentity a => DBStorageOps a where
  -- | A sum type, usually, indicating the kinds of queries that can be run to get values.
  data DBSelect a :: Type
  -- | A sum type, usually, indicating the kinds of queries that can be run to update/insert/delete values.
  data DBUpdate a :: Type

-- | If some @m@ supports atomic transactions, it must implement this class. 
class DBTransaction (m :: Type -> Type) (txn :: Type -> Type) | m -> txn where

  -- | Lift a transactional computation to the underlying @m@.
  liftTxn :: forall b . txn b -> m (Either Errs.RuntimeErr b)

-- | Same as `DBStorageOps`; but here we define /how/ we run the operations defined.
class ( DBIdentity a
      , DBStorageOps a
      , MonadError Errs.RuntimeErr m
      , DBTransaction m txn
      ) => DBStorage m txn a where

  -- | The datatype representing the database. 
  type Db m txn a :: Type

  -- | The errors raised when a DB operation fails.
  type DBError m txn a :: Type

  -- | Execute an update, reporting the list of IDs that were affected due to it.
  dbUpdate :: Db m txn a -> DBUpdate a -> txn (Either (DBError m txn a) [DBId a])

  -- | Execute a select, returning the rows that were matched by the query.
  dbSelect :: Db m txn a -> DBSelect a -> txn [a]

-- | Perform a DBUpdate and get all the affected entities. 
gettingAffected
  :: forall a m txn
   . ( DBStorage m txn a
     , Monad txn
     , MonadError Errs.RuntimeErr m
     , Errs.IsRuntimeErr (DBError m txn a)
     )
  => Db m txn a
  -> (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m [a]
gettingAffected db mkSelect u =
  either throwError' pure . join =<< liftTxn @m performUpdate
 where
  performUpdate = dbUpdate @m db u >>= \case
    Left  err -> pure $ Left (Errs.knownErr err)
    Right ids -> Right <$> concatMapM (dbSelect @m @txn db . mkSelect) ids


-- | Perform a DBUpdate and get the first of the affected entities. 
gettingAffectedFirstMaybe
  :: forall a m txn
   . (DBStorage m txn a, Monad txn, Errs.IsRuntimeErr (DBError m txn a))
  => Db m txn a
  -> (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m (Maybe a)
gettingAffectedFirstMaybe db mkSelect =
  gettingAffected db mkSelect >=> pure . headMay

-- | Perform a DBUpdate and get the first of the affected entities: but throw errors when no resources can be retrieved. 
gettingAffectedFirstErr
  :: forall a m txn
   . ( DBStorage m txn a
     , Monad txn
     , MonadError Errs.RuntimeErr m
     , Errs.IsRuntimeErr (DBError m txn a)
     , Typeable a
     )
  => Db m txn a
  -> (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m a
gettingAffectedFirstErr db mkSelect =
  gettingAffectedFirstMaybe db mkSelect >=> maybe resourceNotFound pure
 where
  resourceNotFound =
    Errs.throwError'
      . ResourceNotFound (typeRep $ Proxy @a)
      $ "No values retrieved after update."

data StorageErr = ResourceNotFound TypeRep Text
  deriving Show

instance Errs.IsRuntimeErr StorageErr where
  errCode = errCode' . \case
    ResourceNotFound tyRep _ -> prefix <> "NOT_FOUND"
     where
      prefix =
        ( Errs.ErrCode
        . pure
        . T.toUpper
        . T.pack
        . Typeable.tyConName
        . Typeable.typeRepTyCon
        $ tyRep
        )
    where errCode' = mappend "ERR.STORAGE"

  httpStatus = \case
    ResourceNotFound{} -> Status.notFound404

  userMessage = Just . \case
    ResourceNotFound _ msg -> msg
