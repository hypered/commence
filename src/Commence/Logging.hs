{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Commence.Logging
Description: Custom prelude for Commence

We import Protolude and re-export it, for the usual protolude niceties; and
this module is meant to add more such re-exports as necessary as the project
progresses.

-}
module Commence.Logging
  (
  -- * Text handling.
    sentence
  , ellipsis

  -- ** Logging.
  , ML.runLogT'
  , ML.runLogT
  , ML.localEnv
  , ML.Logger
  , ML.Level(..)
  , ML.LogType
  , ML.simpleTimeFormat
  , ML.makeDefaultLogger
  , ML.TextShow(..)
  , ML.MonadLog(..)

  -- *** Logging levels.
  , ML.levelDebug
  , ML.levelInfo
  , ML.levelCritical
  , ML.levelError

  -- *** Loggers with proper sentence termination with periods.
  , debug
  , info
  , warning
  , error
  , critical
  , debugW
  , infoW
  , warningW
  , errorW
  , criticalW

  -- *** Loggers with proper sentence termination with ellipsis.
  , debugE
  , infoE
  , warningE
  , errorE
  , criticalE

  -- * Application name.
  , AppName(..)
  , unAppName
  , showAppName
  , parseAppName

  -- * Logger.
  , AppNameLogger

  -- * Utils.
  , pShowStrict
  , pShowLazy
  ) where

import           Control.Lens
import qualified Control.Monad.Log             as ML
import qualified Data.String                   as Str  -- required for IsString instance.
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Options.Applicative           as A
import Protolude
import qualified Text.Pretty.Simple            as PS


-- | A type alias for convenience: this is a `ML.Logger` where the `env` type
-- is an `AppName`.
type AppNameLogger = ML.Logger AppName

-- | Terminate a sentence with a period; avoids clumsy mappends etc. for
-- properly formatting sentences.

sentence :: Text -> Text
sentence = (`mappend` ".")

-- | Terminate a sentence with an ellipsis; avoids clumsy mappends etc. for
-- properly formatting sentences.

ellipsis :: Text -> Text
ellipsis = (`mappend` "…")

--------------------------------------------------------------------------------
-- Logging functions that log with properly terminated sentences.

debugE :: ML.MonadLog env m => Text -> m ()
debugE = ML.debug . ellipsis

infoE :: ML.MonadLog env m => Text -> m ()
infoE = ML.info . ellipsis

warningE :: ML.MonadLog env m => Text -> m ()
warningE = ML.warning . ellipsis

errorE :: ML.MonadLog env m => Text -> m ()
errorE = ML.error . ellipsis

criticalE :: ML.MonadLog env m => Text -> m ()
criticalE = ML.critical . ellipsis

debug :: ML.MonadLog env m => Text -> m ()
debug = ML.debug . sentence

info :: ML.MonadLog env m => Text -> m ()
info = ML.info . sentence

warning :: ML.MonadLog env m => Text -> m ()
warning = ML.warning . sentence

error :: ML.MonadLog env m => Text -> m ()
error = ML.error . sentence

critical :: ML.MonadLog env m => Text -> m ()
critical = ML.critical . sentence

debugW :: (ML.MonadLog env m, Foldable f) => f Text -> m ()
debugW = debug . T.unwords . toList

infoW :: (ML.MonadLog env m, Foldable f) => f Text -> m ()
infoW = info . T.unwords . toList

warningW :: (ML.MonadLog env m, Foldable f) => f Text -> m ()
warningW = warning . T.unwords . toList

errorW :: (ML.MonadLog env m, Foldable f) => f Text -> m ()
errorW = error . T.unwords . toList

criticalW :: (ML.MonadLog env m, Foldable f) => f Text -> m ()
criticalW = critical . T.unwords . toList

-- | An application name: lets us group logging etc. with @/@ as separators.
newtype AppName = AppName { _unAppName :: [Text] }
                deriving (Eq, Show, Semigroup, Monoid) via [Text]

instance ML.TextShow AppName where
  showb = ML.showb . showAppName

-- | Reverse of the IsString instance (below)
showAppName :: AppName -> Text
showAppName (AppName envs) = T.intercalate "/" envs

makeLenses ''AppName

-- | Take any string; split at @/@; and use it as the AppName.
instance IsString AppName where
  fromString = appNameFromText . T.pack

appNameFromText :: Text -> AppName
appNameFromText = AppName . T.splitOn "/"

-- | Parse the application name (`AppName`) wherein the sections are separated
-- by @/@.  Note the use of fromString which ensures we split out the incoming
-- string properly.
parseAppName :: Text -> A.Parser AppName
parseAppName defaultName = Str.fromString <$> A.strOption
  (  A.long "logging-root-app-name"
  <> A.value (T.unpack defaultName)
  <> A.metavar "STRING"
  <> A.help "Application name: sections separated by `/`"
  )

-- | The default pretty show expects lazy text, which is not compatible with
-- our logging.
pShowStrict :: Show a => a -> Text
pShowStrict = TL.toStrict . PS.pShow
{-# INLINE pShowStrict #-}

-- | Renamed version of the pShow from the pretty-simple lib.
pShowLazy :: Show a => a -> TL.Text
pShowLazy = PS.pShow
{-# INLINE pShowLazy #-}
