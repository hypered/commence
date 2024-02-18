{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module: Commence.MultiLogging
Description: A generalisation of `MonadLog` and friends (From
Control.Monad.Log) but with support for logging over multiple loggers.

Example use case: consider we want to
1. Output logs to stdout
2. Output logs to an HTTP based logging collector.

This module provides abstractions for supporting multiple loggers that can
achieve that.
-}
module Commence.Multilogging
  (
  -- * Logging configuration.
    LoggingConf(..)
  , LoggingOutput(..)

  -- ** Configuration lenses.
  , lcOutput
  , lcRootAppName
  , lcLogLevel

  -- ** Parsing the configuration from the CLI.
  , parseLoggingConf

  -- * Multiple loggers in a type-safe newtype.
  , AppNameLoggers(..)

  -- ** Lenses.
  , appNameLoggers
  , localEnv

  -- * Instantiate loggers.
  , makeDefaultLoggersWithConf
  , createLoggers

  -- * Flush and close the loggers, eg. at the end of an application lifecycle.
  , flushAndCloseLoggers

  -- * Typeclass for monads that support logging over multiple loggers.
  , MonadAppNameLogMulti(..)

  -- * Common logging functions over multiple loggers.
  , logInfo
  , debug
  , info
  , warning
  , error
  , critical

  -- * Common logging functions over multiple loggers, with explicit AppName
  -- tags.
  , debugEnv
  , infoEnv
  , warningEnv
  , errorEnv
  , criticalEnv

  -- * Re-exports for convenience.
  , L.AppName(..)
  , L.pShowStrict
  , L.pShowLazy
  ) where

import qualified Commence.Logging              as L
import           Control.Lens
import           Control.Monad.Catch            ( MonadMask )
import qualified Control.Monad.Log             as ML
import qualified Options.Applicative           as A
import           Protolude
import qualified System.Log.FastLogger         as FL

-- | Configuration for logging.
data LoggingConf = LoggingConf
  { _lcOutput      :: LoggingOutput
    -- ^ Initial logging output.
  , _lcRootAppName :: L.AppName
    -- ^ Application name to use at the root of the logger.
  , _lcLogLevel    :: L.Level
    -- ^ The min. logging level to output. Any logging under this level will be
    -- filtered out.
  }
  deriving (Eq, Show)

data LoggingOutput = LoggingFile FilePath | LoggingStdout | NoLogging
  deriving (Eq, Show)

-- | Parse the logging configuration from the CLI.
parseLoggingConf :: A.Parser LoggingConf
parseLoggingConf = do
  _lcOutput      <- parseLogType
  _lcRootAppName <- L.parseAppName
  _lcLogLevel    <- logLevel
  pure LoggingConf { .. }

logLevel :: A.Parser ML.Level
logLevel =
  A.option (A.eitherReader readEither)
    $  A.long "logging-level"
    <> A.value ML.levelInfo
    <> A.showDefault
    <> A.metavar "LOGGING_LEVEL"

parseLogType :: A.Parser LoggingOutput
parseLogType = off <|> logStdout <|> file

off :: A.Parser LoggingOutput
off = A.flag' NoLogging $ A.long "no-logging" <> A.help "Turn logging off."

logStdout :: A.Parser LoggingOutput
logStdout =
  A.flag' LoggingStdout $ A.long "stdout" <> A.help "Log to stdout."

file :: A.Parser LoggingOutput
file =
  LoggingFile
    <$> (  A.strOption
        $  A.long "log"
        <> A.value "./curiosity.log"
        <> A.showDefault
        <> A.metavar "PATH"
        )

-- | A set of multiple loggers.
newtype AppNameLoggers = AppNameLoggers { _appNameLoggers :: [L.AppNameLogger]
                                        -- ^ Multiple loggers to log over.
                                        }

makeLenses ''AppNameLoggers

-- | Generate an initial logger for a given configuration.
makeDefaultLoggersWithConf :: MonadIO m => LoggingConf -> m AppNameLoggers
makeDefaultLoggersWithConf LoggingConf {..} = case _lcOutput of
  NoLogging -> pure $ AppNameLoggers []
  LoggingStdout ->
    AppNameLoggers
      .   (: [])
      <$> L.makeDefaultLogger L.simpleTimeFormat
                              (FL.LogStdout 1024)
                              _lcLogLevel
                              _lcRootAppName
  LoggingFile path ->
    AppNameLoggers
      .   (: [])
      <$> L.makeDefaultLogger L.simpleTimeFormat
                              (FL.LogFile (flspec path) 1024)
                              _lcLogLevel
                              _lcRootAppName

flspec :: FilePath -> FL.FileLogSpec
flspec path = FL.FileLogSpec path (1024 * 1024) 10
  -- 1MB, or about 5240 200-character lines, and keeping 10 rotated files.

createLoggers :: MonadIO m => LoggingConf -> m AppNameLoggers
createLoggers loggingConf = liftIO $ do
  mloggers <- try @SomeException $ makeDefaultLoggersWithConf loggingConf
  case mloggers of
    Left loggerErrs -> panic $
      "Cannot instantiate, logger instantiation failed: " <> show loggerErrs
    Right loggers -> pure loggers

-- | Clean-up & close the loggers.
flushAndCloseLoggers :: MonadIO m => AppNameLoggers -> m ()
flushAndCloseLoggers (AppNameLoggers loggers) =
  liftIO $ mapM_ ML.cleanUp loggers

makeLenses ''LoggingConf

-- | A Monad for logging over a collection of logs.
class MonadAppNameLogMulti m where
  -- | Get the current set of loggers.
  askLoggers :: m AppNameLoggers
  -- | Locally modified loggers, useful for localised logging envs.
  localLoggers :: (L.AppNameLogger -> L.AppNameLogger) -> m a -> m a

-- | Local logging environment over multiple loggers.
localEnv
  :: forall m a
   . MonadAppNameLogMulti m
  => (L.AppName -> L.AppName)
  -> m a
  -> m a
localEnv modEnv = localLoggers modifyEnv
 where
  modifyEnv logger = logger { ML.environment = modEnv (ML.environment logger) }

--------------------------------------------------------------------------------
-- | Useful to log over some AppNameLoggers without the below constraint
-- available.
logInfo
  :: MonadIO m
  => (L.AppName -> L.AppName)
  -> AppNameLoggers
  -> Text
  -> m ()
logInfo env (AppNameLoggers loggers) msg = mapM_ logOver loggers
  where logOver l = L.runLogT' l . L.localEnv env $ L.info msg

--------------------------------------------------------------------------------
-- | A unified set of minimal constraints required for us to be able to log
-- over multiple loggers.
type LoggingConstraints m = (MonadIO m, MonadMask m, MonadAppNameLogMulti m)

debug :: LoggingConstraints m => Text -> m ()
debug = runLogFuncMulti . ML.debug

info :: LoggingConstraints m => Text -> m ()
info = runLogFuncMulti . ML.info

warning :: LoggingConstraints m => Text -> m ()
warning = runLogFuncMulti . ML.warning

error :: LoggingConstraints m => Text -> m ()
error = runLogFuncMulti . ML.error

critical :: LoggingConstraints m => Text -> m ()
critical = runLogFuncMulti . ML.critical

debugEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
debugEnv name = runLogFuncMulti . ML.debug' name

infoEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
infoEnv name = runLogFuncMulti . ML.info' name

warningEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
warningEnv name = runLogFuncMulti . ML.warning' name

errorEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
errorEnv name = runLogFuncMulti . ML.error' name

criticalEnv :: LoggingConstraints m => Text -> m ()
criticalEnv = runLogFuncMulti . ML.debug

runLogFuncMulti :: LoggingConstraints m => ML.LogT L.AppName m () -> m ()
runLogFuncMulti logFunc = do
  AppNameLoggers loggers <- askLoggers
  mapM_ runLoggerOver loggers
  where runLoggerOver logger = ML.runLogT' logger logFunc
