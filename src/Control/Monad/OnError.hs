{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Control.Monad.OnError
  ( module Control.Monad.OnError
  -- * helpers
  , (=<<$)
  ) where

import Prelude hiding (lines, unlines)

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Data.Text (Text, lines, unlines, unpack)
import GHC.Exception (errorCallException)
import qualified Control.Monad.Fail as Fail

import Control.Monad.Extra ((=<<$))


-- ** Detecting errors

-- |Fail the computation if given a 'Left'
onLeftThrowError :: MonadError e m => Either e a -> m a
onLeftThrowError = either throwError pure

-- |Fail the computation if given 'Nothing'
onNothingThrowError :: MonadError e m => e -> Maybe a -> m a
onNothingThrowError e = maybe (throwError e) pure


-- ** Transforming errors

-- |If an error propagates up this call, add some contextual information
annotateError :: MonadError Text m => Text -> m a -> m a
annotateError annotation = flip catchError $ \err -> do
  throwError $ annotation <> ": " <> err

-- |A variant of 'annotateError' which indents the rest of the error message by two spaces
annotateAndIndentError :: MonadError Text m => Text -> m a -> m a
annotateAndIndentError annotation = flip catchError $ \err -> do
  let indentedErr = unlines . fmap ("  " <>) . lines $ err
  throwError $ annotation <> ":\n" <> indentedErr

-- |If an error propagates up this call, transform it using the given function
mapError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
mapError f = onErrorCatch (throwError . f)

-- |If an error propagates up this call, replace it with the given error
replaceError :: MonadError e' m => e' -> ExceptT e m a -> m a
replaceError e' = mapError (const e')


-- ** Handling errors

-- |Run a computation, falling back to the given action upon failure
onErrorCatch :: Monad m => (e -> m a) -> ExceptT e m a -> m a
onErrorCatch catch_ = either catch_ pure <=< onErrorLeft

-- |Run a computation, defaulting to the given value upon failure
onErrorDefault :: Monad m => a -> ExceptT e m a -> m a
onErrorDefault default_ = onErrorMap (const default_)

-- |Run a computation, calling 'fail' upon failure
onErrorFail :: MonadFail m => ExceptT Text m a -> m a
onErrorFail = onErrorCatch (Fail.fail . unpack)

-- |Run a computation, ignoring errors
onErrorIgnore :: Monad m => ExceptT e m () -> m ()
onErrorIgnore = onErrorDefault ()

-- |Run a computation, returning a 'Left' unpon failure
onErrorLeft :: Monad m => ExceptT e m a -> m (Either e a)
onErrorLeft = runExceptT

-- |Run a computation, falling back to the given function upon failure
onErrorMap :: Monad m => (e -> a) -> ExceptT e m a -> m a
onErrorMap f = onErrorCatch (pure . f)

-- |Run a computation, returning 'Nothing' upon failure
onErrorNothing :: Monad m => ExceptT e m a -> m (Maybe a)
onErrorNothing = fmap (either (const Nothing) Just) . onErrorLeft

-- |Run a computation, calling 'throwIO' upon failure
onErrorThrowIO :: MonadIO m => ExceptT Text m a -> m a
onErrorThrowIO = onErrorCatch (liftIO . throwIO . errorCallException . unpack)

-- |Run a computation, calling 'throwIO' upon failure
onErrorThrowM :: MonadThrow m => ExceptT Text m a -> m a
onErrorThrowM = onErrorCatch (throwM . errorCallException . unpack)
