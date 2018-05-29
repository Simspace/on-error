{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Control.Monad.Trans.OnError
  ( module Control.Monad.Trans.OnError
  -- * helpers
  , (=<<$)
  ) where

import Prelude hiding (lines, unlines)

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Except (ExceptT, catchE, withExceptT, runExceptT, throwE)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Data.Text (Text, lines, unlines, unpack)
import GHC.Exception (errorCallException)
import qualified Control.Monad.Fail as Fail

import Control.Monad.Extra ((=<<$))


-- ** Detecting errors

-- |Lift one of the @on[Condition]ThrowError@ checks so it works on a computation instead of a value
--
-- > onNothingThrowError e             ::    Maybe a  -> ExceptT e m a
-- > liftCheck (onNothingThrowError e) :: m (Maybe a) -> ExceptT e m a
liftCheck :: Monad m
          => (a -> ExceptT e m b)
          -> (m a -> ExceptT e m b)
liftCheck f mx = f =<< lift mx

-- |Fail the computation if given a 'Left'
onLeftThrowError :: Monad m => Either e a -> ExceptT e m a
onLeftThrowError = either throwE pure

-- |Fail the computation if given 'Nothing'
onNothingThrowError :: Monad m => e -> Maybe a -> ExceptT e m a
onNothingThrowError e = maybe (throwE e) pure


-- ** Transforming errors

-- |If an error propagates up this call, add some contextual information
annotateError :: Monad m => Text -> ExceptT Text m a -> ExceptT Text m a
annotateError annotation = flip catchE $ \err -> do
  throwE $ annotation <> ": " <> err

-- |A variant of 'annotateError' which indents the rest of the error message by two spaces
annotateAndIndentError :: Monad m => Text -> ExceptT Text m a -> ExceptT Text m a
annotateAndIndentError annotation = flip catchE $ \err -> do
  let indentedErr = unlines . fmap ("  " <>) . lines $ err
  throwE $ annotation <> ":\n" <> indentedErr

-- |If an error propagates up this call, transform it using the given function
mapError :: Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapError = withExceptT

-- |If an error propagates up this call, replace it with the given error
replaceError :: Monad m => e' -> ExceptT e m a -> ExceptT e' m a
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
