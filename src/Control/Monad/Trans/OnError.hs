{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- |
-- = Error-handling utility functions
--
-- This is one of those libraries in which it's easier to look at the big picture than to try to make the pieces fit
-- together by looking at their types. So here's the big picture.
--
--
-- == Detecting, propagating, and handling errors
--
-- The life of an error consists of three phases:
--
-- 1. Some code produces a value which is considered erroneous.
-- 2. The error gets propagated up the call stack.
-- 3. Some code handles the error.
--
-- You need to write your own code for phase 1. Then, this library makes it easy to transition to phase 2, to transform
-- the error as it is propagating up, and to transition to phase 3. And then, you again need to write your own code for
-- phase 3.
--
-- 1. The code you write for phase 1 will typically return 'Nothing' or a 'Left' in case of failure. The library
--    provides a bunch of @on[Condition]ThrowError@ functions which detect those erroneous results and convert them into
--    a propagatable form, thereby transitioning between phase 1 and phase 2.
--
-- 2. In phase 2, write code in the mtl style, with a 'MonadError' constraint indicating the type of errors your code is
--    propagating up. The library provides a function 'mapError' for transforming the errors propagating up from
--    subcalls into the form your callers expect. 'annotateError' is a specialized version of 'mapError' which adds a
--    bit of context to an existing error message.
--
-- 3. To transition to phase 3, the library provides a bunch of @onError[Action]@ functions for stopping the propagation
--    and converting the successful or erroneous value into a form which is easy to manipulate, such as a 'Maybe' or an
--    'Either'.
--
-- In the following example, 'Data.Map.lookup' indicates failure by returning 'Nothing', we detect that case using
-- 'onNothingThrowError', and we replace it with a more informative error message. Since our example uses two 'Map's, we
-- annotate that error with some extra information indicating which 'Map' encountered the error.
--
-- > lookupM :: MonadError Text m
-- >         => Int -> Map Int a -> m a
-- > lookupM k = onNothingThrowError ("key " <> showt k <> " not found")
-- >           . Map.lookup k
-- >
-- > nestedLookupM :: MonadError Text m
-- >               => (Int, Int) -> Map Int (Map Int a) -> m a
-- > nestedLookupM (k1, k2) mm = do
-- >   m <- annotateError "outer map" $ lookupM k1 mm
-- >   a <- annotateError "inner map" $ lookupM k2 m
-- >   pure a
-- >
-- > nestedMap :: Map Int (Map Int Int)
-- > nestedMap = Map.fromList [(1, Map.fromList [(2, 42)])]
--
-- The above example is polymorphic over 'm', and so works with any Monad satisfying the 'MonadError' constraint.
--
-- >>> nestedLookupM (3,4) nestedMap :: Either Text Int
-- Left "outer map: key 3 not found"
-- >>> nestedLookupM (1,4) nestedMap :: MaybeT (ExceptT Text Identity) Int
-- MaybeT (ExceptT (Identity (Left "inner map: key 4 not found")))
--
-- A more important reason for writing code which is polymorphic in 'm' is that the functions in this library work by
-- temporarily specializing 'm' to a concrete Monad stack, and then generalizing it to an arbitrary 'm' again, so the
-- code needs to be polymorphic in order to compile. Whenever you see @ExceptT e m a@ in the type signature of a
-- function in this library, it might be helpful to mentally replace it with @(forall m. (MonadExcept e m, ...) => m
-- a)@. This technique is used so that functions like 'mapError' can change the error type even though 'm' will
-- eventually be instantiated to a concrete Monad stack with a fixed error type.
--
-- Another consequence of this design is that unlike the above call to 'nestedLookupM' which directly returns a 'Left'
-- when specialized to 'Either', a function like @onErrorLeft@ cannot directly return a 'Left', it must wrap it in some
-- underlying Monad. @onError[Action]@ calls take care of the 'MonadError' constraint, so the chosen Monad only needs to
-- satisfy the other constraints of the computation, if any. In this case, 'MonadError' was our only constraint, so we
-- can choose any Monad we want.
--
-- >>> onErrorDefault 0 (nestedLookupM (3,4) nestedMap) :: IO Int
-- 0
-- >>> onErrorLeft      (nestedLookupM (1,2) nestedMap) :: Identity (Either Text Int)
-- Identity (Right 42)
--
--
-- == Converting between different error representations
--
-- Another way to use this library is to make error-related glue code more readable. For example, if we are writing a
-- handler for some HTTP request, in a Monad which has a way to abort with an HTTP error status and a way to run
-- arbitrary IO, we might want to run some existing IO function 'fetchCount', failing with an HTTP 500 status if that
-- call fails. Since 'fetchCount' doesn't know how to report HTTP errors, we'll have to add a bit of glue code around
-- the call:
--
-- > fetchCount :: Text -> IO (Maybe Int)
-- > sendErrorStatus :: Int -> Response a
-- >
-- > countUsers :: Response Int
-- > countUsers = join
-- >            . fmap (maybe (sendErrorStatus 500) pure)
-- >            . liftIO
-- >            $ fetchCount "http://example.com/dogs/count"
--
-- By composing a few higher-order functions, we have succinctly converted the 'Nothing' response into an HTTP 500
-- error. While succinct, this solution isn't very readable, as it requires keeping track of quite a few type
-- transformations. With this library, we can write the following glue code instead:
--
-- > countUsers :: Response Int
-- > countUsers = onErrorCatch sendErrorStatus
-- >            . liftCheck (onNothingThrowError 500)
-- >            . liftIO
-- >            $ fetchCount "http://example.com/dogs/count"
--
-- This code also performs many type transformations, but they now follow the usual three phases, thereby giving the
-- code an extra bit of structure which hopefully helps the reader to follow along more easily. In phase 1, 'fetchCount'
-- may produce a value which is considered erroneous. Then, 'liftIO' is used to make this computation polymorphic in
-- 'm', because as explained earlier, this library expects polymorphic inputs. Next, 'onNothingThrowError' is used to
-- detect the erroneous value and enter phase 2. There are no phase 2 transformations in this example. Finally,
-- 'onErrorCatch' is used to convert the error back into the format expected by the 'Response' Monad, in this case by
-- aborting with an HTTP error.
--
-- In order to support this style, all the functions in this library are designed to compose with each other using
-- function composition. The only exception so far is 'liftCheck', is a higher-order function which lifts an
-- @on[Condition]ThrowError@ function expecting a plain value to one which expects an action returning that value. This
-- way, we don't need to first bind the result of 'fetchCount' to a temporary variable before examining it using
-- 'onNothingThrowError'.
--
-- == Errors vs bugs
--
-- While the role of a library such as this one is to provide a mechanism for handling errors, not an error-handling
-- policy, its author would like to spend a few words encouraging users to distinguish errors from bugs.
--
-- If a function documents a precondition and it is given an argument which fails this precondition, this is a bug, and
-- the fix is to change the code so that this situation doesn't happen. There is no way for the calling code to handle
-- this situation in a sensible way, so please don't put the onus on them by propagating an error upwards. Instead,
-- simply use 'error' to throw a runtime exception. Since asynchronous exceptions can occur at anytime anyway,
-- well-written callers will properly release resources using 'bracket', and some top-level exception handler will log
-- the exception message and tell the user that some internal error has occurred.
--
-- For example, a compiler accepts user-written code which might not be well-formed. The first few phases of the
-- compiler make sure that the code has recognizable syntax and that variables are well-scoped. If it isn't, then that's
-- an error, which should be propagated upwards so that the user can be told how to fix their code. But once the code
-- passes those sanity checks, if the compiler incorrectly performs an optimization in a way which results in
-- transformed code which isn't well-scoped, telling the user that their code is not well-scoped is incorrect and
-- misleading. It is better to fail with an internal error in that case, so that programmers can be notified and can fix
-- this incorrect optimization.
--
-- To do so, it is important to distinguish between:
--
-- 1. input code which is expected to be possibly-ill-scoped because it hasn't yet been scope-checked, and
-- 2. input code which is expected to be well-scoped because it has been scope-checked and all the optimizations it has
--    since been through claim to preserve this invariant.
--
-- So please, document your invariants and pre-conditions, and act accordingly! Using different types for those two
-- different cases, even if the underlying representation is the same, is a simple and lightweight way write this
-- documentation.
module Control.Monad.Trans.OnError where

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
